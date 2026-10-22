# effectful · короткое руководство для heosd

Что нужно знать, чтобы написать часть 4 плана. Не справочник по библиотеке, а
ровно те пять вещей, которые встретятся в `Heos.Effect`, `Handler.Live` и
`Handler.Fake`.

## Зачем

Сценарий «найти и поставить» это цепочка запросов к аппарату. Отлаживать её
против живого усилителя плохо: медленно, шумно, ночью нельзя. Хочется, чтобы
сценарий говорил «дай мне список плееров», а кто ответит, настоящий аппарат
или заготовка из файла, решалось снаружи.

effectful даёт это в типе функции. Сценарий объявляет, какие эффекты ему
нужны, и не знает, кто их выполняет. Это тот же канал `R` из Effect-TS:
`Effect<A, E, R>` там, `Eff es a` здесь. (Порядок `A, E, R` с версии 2.0;
до неё было `Effect<R, E, A>`, как `ZIO[R, E, A]`.)

## 1 · `Eff es a`: действие с перечнем эффектов

`IO a` это «действие, которое может делать что угодно и вернёт `a`».
`Eff es a` это «действие, которое может делать только то, что перечислено
в `es`, и вернёт `a`». `es` это список типов, набор эффектов.

```haskell
Eff '[Heos, IOE] NowPlaying   -- может ходить в Heos и в IO, вернёт NowPlaying
Eff '[Heos] NowPlaying        -- только Heos, IO запрещён
```

Апостроф перед скобкой это список на уровне типов, включается `DataKinds`
(в GHC2024 есть). В сигнатурах его почти не пишут, см. следующий пункт.

## 2 · `:>`: «в наборе есть»

Функции не перечисляют весь набор, а требуют, что в нём есть нужное:

```haskell
whatIsPlaying :: Heos :> es => PlayerId -> Eff es Text
```

Читается: «для любого набора `es`, в котором есть `Heos`, это действие над
`es`». `Heos :> es` стоит слева от `=>`, это ограничение, как `Show a`.
Несколько эффектов через запятую: `(Heos :> es, Log :> es) =>`.

Смысл: функция работает в любом стеке, где есть `Heos`, и не привязана к
конкретному порядку или составу. Это и есть то, что позволяет подменять
обработчик.

Самая частая ошибка компиляции: `Could not deduce (Heos :> es)`. Значит,
внутри функции вызвана операция `Heos`, а в её сигнатуре ограничения нет.
Добавь `Heos :> es =>`.

## 3 · Эффект это GADT

Эффект объявляется как тип данных, где каждый конструктор это одна операция:

```haskell
data Heos :: Effect where
  GetPlayers    :: Heos m [Player]
  GetNowPlaying :: PlayerId -> Heos m NowPlaying
  SetPlayState  :: PlayerId -> PlayState -> Heos m ()

type instance DispatchOf Heos = Dynamic
```

Что здесь что:

- `data Heos :: Effect where` вместо `data Heos = ...`. Это синтаксис GADT,
  см. ниже. `Effect` это вид (kind) типа, он говорит, что `Heos` принимает
  два параметра, `m` и результат.
- `GetNowPlaying :: PlayerId -> Heos m NowPlaying`: конструктор берёт
  `PlayerId`, последний параметр, `NowPlaying`, это тип результата операции.
  У `SetPlayState` результат `()`, у `GetPlayers` список.
- `m` не используется ни в одной операции. Он нужен операциям высшего
  порядка, которые принимают другие действия, например `Listen` у `Writer`.
  У нас таких нет, но параметр обязателен по форме.
- `type instance DispatchOf Heos = Dynamic` пишется всегда, одной строкой.
  Говорит библиотеке, что обработчик выбирается при запуске, а не при
  компиляции. Без этого `Live` и `Fake` не подменить.

Имена операций и имена результатов не должны совпадать: `NowPlaying` как
операция и `NowPlaying` как тип дадут ошибку «data constructor cannot be
used here». Операции с глаголом или `Get`, результаты существительными.

### GADT в двух словах

Обычное `data` перечисляет конструкторы:

```haskell
data PlayState = Play | Pause | Stop
```

Все три имеют один и тот же тип, `PlayState`. GADT (generalised algebraic
data type) записывает каждому конструктору полную сигнатуру, как функции:

```haskell
data Heos :: Effect where
  GetPlayers    :: Heos m [Player]
  GetNowPlaying :: PlayerId -> Heos m NowPlaying
```

и результат у конструкторов разный: `Heos m [Player]` у одного,
`Heos m NowPlaying` у другого. Компилятор это помнит. Когда обработчик
делает `case` и попадает в ветку `GetNowPlaying pid`, он знает, что вернуть
надо именно `NowPlaying`, и ничего другого не примет. Это то, чего обычное
`data` не умеет, и единственная причина, по которой эффекты пишут через GADT.
Расширение `GADTs` в GHC2024 включено.

## 4 · `send` и обёртки

Конструктор это описание операции, не действие. `GetNowPlaying pid` построен,
но ничего не произошло, как `VolumeUp pid 1` до `sendCommand`. Выполняет
операцию `send`:

```haskell
send :: (e :> es) => e m a -> Eff es a
```

Он находит в наборе `es` текущий обработчик для `e` и отдаёт ему операцию.
Чтобы не писать `send (GetNowPlaying pid)` в каждом месте, на каждую
операцию заводят функцию с маленькой буквы:

```haskell
getPlayers :: Heos :> es => Eff es [Player]
getPlayers = send GetPlayers

getNowPlaying :: Heos :> es => PlayerId -> Eff es NowPlaying
getNowPlaying pid = send (GetNowPlaying pid)

setPlayState :: Heos :> es => PlayerId -> PlayState -> Eff es ()
setPlayState pid st = send (SetPlayState pid st)
```

Наружу из `Heos.Effect` уходят только они. Конструкторы нужны обработчикам.

## 5 · Обработчик: снять эффект со стека

Обработчик берёт действие, в стеке которого есть `Heos`, и возвращает
действие без него. В типе это видно буквально:

```haskell
runHeosLive :: IOE :> es => Connection -> Eff (Heos : es) a -> Eff es a
runHeosLive conn = interpret_ $ \case
  GetPlayers -> liftIO $ do
    env <- sendCommand conn GetPlayers >>= either throwIO pure
    decodePlayers env.payload
  GetNowPlaying pid -> liftIO $ ...
  SetPlayState pid st -> liftIO $ ...
```

- `Eff (Heos : es) a -> Eff es a`: на входе стек с `Heos` на вершине, на
  выходе без него. `Heos` обработан, остальное осталось.
- `interpret_` берёт функцию «операция, результат» и делает из неё
  обработчик. Подчёркивание это версия для операций первого порядка, то есть
  для наших, без `m` внутри. `\case` по конструкторам; забытый конструктор
  это предупреждение компилятора.
- `IOE :> es`: обработчику нужен `IO`, потому что внутри `sendCommand`.
  `IOE` это эффект «можно делать IO», а `liftIO` поднимает `IO a` в `Eff es a`.
- Исключение, брошенное внутри `liftIO`, летит сквозь `Eff` как обычное.

`Fake` это другой обработчик того же типа, без `IOE`:

```haskell
runHeosFake :: Fixtures -> Eff (Heos : es) a -> Eff es a
runHeosFake fx = interpret_ $ \case
  GetPlayers -> pure fx.players
  GetNowPlaying _ -> pure fx.nowPlaying
  SetPlayState _ _ -> pure ()
```

Сценарий одинаковый, обработчики разные, в сеть `Fake` не ходит.

## 6 · Запуск

Стек снимают слой за слоем, снаружи внутрь, пока не останется `IOE`, который
снимает `runEff`:

```haskell
main = withReconnect $ \conn ->
  runEff $ runHeosLive conn $ scenario
```

`runEff :: Eff '[IOE] a -> IO a`. Если в стеке нет `IOE` вовсе, как у `Fake`
в тестах, вместо него `runPureEff :: Eff '[] a -> a`, и результат чистый:

```haskell
runPureEff (runHeosFake fx scenario) `shouldBe` "Spektar"
```

Порядок обработчиков важен только когда эффекты зависят друг от друга;
с одним `Heos` думать не о чем.

## Шпаргалка Effect-TS → effectful

| Effect-TS | effectful |
| -- | -- |
| `Effect<A, E, R>` | `Eff es a`; ошибки отдельным эффектом `Error e` или исключениями |
| `R` содержит `Heos` | `Heos :> es` |
| `Context.Tag` + интерфейс сервиса | `data Heos :: Effect where ...` |
| `yield* heos.getPlayers` | `getPlayers` (обёртка над `send`) |
| `Layer` / `Effect.provideService` | `runHeosLive conn`, `runHeosFake fx` |
| `Effect.runPromise` | `runEff` |
| тестовый `Layer` | второй `interpret_` |

## Что импортировать

```haskell
import Effectful (Eff, Effect, Dispatch (Dynamic), DispatchOf, IOE, (:>), liftIO, runEff, runPureEff)
import Effectful.Dispatch.Dynamic (interpret_, send)
```

Версия в проекте: effectful 2.7. Если что-то не находится, сверяйся с
документацией этой версии, имена между 2.x менялись мало, но `interpret_`
появился не сразу.
