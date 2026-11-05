# Карта модулей

Где какой тип лежит и почему именно там. Правило одно: модуль импортирует
только тех, кто ниже него в этой таблице. Листовые модули наверху не знают
ни о ком, кроме `ToParam`; чем ниже, тем больше модуль знает.

## Листовые: маленькие типы, ни от кого не зависят

| модуль | что в нём | типы |
| -- | -- | -- |
| `Heos.Time` | время | `Seconds` |
| `Heos.ToParam` | «как значение пишется в URL» | класс `ToParam`, `.=`, `.=?` |
| `Heos.Json` | общие правила разбора JSON | `heosJson` |
| `Heos.Types.Newtypes` | идентификаторы и обёртки над строками протокола | `PlayerId`, `GroupId`, `QueueId`, `SourceId`, `ContainerId`, `MediaId`, `SearchCriteriaId`, `YesNo`, `TrueFalse` |
| `Heos.Types.Enums` | перечисления | `PlayState`, `OnOff`, `Network`, `SearchKind`, `QueueMode`, `RepeatMode`, `Playable`, `Credentials` |
| `Heos.Types.Records` | записи, которые приходят от аппарата или нужны сценарию; настройки соединений и кэш | `Player`, `Track`, `MusicSource`, `SearchResult`, `SearchCriterion`, `Found`, `HeosConnectionParams`, `AvrConnectionParams`, `Catalogue`, `LiveState` |
| `Heos.Types.Errors` | ошибки, которые видит сценарий | `ProtocolError` |
| `Heos.Types` | фасад над тремя файлами выше | своих нет |

Деление по форме, а не по смыслу: идентификатор, перечисление, запись.
Вопрос «это про плеер или про музыку» не нужен, достаточно «что это за
штука». Порядок внутри: `Newtypes` ни от кого, `Enums` от `Newtypes`,
`Records` от обоих.

Пишется везде `import Heos.Types` без списка, и в библиотеке, и в тестах.
`Heos.Types` импортирует только `ToParam` и `Json`, поэтому его можно
брать откуда угодно, цикла не будет. Если тип переехал между тремя файлами,
импорты менять не надо.

Рядом с типами лежат три чистые функции, которые знают только о них:
`parsePlayState`, `toFound`, `criterionFor`.

## Протокол: как аппарат говорит

| модуль | что в нём | типы |
| -- | -- | -- |
| `Heos.Url` | строка `key=value&…` в обе стороны | `ParamKey`, `UrlPair`, `UrlParam`, `KeyFailure`, `UrlResponse` |
| `Heos.Envelope.Heos` | одна строка JSON от аппарата; `decodePayload`, `fromMessage` вынимают из неё значение | `Envelope`, `HeosHeader`, `HeosResult`, `HeosError`, `HeosOutcome`, `HeosEvent`, `IncomingEnvelope` |
| `Heos.Command.Heos` | все команды спецификации, одна на конструктор | `Command`, `Range`, `ServiceOption`, `QuickSelectId`, `InputName` |
| `Heos.Command.Avr` | команды порта 23: `PW?`, `PWON`, `PWSTANDBY` | `AvrCommand a` |
| `Heos.Envelope.Avr` | одна строка с порта 23 | `AvrResponse` |

`Command` отправляет: его типам нужен `ToParam`. `Envelope` и `Types`
читают: их типам нужен `FromJSON`. Тип, который и отправляется, и
читается (`PlayState`, `MediaId`), имеет оба экземпляра.

Порт 23 говорит иначе: ASCII-строки до `\r`, без JSON и без `SEQUENCE`,
а события (`MV45`) идут в том же потоке, что и ответы. Поэтому `AvrCommand a`
несёт тип своего ответа: `GetPower :: AvrCommand OnOff`, и `answerOf` в
`Connection.Avr` говорит, какая строка отвечает на какую команду. Разбор
ответа один, второго `case` в хендлере нет.

## Сеть: как до аппарата добраться

| модуль | что в нём | типы |
| -- | -- | -- |
| `Heos.Discovery` | SSDP-поиск | `DiscoveryError` |
| `Heos.Connection.Framing` | байты в строки | `Pending` |
| `Heos.Connection.Types` | записи соединения и его ошибки | `HeosConnection`, `ConnectionSockets`, `ConnectionProps`, `ConnectionError` |
| `Heos.Connection.Heos` | сокеты, reader, heartbeat, `sendOn`, `readPid` | нет своих |
| `Heos.Connection.Reconnect` | повтор поверх соединения | нет своих |
| `Heos.Connection.Avr` | порт 23: `withAvr`, `sendAvrCommand`, `answerOf` | `AvrConnection`, `AvrConnectionError` |
| `Heos.Connection` | фасад: `withReconnect`, `withAvr`, `sendCommand`, `subscribe` | `HeosConnection` абстрактный |

Порт 23 открывается внутри `withReconnect`, по адресу из `Connection`.
Обрыв любого из двух соединений это `IOException`, и `withReconnect`
поднимает оба заново, вместе с поиском адреса. Параметры едут аргументом:
`withReconnect params` → `reconnecting` → `withConnection`, дальше они уже
внутри `Connection`; у `withAvr` так же, дальше внутри `AvrConnection`.

Плеер не параметр: `withConnection` после запуска reader спрашивает
`get_players` и берёт первый `pid` в `HeosConnection.pid`. Пустой список
это `PlayerNotFound`, одна из `ConnectionError`, и `withReconnect` пробует
снова. `runConnection` отдаёт только `ConnectionProps`: ячейки и потоки, без
разговора с аппаратом, чтобы тесты собирали `HeosConnection` сами.

## Устройство: оба соединения вместе

| модуль | что в нём | типы |
| -- | -- | -- |
| `Heos.Device.Parameters` | настройки устройства целиком | `DeviceParams`, `defaultParams` |
| `Heos.Device` | `withDevice`, `initialize`: два соединения и кэш в одной записи | `Device` |

Входная дверь библиотеки. Пользователь не должен знать, что соединений два и
в каком порядке складывать хендлеры: `withDevice params $ \dev -> runDevice dev
сценарий`, и всё. Что внутри `Device`, снаружи не видно.

Кэш поделён по тому, кто пишет. `Catalogue` (плееры, источники, критерии
по каждому источнику) заполняется один раз в `initialize` и дальше не
меняется, поэтому лежит в `Device` обычным значением. `LiveState` (что
играет, громкость, состояние) правит поток событий, поэтому он в одной
`TVar`: снимок «что играет и как громко» нужен целым, а писатель один.
`initialize` идёт в порядке из документации: события выключить, запросы,
события включить; запросы по плееру берут `pid` из соединения.

## Домен: что нужно сценариям

| модуль | что в нём | типы |
| -- | -- | -- |
| `Heos.Effect.Heos` | операции эффекта `Heos` и их обёртки | `Heos` |
| `Heos.Effect.Avr` | эффект питания: `getPower`, `setPower` | `Avr` |
| `Heos.Handler.Live` | `Heos` поверх `Connection` | нет своих |
| `Heos.Handler.AvrLive` | `Avr` поверх `AvrConnection` | нет своих |
| `Heos.Handler.AvrFake` | `Avr` без сети: одна ячейка с питанием | `FakeAvrState` |
| `Heos.Handler.Fake` | эффект поверх заготовок | `FakeState` |
| `Heos.Handler.Fixtures` | заготовки для подделки: очередь из записанного ответа | своих нет |
| `Heos.Service` | сценарии: найти и поставить, статус | появятся |

Два эффекта, а не один, потому что это два разных аппарата с точки зрения
протокола: у колонки HEOS порта 23 нет, а HEOS не умеет включить усилитель
молча. Сценарий, которому нужно и то и другое, пишет `(Heos :> es, Avr :> es)`
и запускается как `runEff . runHeosLive dev . runAvrLive dev.avr`.

`SearchResult` это как аппарат отвечает на поиск, `Found` это что из этого
нужно сценарию. Перевод одного в другое делает `Live` при поиске, потому что
только он знает `sid` и строку запроса.

## Фасад: что видит пользователь библиотеки

| модуль | что в нём | типы |
| -- | -- | -- |
| `Heos` | реэкспорт `Time`, `Types`, `ToParam`, `Envelope.*`, `Command.*`, `Connection`, `Device.Parameters`, `Effect.*`, `Handler.Live`, `Handler.AvrLive`, `Device` | своих нет |

Папка там, где файлов по смыслу несколько (`Command`, `Envelope`, `Effect`:
по одному на протокол), а общий модуль над папкой только там, где он что-то
скрывает. `Heos.Connection` остался, потому что его список экспорта короче,
чем у модулей под ним: `HeosConnection` без конструктора, без сокетов. Над
`Command` и `Effect` общего модуля нет: он бы только склеивал, это делает
корневой `Heos`.

`Heos` импортирует всё, поэтому сам он никому внутри библиотеки не нужен и
никем не импортируется: иначе цикл. Им пользуются только потребители:
`FirstTalk`, будущий сервер MCP, при желании тесты. Пишется `import Heos`
без списка, в этом и смысл. Если имя из `Heos` столкнётся с другим импортом,
компилятор скажет `Ambiguous occurrence`; лечится `hiding` или `qualified`.

`FirstTalk` это стенд, первый пользователь библиотеки, а не её часть. Сейчас
лежит в `src/FirstTalk.hs` и экспортируется пакетом; место ему в `app/`
рядом с `Main`, туда же потом пойдёт сервер MCP.

## Как искать, не открывая файлы

- `Cmd+T` в VS Code, набрать имя типа: поиск по символам всего проекта.
- `F12` или `Cmd+клик` на имени: перейти к определению.
- `Shift+F12`: все места, где тип используется.
- В терминале: `grep -rn "^data Found\|^newtype Found" src`.
