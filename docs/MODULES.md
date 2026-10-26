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
| `Heos.Types.Records` | записи, которые приходят от аппарата или нужны сценарию | `Player`, `NowPlaying`, `Track`, `MusicSource`, `SearchResult`, `SearchCriterion`, `Found` |
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
| `Heos.Envelope` | одна строка JSON от аппарата | `Envelope`, `HeosHeader`, `HeosResult`, `HeosError`, `HeosOutcome`, `HeosEvent`, `IncomingEnvelope` |
| `Heos.Command` | все команды спецификации, одна на конструктор | `Command`, `Range`, `ServiceOption`, `QuickSelectId`, `InputName` |

`Command` отправляет: его типам нужен `ToParam`. `Envelope` и `Types`
читают: их типам нужен `FromJSON`. Тип, который и отправляется, и
читается (`PlayState`, `MediaId`), имеет оба экземпляра.

## Сеть: как до аппарата добраться

| модуль | что в нём | типы |
| -- | -- | -- |
| `Heos.Discovery` | SSDP-поиск | `DiscoveryError` |
| `Heos.Connection.Parameters` | настройки соединения | `ConnectionParams`, `defaultParams` |
| `Heos.Connection.Framing` | байты в строки | `Pending` |
| `Heos.Connection.Internal` | сокеты, reader, heartbeat, sendCommand | `Connection`, `ConnectionSockets`, `ConnectionProps`, `ConnectionError` |
| `Heos.Connection.Reconnect` | повтор поверх соединения | нет своих |
| `Heos.Connection` | фасад: `withReconnect`, `sendCommand`, `subscribe` | `Connection` абстрактный |

## Домен: что нужно сценариям

| модуль | что в нём | типы |
| -- | -- | -- |
| `Heos.Effect` | операции эффекта и их обёртки | `Heos`, `ProtocolError` |
| `Heos.Handler.Live` | эффект поверх `Connection` | нет своих |
| `Heos.Handler.Fake` | эффект поверх заготовок | `FakeState` |
| `Heos.Handler.Fixtures` | заготовки для подделки: очередь из записанного ответа | своих нет |
| `Heos.Service` | сценарии: найти и поставить, статус | появятся |

`SearchResult` это как аппарат отвечает на поиск, `Found` это что из этого
нужно сценарию. Перевод одного в другое делает `Live` при поиске, потому что
только он знает `sid` и строку запроса.

## Фасад: что видит пользователь библиотеки

| модуль | что в нём | типы |
| -- | -- | -- |
| `Heos` | реэкспорт `Time`, `Types`, `ToParam`, `Envelope`, `Command`, `Connection`, `Effect`, `Handler.Live` | своих нет |

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
