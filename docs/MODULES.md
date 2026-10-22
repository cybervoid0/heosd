# Карта модулей

Где какой тип лежит и почему именно там. Правило одно: модуль импортирует
только тех, кто ниже него в этой таблице. Листовые модули наверху не знают
ни о ком, кроме `ToParam`; чем ниже, тем больше модуль знает.

## Листовые: маленькие типы, ни от кого не зависят

| модуль | что в нём | типы |
| -- | -- | -- |
| `Heos.Time` | время | `Seconds` |
| `Heos.ToParam` | «как значение пишется в URL» | класс `ToParam`, `.=`, `.=?` |
| `Heos.Player` | плеер и то, что он играет | `PlayerId`, `GroupId`, `QueueId`, `PlayState`, `OnOff`, `Network`, `Player`, `NowPlaying` |
| `Heos.Media` | музыка и источники | `SourceId`, `ContainerId`, `MediaId`, `SearchCriteriaId`, `SearchKind`, `SearchResult`, `MusicSource`, `YesNo`, `TrueFalse` |

Принцип разделения `Player` и `Media`: `pid`, `gid`, `qid` про то, *на чём*
играет, `sid`, `cid`, `mid`, `scid` про то, *что* играет.

`heosJson` из `Config` просится в отдельный `Heos.Json`: `Config` будет
импортировать `Player` и `Media`, а они его, и это цикл.

## Протокол: как аппарат говорит

| модуль | что в нём | типы |
| -- | -- | -- |
| `Heos.Url` | строка `key=value&…` в обе стороны | `ParamKey`, `UrlPair`, `UrlParam`, `KeyFailure`, `UrlResponse` |
| `Heos.Envelope` | одна строка JSON от аппарата | `Envelope`, `HeosHeader`, `HeosResult`, `HeosError`, `HeosOutcome`, `HeosEvent`, `IncomingEnvelope` |
| `Heos.Command` | все команды спецификации, одна на конструктор | `Command`, `Credentials`, `Range`, `QueueMode`, `RepeatMode`, `ServiceOption`, `QuickSelectId`, `InputName` |

`Command` отправляет: его типам нужен `ToParam`. `Envelope`, `Player`,
`Media` читают: их типам нужен `FromJSON`. Тип, который и отправляется, и
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
| `Heos.Effect` | операции эффекта и их обёртки | `Heos`, `Playable`, `Found` |
| `Heos.Handler.Live` | эффект поверх `Connection` | нет своих |
| `Heos.Handler.Fake` | эффект поверх заготовок | `Fixtures` (появится) |
| `Heos.Service` | сценарии: найти и поставить, статус | появятся |

`SearchResult` это как аппарат отвечает на поиск, `Found` это что из этого
нужно сценарию. Перевод одного в другое делает `Live` при поиске, потому что
только он знает `sid` и строку запроса.

## Как искать, не открывая файлы

- `Cmd+T` в VS Code, набрать имя типа: поиск по символам всего проекта.
- `F12` или `Cmd+клик` на имени: перейти к определению.
- `Shift+F12`: все места, где тип используется.
- В терминале: `grep -rn "^data Found\|^newtype Found" src`.
