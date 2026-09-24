-- | Учебный разбор библиотеки effectful.
module ComplexDemo (
    -- * Свой эффект
    Logger (..),
    logMsg,
    runLoggerIO,
    runLoggerPure,

    -- * Бизнес-логика
    countdown,
    demoIO,
    demoPure,
) where

import Effectful (
    Dispatch (Dynamic),
    DispatchOf,
    Eff,
    Effect,
    IOE,
    MonadIO (liftIO),
    runEff,
    runPureEff,
    type (:>),
 )
import Effectful.Dispatch.Dynamic (interpret_, reinterpret_, send)
import Effectful.State.Static.Local (State, get, put, runState)
import Effectful.Writer.Static.Local (runWriter, tell)

-- 1. Объявляем эффект как GADT: список операций, которые он умеет.
data Logger :: Effect where
    Log :: String -> Logger m ()

-- 2. Говорим, что обработчик выбирается в рантайме (динамическая диспетчеризация).
type instance DispatchOf Logger = Dynamic

-- 3. Удобная обёртка: @send@ кладёт операцию в очередь, не решая, кто её выполнит.
--    Ограничение @Logger :> es@ читается как «в наборе эффектов es есть Logger».
logMsg :: (Logger :> es) => String -> Eff es ()
logMsg msg = send (Log msg)

-- 4a. Первый обработчик: печатает в stdout. Требует IOE в остатке стека.
runLoggerIO :: (IOE :> es) => Eff (Logger : es) a -> Eff es a
runLoggerIO = interpret_ $ \case
    Log msg -> liftIO (putStrLn msg)

-- 4b. Второй обработчик той же операции: копит строки, без всякого IO.
runLoggerPure :: Eff (Logger : es) a -> Eff es (a, [String])
runLoggerPure = reinterpret_ runWriter $ \case
    Log msg -> tell [msg]

-- 5. Логика написана против ограничений, а не против конкретного стека монад.
countdown :: (Logger :> es, State Int :> es) => Eff es Int
countdown = do
    -- @get@ сам по себе не знает, какое состояние доставать, поэтому тип указан явно.
    n <- get @Int
    if n <= 0
        then do
            logMsg "поехали!"
            pure 0
        else do
            logMsg (show n <> "...")
            put (n - 1)
            (+ 1) <$> countdown

-- 6. Сборка стека: каждый run* снимает один эффект со списка.
demoIO :: IO (Int, Int)
demoIO = runEff . runLoggerIO . runState (3 :: Int) $ countdown

-- То же самое вычисление без IO вообще — меняется только набор обработчиков.
demoPure :: ((Int, Int), [String])
demoPure = runPureEff . runLoggerPure . runState (3 :: Int) $ countdown
