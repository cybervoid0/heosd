module Demo where

import Control.Monad.State qualified as St
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
import Effectful.Dispatch.Dynamic (interpret_, send)
import Effectful.State.Static.Local (State, get, put, runState)

-- IOE - эффект IO - встроенный разрешенный эффект
-- IOE :> es в сигнатуре читается как «в наборе es есть разрешение на ввод-вывод».
hello :: (IOE :> es) => Eff es ()
hello = do
  -- liftIO поднимает IO действие в ... куда?
  liftIO $ putStrLn "Привет из Eff"
  liftIO $ putStrLn "это обычная монада (обычный моноид в категории эндофункторов)"

-- State Int означает что тут эффект типа число
counter :: (State Int :> es) => Eff es String
counter = do
  n <- get @Int
  put $ n + 1
  pure $ "was " <> show n

runHello :: IO ()
-- runEff требует, чтобы к моменту запуска в списке эффектов остался ровно IOE
runHello = runEff hello

runCounter :: (String, Int)
-- Каждая функция run* снимает ровно один эффект
-- никакого IO здесь нет вообще, поэтому pure
runCounter = runPureEff $ runState 10 counter

demoState :: Int -> St.State Int String
demoState n = do
  St.put n
  St.modify (+ 1)
  z <- St.get
  pure $ "was: " <> show n <> " new: " <> show z

----------------------------

-- список того что эффект умеет
data Console :: Effect where
  WriteLine :: String -> Console m ()

-- ReadLine :: Console m String

-- обработчик подставляется при сборке программы
type instance DispatchOf Console = Dynamic

-- обёртка
writeLine :: (Console :> es) => String -> Eff es ()
writeLine s = send $ WriteLine s

greet :: (Console :> es) => Eff es ()
greet = do
  writeLine "здравствуйте"
  writeLine "это ваш первый эффект"

runConsoleIO :: (IOE :> es) => Eff (Console : es) a -> Eff es a
runConsoleIO = interpret_ $ \case
  WriteLine s -> liftIO $ putStrLn s

runGreetIO :: IO ()
runGreetIO = runEff (runConsoleIO greet)