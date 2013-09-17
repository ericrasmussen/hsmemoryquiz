import Quiz (runGame)
import Config (cmdArgs, config, fromConfig)


-- | Parses command line args, attempts to build a Registry, and either runs
-- the game or displays the error.
main = do
  cfg <- cmdArgs config
  env <- fromConfig cfg
  case env of
    Left  e -> putStrLn e
    Right r -> runGame  r


