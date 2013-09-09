import Quiz
import Config



-- | A convenient but hacky way to run our game. Currently the Right case is
-- useless because game termination only happens by throwing an error when
-- someone types "quit" at the prompt. A possible TODO is having a user
-- optionally supply a number of times to be tested so that quitting early
-- would be an actual exception condition.
runGame :: Registry -> IO ()
runGame registry = do
  let state = newQuizState
  res <- runQuiz registry state playGame
  case res of
    -- temporary hack. could check e with System.IO.Error.isEOFError and print
    -- nicer error message
    (Left  e, q) -> putStrLn $ concat ["Caught exc: "
                                      , e
                                      , "\nFinal score: "
                                      , show q
                                      ]
    (Right _, q) -> putStrLn $ "Final score: " ++ show q

-- | Parses command line args, attempts to build a QuizState, and either runs
-- the game or displays the error.
main = do
  cfg <- cmdArgs config
  env <- fromConfig cfg
  case env of
    Left  e -> putStrLn e
    Right r -> runGame  r




