import Control.Exception
import Data.Foldable (find)
import GHC.Base (when)
import System.Exit (exitSuccess)
import System.IO
import Text.Read (Lexeme (String))
import Text.Read.Lex (Number)

type TodoName = String

data Todo where
  Todo ::
    { name :: String,
      checked :: Bool
    } ->
    Todo
  deriving (Show, Eq)

main :: IO ()
main = do
  putStrLn "Made with <3 in Haskell by Superredstone\n"
  todoProgram []

todoProgram :: [Todo] -> IO ()
todoProgram todos = do
  line <- printAndGet "> "

  case strip line of
    "new" -> do
      newTodo todos
    "show" -> do
      showTodos todos
    "delete" -> do
      deleteTodo todos
    "toggle" -> do
      toggleTodo todos
    "help" -> do
      printHelp todos
    "version" -> do
      putStrLn "v1.0\n"
      todoProgram todos
    "exit" -> do
      putStrLn "\nExiting the program..."
    _ -> do
      putStrLn "Invalid operation."
      todoProgram todos

printHelp :: [Todo] -> IO ()
printHelp todos = do
  putStrLn
    "new\tCreate a new todo\n\
    \delete\tDelete an existing todo\n\
    \show\tDisplay all todos\n\
    \help\tPrint this message\n\
    \version\tPrint program version\n\
    \exit\tClose the program\n"
  todoProgram todos

newTodo :: [Todo] -> IO ()
newTodo todos =
  do
    line <- printAndGet "Name: "
    putStrLn ""
    if todoExists line todos
      then do
        putStrLn "This todo name already exists\n"
        todoProgram todos
      else do
        let newTodos = todos ++ [Todo line False]
        todoProgram newTodos

toggleTodo todos = do
  input <- printAndGet "Todo name: "
  putStrLn ""
  if todoExists input todos
    then do
      let index = indexOfTodo input todos
      let new = replace index (Todo (name (todos !! index)) (not (checked (todos !! index)))) todos
      todoProgram new
    else do
      putStrLn "This todo does not exist.\n"
      todoProgram todos

replace pos newVal list = take pos list ++ newVal : drop (pos + 1) list

deleteTodo :: [Todo] -> IO ()
deleteTodo todos = do
  input <- printAndGet "Todo name: "
  if todoExists input todos
    then do
      let index = indexOfTodo input todos
      todoProgram (drop index todos)
    else do
      putStrLn "This todo does not exist."
      todoProgram todos

showTodos :: [Todo] -> IO ()
showTodos todos = do
  printTodos todos
  putStrLn ""
  todoProgram todos

printTodos :: [Todo] -> IO ()
printTodos [] = return ()
printTodos todos = do
  let currentTodo = head todos
  let completed = if checked currentTodo then "[X] " else "[ ] "
  putStrLn (completed ++ name currentTodo)
  printTodos (tail todos)

printAndGet :: String -> IO String
printAndGet str = do
  putStr str
  hFlush stdout
  catch getLine handleEof

handleEof :: SomeException -> IO String
handleEof _ = do
  putStrLn "\nThanks for using my program.\n"
  exitSuccess

todoExists :: String -> [Todo] -> Bool
todoExists str = foldr (\t -> (||) (str == name t)) False

indexOfTodo :: String -> [Todo] -> Int
indexOfTodo elt list = length $ takeWhile (/= elt) (extractName list)

extractName :: [Todo] -> [String]
extractName = map name

strip :: String -> String
strip = filter (/= ' ')
