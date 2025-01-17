{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Exception
import Data.Aeson (FromJSON, ToJSON, decode, encode)
import Data.ByteString (pack)
import qualified Data.ByteString.Lazy as BS
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)
import System.Directory
import System.Exit (exitSuccess)
import System.IO

data Todo = Todo
  { name :: String,
    checked :: Bool
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

todosFileName :: String
todosFileName = ".todos"

main :: IO ()
main = do
  putStrLn "Made with <3 in Haskell by Superredstone\n"
  todos <- readTodos
  todoProgram todos

todoProgram :: [Todo] -> IO ()
todoProgram todos = do
  line <- printAndGet "> "

  case strip line of
    "new" -> newTodo todos
    "show" -> showTodos todos
    "delete" -> deleteTodo todos
    "save" -> saveTodos todos
    "toggle" -> toggleTodo todos
    "help" -> printHelp todos
    "version" -> do
      putStrLn "v1.1\n"
      todoProgram todos
    "exit" -> putStrLn "Exiting the program...\n"
    _ -> do
      putStrLn "Invalid operation.\n"
      todoProgram todos

printHelp :: [Todo] -> IO ()
printHelp todos = do
  putStrLn
    "new\t\tCreate a new todo\n\
    \delete\t\tDelete an existing todo\n\
    \show\t\tDisplay all todos\n\
    \help\t\tPrint this message\n\
    \version\t\tPrint program version\n\
    \exit\t\tClose the program\n"
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

toggleTodo :: [Todo] -> IO ()
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

replace :: Int -> a -> [a] -> [a]
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

saveTodos :: [Todo] -> IO ()
saveTodos todos = do
  putStrLn "Saving..."
  BS.writeFile todosFileName (encode todos)
  putStrLn "Saved.\n"
  todoProgram todos

readTodos :: IO [Todo]
readTodos = do
  fileExist <- doesFileExist ".todos"
  if fileExist
    then do
      file <- BS.readFile todosFileName
      let todos = decode file :: Maybe [Todo]
      return (fromMaybe [] todos)
    else do
      return []

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
