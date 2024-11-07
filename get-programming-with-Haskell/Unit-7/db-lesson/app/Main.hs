module Main (main) where

import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Data.Time

data Tool = Tool
    { toolId :: Int
    , name :: String
    , description :: String
    , lastReturned :: Day
    , timesBorrowed :: Int
    }

data User = User
    { userId :: Int
    , userName :: String
    }

instance Show User where
    show user = mconcat [ show $ userId user
                        , ".) "
                        , userName user ]

instance Show Tool where
   show tool = mconcat [ show $ toolId tool
                       , ".) "
                       , name tool
                       , "\n description: "
                       , description tool
                       , "\n last returned: "
                       , show $ lastReturned tool
                       , "\n times borrowed: "
                       , show $ timesBorrowed tool
                       , "\n"]


withConn :: String -> (Connection -> IO ()) -> IO ()
withConn dbName action = do
    conn <- open dbName
    action conn
    close conn

addUser :: String -> IO ()
addUser userName = withConn "tools.db" $
                   \conn -> do
                     execute conn "INSERT INTO users (username) VALUES (?)"
                       (Only userName)
                     print "user added"

checkout :: Int -> Int -> IO ()
checkout userId toolId = withConn "tools.db" $
                         \conn -> do
                           execute conn
                             "INSERT INTO checkedout (user_id,tool_id) VALUES (?,?)"
                              (userId,toolId)

instance FromRow User where
    fromRow = User <$> field
                   <*> field

instance FromRow Tool where
    fromRow = Tool <$> field
                   <*> field
                   <*> field
                   <*> field
                   <*> field

printUsers :: IO ()
printUsers = withConn "tools.db" $
             \conn -> do
               resp <- query_ conn "SELECT * FROM users;" :: IO [User]
               mapM_ print resp

printToolQuery :: Query -> IO ()
printToolQuery q = withConn "tools.db" $
                    \conn -> do
                      resp <- query_ conn q :: IO [Tool]
                      mapM_ print resp

printTools :: IO ()
printTools = printToolQuery "SELECT * FROM tools;"

printAvailable :: IO ()
printAvailable = printToolQuery $
             mconcat [ "select * from tools "
                     , "where id not in "
                     , "(select tool_id from checkedout);"]

printCheckedout :: IO ()
printCheckedout = printToolQuery $
             mconcat [ "select * from tools "
                     , "where id in "
                     , "(select tool_id from checkedout);"]

selectTool :: Connection -> Int -> IO (Maybe Tool)
selectTool conn toolId = do
  resp <- query conn
            "SELECT * FROM tools WHERE id = (?)"
            (Only toolId) :: IO [Tool]
  return $ firstOrNothing resp

firstOrNothing :: [a] -> Maybe a
firstOrNothing [] = Nothing
firstOrNothing (x:_) = Just x

updateTool :: Tool -> Day -> Tool
updateTool tool date = tool
    { lastReturned = date
    , timesBorrowed = 1 + timesBorrowed tool
    }

updateOrWarn :: Maybe Tool -> IO ()
updateOrWarn Nothing = putStrLn "id not found"
updateOrWarn (Just tool) = withConn "tools.db" $
                            \conn -> do
                              let q = mconcat [ "update tools set "
                                              , "lastReturned = ?,"
                                              , " timesBorrowed = ? "
                                              , "where id = ?;"]
                              execute conn q ( lastReturned tool
                                             , timesBorrowed tool
                                             , toolId tool)
                              putStrLn "tool updated"

updateToolTable :: Int -> IO ()
updateToolTable toolId = withConn "tools.db" $
                           \conn -> do
                             tool <- selectTool conn toolId
                             currentDay <- utctDay <$> getCurrentTime
                             let updatedTool = updateTool <$> tool
                                                          <*> pure currentDay
                             updateOrWarn updatedTool

checkin :: Int -> IO ()
checkin toolId = withConn "tools.db" $
                  \conn -> do
                    execute conn "delete from checkedout where tool_id = (?);"
                                  (Only toolId)

checkinAndUpdate :: Int -> IO ()
checkinAndUpdate toolId = do
    checkin toolId
    updateToolTable toolId

promptAndAddUser :: IO ()
promptAndAddUser = do
    putStrLn "Enter new user name:"
    userName <- getLine
    addUser userName

promptAndCheckout :: IO ()
promptAndCheckout = do
    putStrLn "Enter the id of the user:"
    userId <- pure read <*> getLine
    putStrLn "Enter the id of the tool:"
    toolId <- pure read <*> getLine
    checkout userId toolId

promptAndCheckin :: IO ()
promptAndCheckin = do
    putStrLn "Enter the id of the tool:"
    toolId <- pure read <*> getLine
    checkinAndUpdate toolId

-- (>>) performs the next action without passing a result
performCommand :: String -> IO ()
performCommand command
    | command == "users" = printUsers >> main
    | command == "tools" = printTools >> main
    | command == "adduser" = promptAndAddUser >> main
    | command == "checkout" = promptAndCheckout >> main
    | command == "checkin" = promptAndCheckin >> main
    | command == "in" = printAvailable >> main
    | command == "out" = printCheckedout >> main
    | command == "quit" = putStrLn "ciao!"
    | otherwise = putStrLn "Sorry, command not found" >> main

main :: IO ()
main = do
    putStrLn "Enter a command:"
    putStrLn "(users, tools, adduser, checkout, checkin, in, out, quit)"
    command <- getLine
    performCommand command
