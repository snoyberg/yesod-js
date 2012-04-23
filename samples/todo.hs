{-# LANGUAGE OverloadedStrings, QuasiQuotes, TemplateHaskell, KindSignatures,
             TypeFamilies, FlexibleContexts, GADTs, MultiParamTypeClasses,
             FlexibleInstances, TypeSynonymInstances, ScopedTypeVariables
  #-}
import Yesod.Core
import Yesod.Persist
import Data.Text (Text)
import Database.Persist.Sqlite
import Network.Wai.Handler.Warp (run)
import Yesod.Json
import Yesod.Form.Jquery (YesodJquery)
import Network.HTTP.Types (status201, status204)
import Yesod.Javascript
import Data.Aeson hiding (object)
import Control.Applicative ((<$>), (<*>))
import Text.Lucius (lucius)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Todo
    text Text
    done Bool
|]

instance ToJSON (Entity Todo) where
    toJSON (Entity tid (Todo text done)) = object
        [ "id" .= tid
        , "text" .= text
        , "done" .= done
        ]
instance FromJSON Todo where
    parseJSON (Object o) = Todo
        <$> o .: "text"
        <*> o .: "done"
    parseJSON _ = fail "Invalid todo"

data App = App ConnectionPool

mkYesod "App" [parseRoutes|
/ HomeR GET
/todo TodosR GET PUT
/todo/#TodoId TodoR GET DELETE
|]

instance Yesod App
instance YesodPersist App where
    type YesodPersistBackend App = SqlPersist
    runDB f = do
        App pool <- getYesod
        runSqlPool f pool
instance YesodJquery App

jsTodoText :: JSValue (Entity Todo) -> JSValue Text
jsTodoText = jsGetter "text"

jsTodoDone :: JSValue (Entity Todo) -> JSValue Bool
jsTodoDone = jsGetter "done"

getHomeR :: Handler RepHtml
getHomeR = defaultLayout $ do
    toWidget [lucius|
.done {
    color: #999;
}
|]
    runJS $ do
        (todos :: JSValue [Entity Todo], reload) <- ajaxJson TodosR
        list <- htmlOutput $ wrapTag "ul" $ jsjoin $ jsfor todos $ \todo ->
            wrapTagClass "li"
                (jsif (jsTodoDone todo) "done" "notdone")
                $ jsToHtml $ jsTodoText todo

        countWidget <- textOutput $ jsShowInt $ jslength todos

        (taskWidget, taskValue) <- textInput

        putter <-
            let val = jsonObject
                    [ ("text", jsCast taskValue)
                    , ("done", jsCast jsFalse)
                    ]
             in putJson
                    TodosR
                    val
                    reload
        submitWidget <- button [whamlet|Add Item|] putter

        lift [whamlet|
<h2>Item count: ^{countWidget}
^{list}
<p>
    Enter new task: #
    ^{taskWidget}
    ^{submitWidget}
|]

getTodosR :: Handler RepJson
getTodosR =
    runDB (selectList [] []) >>= jsonToRepJson . asTodoEntities
  where
    asTodoEntities :: [Entity Todo] -> [Entity Todo]
    asTodoEntities = id

putTodosR :: Handler ()
putTodosR = do
    todo <- parseJsonBody_
    tid <- runDB $ insert todo
    sendResponseCreated $ TodoR tid

getTodoR :: TodoId -> Handler RepJson
getTodoR tid = runDB (get404 tid) >>= jsonToRepJson . Entity tid

deleteTodoR :: TodoId -> Handler ()
deleteTodoR tid = do
    runDB (delete tid)
    sendResponseStatus status204 ()

main :: IO ()
main = do
    pool <- createSqlitePool "todo.db3" 5
    runSqlPool (runMigration migrateAll) pool
    toWaiApp (App pool) >>= run 3000
