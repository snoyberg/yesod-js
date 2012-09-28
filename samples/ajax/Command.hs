{-# LANGUAGE GADTs, DeriveGeneric, OverloadedStrings #-}
module Command where

import JS.Prelude
import JS.Types
import Yesod.Core (GHandler, lookupGetParam, invalidArgs, RepJson, Yesod, Route)
import Yesod.Json (parseJsonBody_, jsonToRepJson)
import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics (Generic)
import Data.Text (Text)
import Data.Monoid ((<>))

data Command input output where
    Hello :: Command HelloInput HelloOutput
    Count :: Command CountInput CountOutput

data CommandServer sub master = CommandServer
    { serveHello :: HelloInput -> GHandler sub master HelloOutput
    , serveCount :: CountInput -> GHandler sub master CountOutput
    }

class Yesod master => RunsCommand master where
    commandRoute :: Route master

runCommandClient :: RunsCommand master
                 => JSExp input
                 -> (JSExp output -> JSBody ())
                 -> Command input output
                 -> JS master (JSExp ())
runCommandClient input success command = do
    as <- ajaxSettings input success (commandRoute, [("command", commandName command)])
    return $ ajax as

runCommandServer :: CommandServer sub master -> GHandler sub master RepJson
runCommandServer cs = do
    mcommand <- lookupGetParam "command"
    command <-
        case mcommand of
            Nothing -> invalidArgs ["command not provided"]
            Just command -> return command
    case () of
        () | command == commandName Hello -> helper $ serveHello cs
        () | command == commandName Count -> helper $ serveCount cs
        () | otherwise -> invalidArgs ["Invalid command: " <> command]
  where
    helper :: (FromJSON input, ToJSON output)
           => (input -> GHandler sub master output) -> GHandler sub master RepJson
    helper f = parseJsonBody_ >>= f >>= jsonToRepJson

commandName :: Command input output -> Text
commandName Hello = "hello"
commandName Count = "count"

data HelloInput = HelloInput
    { hiName :: Text
    }
    deriving Generic
instance ToJSON HelloInput
instance FromJSON HelloInput
data HelloOutput = HelloOutput
    { hoContent :: Text
    }
    deriving Generic
instance ToJSON HelloOutput
instance FromJSON HelloOutput

data CountInput = CountInput
    { ciWord :: Text
    }
    deriving Generic
instance ToJSON CountInput
instance FromJSON CountInput
data CountOutput = CountOutput
    { coLength :: Int
    }
    deriving Generic
instance ToJSON CountOutput
instance FromJSON CountOutput

helloInputJS :: JSExp Text -> JSExp HelloInput
helloInputJS t = JSObject [JSPair "hiName" t]

hoContentJS :: JSExp HelloOutput -> JSExp Text
hoContentJS = flip JSProperty "hoContent"

countInputJS :: JSExp Text -> JSExp CountInput
countInputJS t = JSObject [JSPair "ciWord" t]

coLengthJS :: JSExp CountOutput -> JSExp Int
coLengthJS = flip JSProperty "coLength"
