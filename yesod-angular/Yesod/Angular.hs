{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Yesod.Angular
    ( YesodAngular (..)
    , runAngular
    , addCommand
    , addCtrl
    , addCtrlRaw
    , setDefaultRoute
    , GAngular
    ) where

import           Control.Applicative        ((<$>))
import           Control.Monad.Trans.Writer (WriterT, runWriterT, tell)
import           Data.Aeson                 (FromJSON, ToJSON)
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           Data.Maybe                 (fromMaybe)
import           Data.Monoid                (First (..), Monoid (..))
import           Data.Text                  (Text)
import           Text.Hamlet                (HtmlUrl, hamletFile)
import           Text.Julius                (JavascriptUrl, julius, juliusFile)
import           Yesod.Core                 (GHandler, GWidget, RepHtml,
                                             RepHtml (RepHtml), Route, Yesod,
                                             addScriptEither, defaultLayout,
                                             getUrlRenderParams, getYesod, lift,
                                             lookupGetParam, newIdent,
                                             sendResponse, toContent,
                                             toWidget, whamlet)
import           Yesod.Json                 (jsonToRepJson, parseJsonBody_)
import Language.Haskell.TH.Syntax (Q, Exp (AppE, LitE), Lit (StringL))
import qualified Data.Text as T
import Data.Char (isAlpha)

class Yesod master => YesodAngular master where
    urlAngularJs :: master -> Either (Route master) Text
    urlAngularJs _ = Right "//ajax.googleapis.com/ajax/libs/angularjs/1.0.2/angular.min.js"

    wrapAngular :: Text -> GWidget sub master () -> GHandler sub master RepHtml
    wrapAngular modname widget = defaultLayout [whamlet|<div ng-app=#{modname}>^{widget}|]

data AngularWriter sub master = AngularWriter
    { awCommands     :: Map Text (GHandler sub master ())
    , awPartials     :: Map Text (HtmlUrl (Route master))
    , awRoutes       :: JavascriptUrl (Route master)
    , awControllers  :: JavascriptUrl (Route master)
    , awDefaultRoute :: First Text
    }
instance Monoid (AngularWriter sub master) where
    mempty = AngularWriter mempty mempty mempty mempty mempty
    AngularWriter a1 a2 a3 a4 a5
        `mappend` AngularWriter b1 b2 b3 b4 b5
        = AngularWriter
            (mappend a1 b1)
            (mappend a2 b2)
            (mappend a3 b3)
            (mappend a4 b4)
            (mappend a5 b5)

type GAngular sub master = WriterT (AngularWriter sub master) (GHandler sub master)

runAngular :: YesodAngular master
           => GAngular sub master ()
           -> GHandler sub master RepHtml
runAngular ga = do
    master <- getYesod
    ((), AngularWriter{..}) <- runWriterT ga
    mc <- lookupGetParam "command"
    fromMaybe (return ()) $ mc >>= flip Map.lookup awCommands
    mp <- lookupGetParam "partial"
    case mp >>= flip Map.lookup awPartials of
        Nothing -> return ()
        Just htmlurl -> getUrlRenderParams >>= sendResponse . RepHtml . toContent . htmlurl

    modname <- newIdent

    let defaultRoute =
            case awDefaultRoute of
                First (Just x) -> [julius|.otherwise({redirectTo:"#{x}"})|]
                First Nothing -> mempty

    wrapAngular modname $ do
        addScriptEither $ urlAngularJs master
        [whamlet|<div ng-view>|]
        toWidget [julius|
angular
    .module("#{modname}", [])
    .config(["$routeProvider", function($routeProvider) {
        $routeProvider ^{awRoutes} ^{defaultRoute} ;
    }]);
^{awControllers}
|]

addCommand :: (FromJSON input, ToJSON output)
           => (input -> GHandler sub master output)
           -> GAngular sub master Text
addCommand f = do
    name <- lift newIdent
    tell mempty { awCommands = Map.singleton name handler }
    return $ "?command=" `mappend` name
  where
    handler = do
        input <- parseJsonBody_
        output <- f input
        repjson <- jsonToRepJson output
        sendResponse repjson

addCtrl :: Text -- ^ route pattern
        -> Text -- ^ template name
        -> Q Exp
addCtrl route name = do
    let name' = T.filter isAlpha name
    [|addCtrlRaw $(liftT name') $(liftT route) $(hamletFile $ fn "hamlet") $(juliusFile $ fn "julius")|]
  where
    liftT t = do
        p <- [|T.pack|]
        return $ AppE p $ LitE $ StringL $ T.unpack t
    fn suffix = T.unpack $ T.concat ["angular/", name, ".", suffix]

addCtrlRaw :: Text -- ^ user-friendly name
           -> Text -- ^ route pattern
           -> HtmlUrl (Route master) -- ^ template
           -> JavascriptUrl (Route master) -- ^ controller
           -> GAngular sub master ()
addCtrlRaw name' route template controller = do
    name <- (mappend $ mappend name' "__") <$> lift newIdent
    tell mempty
        { awPartials = Map.singleton name template
        , awRoutes = [julius|.when("#{route}", {controller:#{name}, templateUrl:"?partial=#{name}"})|]
        , awControllers = [julius|var #{name} = ^{controller};|]
        }

setDefaultRoute :: Text -> GAngular sub master ()
setDefaultRoute x = tell mempty { awDefaultRoute = First $ Just x }
