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
    , AngularT
    ) where

import           Control.Applicative
import           Data.Char (isAlpha)
import           Control.Monad.Trans.Writer
import           Data.Aeson
import qualified Data.Text as T
import           Data.Map
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Monoid
import           Data.Text
import           Language.Haskell.TH.Syntax (Q, Exp (AppE, LitE), Lit (StringL))
import           Text.Hamlet
import           Text.Julius
import           Yesod


-- | YesodAngular wraps a widget in ng-app named @modname.
class Yesod site => YesodAngular site where
    urlAngularJs :: site -> Either (Route site) Text
    urlAngularJs _ = Right "//cdnjs.cloudflare.com/ajax/libs/angular.js/1.2.16/angular.min.js"

    wrapAngular :: Text -> WidgetT site IO () -> HandlerT site IO Html
    wrapAngular modname widget = defaultLayout [whamlet| <div ng-app=#{modname}>^{widget} |]


data AngularWriter site = AngularWriter
    { awCommands     :: Map Text (HandlerT site IO ())
    , awPartials     :: Map Text (HtmlUrl (Route site))
    , awRoutes       :: JavascriptUrl (Route site)
    , awControllers  :: JavascriptUrl (Route site)
    , awDefaultRoute :: First Text
    }


type AngularT site = WriterT (AngularWriter site) (HandlerT site IO)


instance Monoid (AngularWriter site) where
    mempty = AngularWriter mempty mempty mempty mempty mempty
    AngularWriter a1 a2 a3 a4 a5
        `mappend` AngularWriter b1 b2 b3 b4 b5 = AngularWriter
                                                    (mappend a1 b1)
                                                    (mappend a2 b2)
                                                    (mappend a3 b3)
                                                    (mappend a4 b4)
                                                    (mappend a5 b5)


-----------------------------------------------------------------------


runAngular :: YesodAngular site
           => AngularT site ()
           -> HandlerT site IO Html
runAngular ga = do
    site <- getYesod
    ((), AngularWriter{..}) <- runWriterT ga
    mc <- lookupGetParam "command"
    fromMaybe (return ()) $ mc >>= flip Map.lookup awCommands
    mp <- lookupGetParam "partial"
    case mp >>= flip Map.lookup awPartials of
        Nothing -> return ()
        Just htmlurl -> do
            ps <- getUrlRenderParams
            let rep = toTypedContent . htmlurl $ ps
            sendResponse rep

    modname <- newIdent

    let defaultRoute =
            case awDefaultRoute of
                First (Just x) -> [julius|.otherwise({redirectTo:"#{rawJS x}"})|]
                First Nothing -> mempty

    wrapAngular modname $ do
        addScriptEither $ urlAngularJs site
        addScriptRemote "//cdnjs.cloudflare.com/ajax/libs/angular.js/1.2.16/angular-route.min.js"
        [whamlet| <div ng-view> |]
        toWidget [julius|
            angular.module("#{rawJS modname}", ['ngRoute']).config(["$routeProvider",
                function($routeProvider, $locationProvider) {
                    $routeProvider ^{awRoutes} ^{defaultRoute};
                }]);
            ^{awControllers}
        |]


addCommand :: (FromJSON input, ToJSON output)
           => (input -> HandlerT site IO output)
           -> AngularT site Text
addCommand f = do
    name <- lift newIdent
    tell mempty { awCommands = Map.singleton name handler }
    return $ "?command=" `mappend` name
  where
    handler = do
        input <- requireJsonBody
        output <- f input
        repjson <- returnJson output
        sendResponse repjson


setDefaultRoute :: Text -> AngularT site ()
setDefaultRoute x = tell mempty { awDefaultRoute = First $ Just x }


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
           -> HtmlUrl (Route site) -- ^ template
           -> JavascriptUrl (Route site) -- ^ controller
           -> AngularT site ()
addCtrlRaw name' route template controller = do
    name <- mappend (mappend name' "__") <$> lift newIdent
    tell mempty
        { awPartials = Map.singleton name template
        , awRoutes = [julius| .when("#{rawJS route}",
                                { "controller": #{rawJS name}
                                , "templateUrl": "?partial=#{rawJS name}"
                                })
                     |]
        , awControllers = [julius| var #{rawJS name} = ^{controller} |]
        }
