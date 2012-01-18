{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Site (
  app
) where

import Control.Applicative
import Control.Concurrent (myThreadId, killThread)
import Control.Monad.Trans
import Control.Monad.State
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BC
import Data.Maybe
import qualified Data.Text as T
import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.Heist
import Snap.Util.FileServe
import System.IO
import System.Process
import Text.Templating.Heist
import qualified  Text.XmlHtml as X

import Application
import CSPM hiding (App)
import Util.Annotated
import Util.Exception

decodedParam :: MonadSnap f => BC.ByteString -> f BC.ByteString
decodedParam p = fromMaybe "" <$> getParam p

index :: Handler App App ()
index =
    let
        getter = heistLocal (bindSplices [("plainsource", plainSourceCode "")]) 
            $ render "index"
        setter = typeCheck (render "type_check_result")
    in method GET getter <|> method POST setter

typeCheck :: Handler App App () -> Handler App App ()
typeCheck f = do
    sourcebs <- decodedParam "sourceCode"
    let source = BC.toString sourcebs
    r <- liftIO $ libcspmTypeCheck source
    src <- liftIO $ highlight source
    heistLocal (bindSplices [
        ("result", resultSplice r),
        ("sourcecode", sourceCodeSplice src),
        ("plainsource", plainSourceCode source)]) $ f

ajaxTypeCheck :: Handler App App ()
ajaxTypeCheck = typeCheck (render "type_check_output")

libcspmTypeCheck :: String -> IO (Bool, [ErrorMessage], [ErrorMessage])
libcspmTypeCheck input = do
    session <- initCSPMState
    runCSPMWarningM session $ do
        merr <- tryM $ do
            parsedFile <- parseStringAsFile input
            renamedFile <- renameFile parsedFile
            typeCheckFile renamedFile
            return ()
        ws <- getState lastWarnings
        case merr of
            Left (SourceError es) -> return (False, ws, es)
            Right _ -> return (True, ws, [])
            _ -> error "Internal error"

highlight :: String -> IO String
highlight input =
    readProcess "ruby" 
        ["dependencies/textmate2css/highlight.rb", 
        "-s", "dependencies/cspm-textmate/CSPM.tmLanguage"] input

mkElem :: String -> [(String, String)] -> [X.Node] -> X.Node
mkElem e attrs children =
    X.Element (T.pack e) [(T.pack k, T.pack v) | (k,v) <- attrs] children

mkText :: String -> X.Node
mkText str = X.TextNode (T.pack str)

mkHtml :: String -> [X.Node]
mkHtml str = 
    case X.parseXML "" (BC.fromString str) of
        Left s -> error (s++"\n"++str)
        Right (X.XmlDocument _ _ ns) -> ns
        Right _ -> error "Unknown HTML Type"

resultSplice :: Monad m => (Bool, [ErrorMessage], [ErrorMessage]) -> Splice m
resultSplice (suceeded, warnings, errors) =
    let
        resultParagraph = mkText $
            if suceeded then "Typechecking Suceeded" else "Typechecking Failed"

        mkLocationLink l = 
            let
                lineNo = 
                    case l of 
                        Unknown -> ""
                        _ -> show (srcLocLine (srcSpanStart l))
            in mkElem "a" [("href", "#line"++lineNo)] [mkText (show l)]

        formatMessage msg =
            [mkLocationLink (location msg), mkElem "pre" [] [mkText (show (message msg))]]

        mkPara xs s = 
            mkElem "ul" [("class", s)] (map (\ err -> mkElem "li" [] (formatMessage err)) xs)
        errorsParagraph = mkPara errors "error_list errors"
        warningsParagraph = mkPara warnings "error_list warnings"
    in return $ [resultParagraph, errorsParagraph, warningsParagraph]

plainSourceCode :: Monad m => String -> Splice m
plainSourceCode s= return [mkText s]

sourceCodeSplice :: Monad m => String -> Splice m
sourceCodeSplice source = return $ mkHtml source

-- | The application's routes.
routes :: [(BS.ByteString, Handler App App ())]
routes = [
    ("/", index),
    ("/ajax/typecheck", ajaxTypeCheck),
    ("/static/", serveDirectory "resources/static"),
    ("/pull_from_git/some_long_secret_that_noone-knows/", killServer)]

killServer :: Handler App App ()
killServer = do
    tid <- gets _threadId
    liftIO $ killThread tid
    writeText "Killed"

-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "CSPM TypeChecker Application." Nothing $ do
    h <- nestSnaplet "heist" heist $ heistInit "resources/templates"
    tid <- liftIO $ myThreadId
    addRoutes routes
    return $ App h tid



data CSPMWarningState = CSPMWarningState {
        cspmSession :: CSPMSession,
        lastWarnings :: [ErrorMessage]
    }

initCSPMState :: IO CSPMWarningState
initCSPMState = do
    sess <- newCSPMSession
    return $ CSPMWarningState sess []

type CSPMWarningMonad = StateT CSPMWarningState IO

runCSPMWarningM :: CSPMWarningState -> CSPMWarningMonad a -> IO a
runCSPMWarningM st a = runStateT a st >>= return . fst

getState :: (CSPMWarningState -> a) -> CSPMWarningMonad a
getState = gets

instance CSPMMonad CSPMWarningMonad where
    getSession = gets cspmSession
    setSession s = modify (\ st -> st { cspmSession = s })
    handleWarnings ws = modify (\ st -> st { lastWarnings = ws })
