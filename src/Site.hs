{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Site (
  app
) where

import Control.Applicative
import Control.Monad.Trans
import Control.Monad.State
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BC
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.Heist
import Snap.Util.FileServe
import Text.Templating.Heist
import qualified  Text.XmlHtml as X

import Application
import CSPM hiding (App)
import Util.Annotated
import Util.Exception
import Util.PrettyPrint hiding (render)

index :: Handler App App ()
index = heistLocal (bindSplices []) $ render "index"

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
            Right v -> return (True, ws, [])

--method GET getter <|> method POST setter

typecheck :: Handler App App ()
typecheck = do
    sourcebs <- decodedParam "sourceCode"
    let source = BC.toString sourcebs
    r <- liftIO $ libcspmTypeCheck source
    heistLocal (bindSplices [
        ("result", resultSplice r),
        ("sourcecode", sourceCodeSplice source),
        ("plainsource", plainSourceCode source)]) $ render "type_check_result"
  where
    decodedParam p = fromMaybe "" <$> getParam p

mkElem e attrs children = X.Element (T.pack e) [(k, T.pack v) | (k,v) <- attrs] children
mkText str = X.TextNode (T.pack str)

resultSplice :: Monad m => (Bool, [ErrorMessage], [ErrorMessage]) -> Splice m
resultSplice (suceeded, warnings, errors) =
    let
        resultParagraph = mkText $
            if suceeded then "Typechecking Suceeded" else "Typechecking Failed"

        mkLocationLink loc = 
            let
                lineNo = 
                    case loc of 
                        Unknown -> ""
                        _ -> show (srcLocLine (srcSpanStart loc))
            in mkElem "a" [("href", "#line"++lineNo)] [mkText (show loc)]

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
sourceCodeSplice source = return $ pack $ mkElem "ol" [] $
    map (\ (ln, l) -> 
        mkElem "li" [] [mkElem "a" [("id", "line"++show ln)] [mkText (show ln++":")], 
        mkElem "pre" [] [mkText l]]
    ) (zip [1..] (lines source))

    where pack x = [x]

-- | The application's routes.
routes :: [(BS.ByteString, Handler App App ())]
routes = [
    ("/", index),
    ("/typecheck", typecheck),
    ("/static/", serveDirectory "resources/static")]

-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "CSPM TypeChecker Application." Nothing $ do
    h <- nestSnaplet "heist" heist $ heistInit "resources/templates"
    addRoutes routes
    return $ App h



data CSPMWarningState = CSPMWarningState {
        cspmSession :: CSPMSession,
        lastWarnings :: [ErrorMessage]
    }

initCSPMState :: IO CSPMWarningState
initCSPMState = do
    sess <- newCSPMSession
    return $ CSPMWarningState sess []

resetCSPM :: CSPMWarningMonad ()
resetCSPM = do
    sess <- liftIO $ newCSPMSession
    modify (\st -> st { cspmSession = sess, lastWarnings = [] })

type CSPMWarningMonad = StateT CSPMWarningState IO

runCSPMWarningM :: CSPMWarningState -> CSPMWarningMonad a -> IO a
runCSPMWarningM st a = runStateT a st >>= return . fst

getState :: (CSPMWarningState -> a) -> CSPMWarningMonad a
getState = gets

modifyState :: (CSPMWarningState -> CSPMWarningState) -> CSPMWarningMonad ()
modifyState = modify

instance CSPMMonad CSPMWarningMonad where
    getSession = gets cspmSession
    setSession s = modify (\ st -> st { cspmSession = s })
    handleWarnings ws = modify (\ st -> st { lastWarnings = ws })
