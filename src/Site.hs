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
import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.Heist
import Snap.Util.FileServe
import System.IO
import Text.Templating.Heist
import qualified  Text.XmlHtml as X

import Application
import CSPM hiding (App)
import Data.List.Split
import Text.SyntaxHighlight.TextMate
import Text.Printf
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
    heistLocal (bindSplices [
        ("result", resultSplice r),
        ("sourcecode", sourceCodeSplice source),
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
            _ <- typeCheckFile renamedFile
            return ()
        ws <- getState lastWarnings
        case merr of
            Left (SourceError es) -> return (False, ws, es)
            Right _ -> return (True, ws, [])
            _ -> error "Internal error"

mkElem :: String -> [(String, String)] -> [X.Node] -> X.Node
mkElem e attrs children =
    X.Element (T.pack e) [(T.pack k, T.pack v) | (k,v) <- attrs] children

mkText :: String -> X.Node
mkText str = X.TextNode (T.pack str)

--mkHtml :: String -> [X.Node]
--mkHtml str = 
--    case X.parseXML "" (BC.fromString str) of
--        Left s -> error (s++"\n"++str)
--        Right (X.XmlDocument _ _ ns) -> ns
--        Right _ -> error "Unknown HTML Type"

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

plainSourceCode :: String -> Splice AppHandler
plainSourceCode s = return [mkText s]

sourceCodeSplice :: String -> Splice AppHandler
sourceCodeSplice source = do
    syntaxFile <- lift $ gets _cspmSynytaxFile

    let
        fileToHtml :: HLFile -> [X.Node]
        fileToHtml (HLFile segs) = fst $ processSegments (HLNewLine:segs) 0

        processSegments :: [HLSegment] -> Int -> ([X.Node], Int)
        processSegments [] lineNo = ([], lineNo)
        processSegments (seg:segs) lineNo = 
            let
                (n, l) = segmentToHtml seg lineNo
                (ns, l') = processSegments segs l
            in (n:ns, l') 

        segmentToHtml :: HLSegment -> Int -> (X.Node, Int)
        segmentToHtml (HLString s) lineNo = (mkText s, lineNo)
        segmentToHtml HLNewLine lineNo = 
            (mkElem "span" [("class", "line_number"), ("id", "line"++show lineNo)] 
                [mkText lineNoTxt], lineNo+1)
            where lineNoTxt = printf "%4d" lineNo
        segmentToHtml (HLTag tag segs) lineNo =
            (mkElem "span" [("class", tagClass)] ns, l')
            where
                (ns, l') = processSegments segs lineNo

                tagClass :: String
                tagClass = punc " " $ map (punc "_") $ allPrefixes $ splitOn "." tag

                punc :: String -> [String] -> String
                punc _ [] = []
                punc _ [x] = x
                punc s (x:xs) = x ++ s ++ punc s xs

                allPrefixes :: [a] -> [[a]]
                allPrefixes [] = []
                allPrefixes [x] = [[x]]
                allPrefixes (x:xs) = [x] : map ((:) x) (allPrefixes xs)
    
    return $ fileToHtml $ highlightFile syntaxFile source

-- | The application's routes.
routes :: [(BS.ByteString, Handler App App ())]
routes = [
    ("/", index),
    ("/ajax/typecheck", ajaxTypeCheck),
    ("/static/", serveDirectory "resources/static")]

-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "CSPM TypeChecker Application." Nothing $ do
    h <- nestSnaplet "heist" heist $ heistInit "resources/templates"
    syntaxF <-
        liftIO $ parseSyntaxFile "dependencies/cspm-textmate/CSPM.tmLanguage"
    addRoutes routes
    return $ App h syntaxF

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
