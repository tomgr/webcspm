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
import qualified Data.Map as M
import Data.Maybe
import Data.List.Split
import qualified Data.Text as T
import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.Heist
import Snap.Util.FileServe
import System.IO
import Text.Templating.Heist
import Text.Printf
import qualified  Text.XmlHtml as X

import Application
import CSPM hiding (App)
import Text.SyntaxHighlight.TextMate
import Util.Annotated
import Util.Exception
import Util.PrettyPrint hiding (render)

decodedParam :: MonadSnap f => BC.ByteString -> f BC.ByteString
decodedParam p = fromMaybe "" <$> getParam p

index :: Handler App App ()
index =
    let
        typeCheck src f =
            heistLocal (bindSplices [("plainsource", textSplice $ T.pack $ src)]) $ f

        getter = typeCheck "" $ render "site"
        setter = do
            sourcebs <- decodedParam "sourceCode"
            typeCheck (BC.toString sourcebs) $ render "site_with_results"
    in method GET getter <|> method POST setter

help :: Handler App App ()
help = render "help"

mkElem :: String -> [(String, String)] -> [X.Node] -> X.Node
mkElem e attrs children =
    X.Element (T.pack e) [(T.pack k, T.pack v) | (k,v) <- attrs] children

mkText :: String -> X.Node
mkText str = X.TextNode (T.pack str)

typecheckCSPMSplice :: SnapletSplice b App
typecheckCSPMSplice = do
    node <- liftHeist $ getParamNode

    let sourceNode = case X.childElementTag "script" node of
                    Just n -> n
                    Nothing -> case X.childElementTag "escapedscript" node of
                                Just n -> n
                                Nothing -> error "Could not find source"

    -- Recursively process the source code.
    [processedSourceNode] <- liftHeist $ runNodeList [sourceNode]
    let sourceCode = T.unpack $ X.nodeText processedSourceNode

    (suceeded, warnings, errors, types) <- liftIO (libcspmTypeCheck sourceCode)

    syntaxFile <- gets _cspmSynytaxFile
    let
        Just outputFormat = X.childElementTag "output" node
        linePrefix = X.getAttribute "id" outputFormat
        outputNodes = X.elementChildren outputFormat

        mkDefn cls label err = liftHeist $
            callTemplateWithText "type_error" [
                ("labelclass", cls),
                ("label", label),
                ("location", T.pack $ show $ location err),
                ("linenumber", T.pack $ mkLineTitle linePrefix lineNo),
                ("message", T.pack $ show $ message $ err)
            ]
            where lineNo = case location err of
                                Unknown -> ""
                                l -> show $ srcLocLine $ srcSpanStart l
        
        typeMap = M.fromList [(show n, show $ prettyPrint t) | (n,t) <- types]

        makeOutput :: X.Node -> SnapletSplice b App
        makeOutput n | X.tagName n == Just "source" = return $
            sourceCodeNode syntaxFile linePrefix typeMap sourceCode
        makeOutput n | X.tagName n == Just "outcome" = return $ pack $ fromJust $
            if suceeded then X.childElementTag "passes" n
            else X.childElementTag "fails" n
        makeOutput n | X.tagName n == Just "errors" = do
            es <- mapM (mkDefn "important" "Error") errors
            ws <- mapM (mkDefn "warning" "Warning") warnings
            return $ pack $ mkElem "dl" [("class", "error_list")] (concat es++concat ws)
        makeOutput n = return [n]
        
        pack x = [x]
    
    nss <- mapM makeOutput outputNodes
    return $ concat nss

blockCSPMSplice :: SnapletSplice b App
blockCSPMSplice = do
    node <- liftHeist $ getParamNode
    syntaxFile <- gets _cspmSynytaxFile
    return $ sourceCodeNode syntaxFile (X.getAttribute "id" node) (M.fromList []) $
        case X.childElementTag "script" node of
            Just n -> T.unpack $ X.nodeText $ n
            Nothing -> T.unpack $ X.nodeText $ node

typeSplice :: SnapletSplice b App
typeSplice = do
    node <- liftHeist $ getParamNode
    return [mkElem "span" [("class", "type")] [X.TextNode $ X.nodeText node]]

cspmSplice :: SnapletSplice b App
cspmSplice = do
    node <- liftHeist $ getParamNode
    return [mkElem "span" [("class", "cspm")] [X.TextNode $ X.nodeText node]]

mkLineTitle :: Maybe T.Text -> String -> String
mkLineTitle (Just linePrefix) lineNo = (T.unpack linePrefix)++"_line"++lineNo
mkLineTitle Nothing lineNo = "line"++lineNo

-- | A splice that highlights the given string as CSPM source coe.
sourceCodeNode :: TMSyntaxFile -> Maybe T.Text -> M.Map String String -> String -> [X.Node]
sourceCodeNode syntaxFile linePrefix typeMap source =
    let
        fileToHtml :: HLFile -> [X.Node]
        fileToHtml (HLFile segs) = fst $ processSegments (HLNewLine:segs) 1

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
            (mkElem "span" [("class", "line_number"), ("id", mkLineTitle linePrefix (show lineNo))] 
                [mkText lineNoTxt], lineNo+1)
            where lineNoTxt = printf "%4d" lineNo
        segmentToHtml (HLTag tag segs) lineNo =
            (mkElem "span" (("class", tagClass):extraAttrs) ns, l')
            where
                extraAttrs = 
                    if tag == "entity.name.function.cspm" then
                        case head segs of
                            HLString s -> case M.lookup s typeMap of
                                Just t -> [("data-type", t)]
                                Nothing -> []
                            _ -> []
                    else []
                
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

    in [mkElem "pre" [("class", "textmate-highlight solarized_light_")] 
                (fileToHtml $ highlightFile syntaxFile source)]

-- | The application's routes.
routes :: [(BS.ByteString, Handler App App ())]
routes = [
    ("/", index),
    ("/help", help),
    ("/static/", serveDirectory "resources/static")]

-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "CSPM TypeChecker Application." Nothing $ do
    h <- nestSnaplet "heist" heist $ heistInit "resources/templates"
    cspmSyntaxFile <-
        liftIO $ parseSyntaxFile "dependencies/cspm-textmate/CSPM.tmLanguage"
    addRoutes routes
    addSplices [
        ("type", typeSplice),
        ("cspm", cspmSplice),
        ("blockcspm", blockCSPMSplice),
        ("typecheck", typecheckCSPMSplice)]
    return $ App h cspmSyntaxFile

-- ****************************************************************************
-- LibCSPM Functions

libcspmTypeCheck :: String -> IO (Bool, [ErrorMessage], [ErrorMessage], [(Name, Type)])
libcspmTypeCheck input = do
    session <- initCSPMState
    runCSPMWarningM session $ do
        merr <- tryM $ do
            parsedFile <- parseStringAsFile input
            renamedFile <- renameFile parsedFile
            tcFile <- typeCheckFile renamedFile
            dsFile <- desugarFile tcFile
            bindFile dsFile
            ns <- getBoundNames
            mapM (\n -> do
                    pexp <- parseExpression (show n)
                    rnexp <- renameExpression pexp
                    t <- typeOfExpression rnexp
                    return (n, t)
                ) ns
        ws <- getState lastWarnings
        case merr of
            Left (SourceError es) -> return (False, ws, es, [])
            Right m -> return (True, ws, [], m)
            _ -> error "Internal error"

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
