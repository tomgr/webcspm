-- | A module that provides an experimental syntax highlighter using TextMate
-- syntax files.
module Text.SyntaxHighlight.TextMate (
    TMSyntaxFile, parseSyntaxFile,
    HLFile(..), HLSegment(..), highlightFile,
) where

import Data.Array
import Data.List
import Data.List.Split
import qualified Data.Map as M
import Text.Regex.Base
import Text.Regex.PCRE.String
import Text.XML.Plist

data TMSyntaxFile =
    TMSyntaxFile {
        fileTypes :: [String],
        fileName :: String,
        patterns :: [TMRule],
        repository :: M.Map String TMRule,
        scopeName :: String,
        uniqueId :: String
    }

data TMRule = 
    Include {
        ruleKey :: String
    }
    | Match {
        regex :: Regex,
        name :: String,
        captures :: M.Map Int TMCapture
    }
    | RangeMatch {
        startRegex :: Regex,
        startCaptures :: M.Map Int TMCapture,
        endRegex :: Regex,
        endCaptures :: M.Map Int TMCapture,
        name :: String,
        contentName :: Maybe String,
        rmRules :: [TMRule]
    }
    | MultiRule {
        rmRules :: [TMRule]
    }

data TMCapture = TMCapture {
        identifier :: Int,
        nameAttribute :: String
    }

parseSyntaxFile :: String -> IO TMSyntaxFile
parseSyntaxFile str = do
    root <- readPlistFromFile str
    let 
        vForK :: PlObject -> String -> PlObject
        vForK (PlDict kvs) k = head [v | (k',v) <- kvs, k' == k]
        vForK _ _ = error "vForK: invalid arg"

        hasKey :: PlObject -> String -> Bool
        hasKey (PlDict kvs) k = length [v | (k',v) <- kvs, k' == k] > 0
        hasKey _ _ = error "hasKey: invalid arg"

        exStr :: PlObject -> String
        exStr (PlString s) = s
        exStr _ = error "exStr: not a string"

        exList :: PlObject -> [PlObject]
        exList (PlArray as) = as
        exList _ = error "exList: not a list"

        exDict :: PlObject -> [(String, PlObject)]
        exDict (PlDict ds) = ds
        exDict _ = error "exDict: not a dict"
        
        convertCapture :: (String, PlObject) -> TMCapture
        convertCapture (key, capObj) = TMCapture {
                identifier = read key,
                nameAttribute = exStr $ vForK capObj "name"
            }
        
        mkCaps cs = M.fromList [(identifier c, c) | c <- cs]

        convertPattern :: PlObject -> TMRule
        convertPattern patObj | hasKey patObj "match" =
            Match {
                regex = makeRegex $ exStr $ vForK patObj "match",
                name = exStr $ vForK patObj "name",
                captures = mkCaps $ map convertCapture $
                    if hasKey patObj "captures" then 
                        exDict $ vForK patObj "captures"
                    else []
            }
        convertPattern patObj | hasKey patObj "include" =
            Include {
                ruleKey = exStr $ vForK patObj "include"
            }
        convertPattern patObj | hasKey patObj "begin" =
            RangeMatch {
                startRegex = makeRegex $ exStr $ vForK patObj "begin",
                startCaptures = mkCaps $ map convertCapture $
                    if hasKey patObj "captures" then
                        exDict $ vForK patObj "captures"
                    else if hasKey patObj "startCaptures" then
                        exDict $ vForK patObj "startCaptures"
                    else [],
                endRegex = makeRegex $ exStr $ vForK patObj "end",
                endCaptures = mkCaps $ map convertCapture $
                    if hasKey patObj "captures" then
                        exDict $ vForK patObj "captures"
                    else if hasKey patObj "endCaptures" then
                        exDict $ vForK patObj "endCaptures"
                    else [],
                name = exStr $ vForK patObj "name",
                contentName = 
                    if hasKey patObj "contentName" then 
                        Just $ exStr $ vForK patObj "contentName"
                    else Nothing,
                rmRules = 
                    if hasKey patObj "patterns" then 
                        map convertPattern $ exList $ vForK patObj "patterns"
                    else []
            }
        convertPattern patObj | hasKey patObj "patterns" =
            MultiRule {
                rmRules = map convertPattern $ exList $ vForK patObj "patterns"
            }
        convertPattern patObj = error $ "Unrecognised pattern: "++show patObj

        syntaxFile :: TMSyntaxFile
        syntaxFile = TMSyntaxFile {
                fileTypes = map exStr $ exList $ vForK root "fileTypes",
                fileName = exStr $ vForK root "name",
                patterns = map convertPattern $ exList $ vForK root "patterns",
                repository = let PlDict kvs = vForK root "repository" in
                    M.fromList [(k, convertPattern v) | (k, v) <- kvs],
                scopeName = exStr $ vForK root "scopeName",
                uniqueId = exStr $ vForK root "uuid"
            }
    
    return syntaxFile

-- | Alias for ease. The parse stack contains the list of rules that currently
-- are allowed, and possibly a rule that we matched the start of, but have yet
-- to find the end of (i.e. it is necessarily a RangeMatch).
type ParserState = ([TMRule], Maybe TMRule)

extractRegex :: TMRule -> (Regex, (TMRule, Bool))
extractRegex (rule@(Match r _ _)) = (r, (rule, False))
extractRegex (rule@(RangeMatch start _ _ _ _ _ _)) = 
    (start, (rule, False))
extractRegex _ = error "Unexpected regex"

selectBest :: [(MatchText String, a)] -> Maybe (MatchText String, a)
selectBest [] = Nothing
selectBest rs = Just $ head $
    sortBy (\(mt1,_) (mt2,_) ->
            let
                (_, (offset1, _)) = mt1!0
                (_, (offset2, _)) = mt2!0
            in compare offset1 offset2) rs

highlightFile :: TMSyntaxFile -> String -> HLFile
highlightFile syntaxFile input =
    let
        reduceRules :: [TMRule] -> [TMRule]
        reduceRules rss =
            let
                ruleError k = error ("Could not find rules for "++show k)

                rulesForKey :: String -> [TMRule]
                rulesForKey ('#':k) = 
                    [M.findWithDefault (ruleError k) k (repository syntaxFile)]
                rulesForKey "$self" = reduce (patterns syntaxFile)
                rulesForKey k = ruleError k

                reduce (Include k : rs) = reduce (rulesForKey k ++ rs)
                reduce (rule@(Match _ _ _) : rs) = rule : reduce rs
                reduce (rule@(RangeMatch _ _ _ _ _ _ _) : rs) =
                    rule : reduce rs
                reduce (MultiRule rs : rs') = reduce (rs ++ rs')
                reduce [] = []
            in reduce rss

        -- | Match the lines, starting in the given parse state. Returns when
        -- either, all input has been consumed, or when the end regex of the
        -- current rule on the stack has been hit. Returns the segments produced,
        -- any remaining input.
        matchLines :: ParserState -> [String] -> ([HLSegment], [String])
        matchLines _ [] = ([], [])
        matchLines state (line:remainingLines) =
            let
                (stateRules, currentRule) = state

                mkCaptures :: MatchText String -> (TMRule, Bool) -> ([HLSegment], String)
                mkCaptures matchText (rule, isRangeEnd) =
                    let
                        (fullMatch, (fullMatchOffset, fullMatchLength)) = 
                            matchText!0

                        (0,captureCount) = bounds matchText

                        cs = if isRangeMatch rule then
                                if isRangeEnd then startCaptures rule 
                                else endCaptures rule
                            else captures rule

                        tag = name rule
                        
                        mkSegments :: Int -> String -> Int -> [HLSegment]
                        mkSegments _ rest capture | capture >= captureCount =
                            if rest /= [] then [HLString rest] else []
                        mkSegments offset rest capture = 
                            case matchText!capture of
                                (_, (matchOffset, matchLength)) ->
                                    let
                                        (matched, rest') = splitAt (matchOffset+matchLength) rest
                                        cap = 
                                            case M.lookup capture cs of
                                                Just c -> HLTag (nameAttribute c) [HLString matched]
                                                Nothing -> HLString matched

                                        b = cap:mkSegments (offset+matchLength) rest' (capture+1)

                                        (beforeText,_) = splitAt matchOffset rest
                                    in 
                                        if matchOffset == offset then b
                                        else HLString beforeText : b

                        (_, remainingLine) = splitAt (fullMatchOffset+fullMatchLength) line

                        segs' = if M.null cs then [HLTag tag [HLString fullMatch]]
                                else mkSegments 0 fullMatch 1
                    in ([HLTag tag segs'], remainingLine)
                
                -- | Returns, the match, (the matching rule, isEndMatch)
                matchRegexs :: [(Regex, a)] -> [(MatchText String, a)]
                matchRegexs [] = []
                matchRegexs ((regexp, label):rs) = 
                    case matchOnceText regexp line of
                        Just (_, mt, _) -> (mt, label):matchRegexs rs
                        Nothing -> matchRegexs rs
                
                matches :: [(MatchText String, (TMRule, Bool))]
                matches = matchRegexs $
                    case currentRule of
                        Just r -> (endRegex r, (r, True)):map extractRegex stateRules
                        Nothing -> map extractRegex stateRules
                
                isRangeMatch (RangeMatch _ _ _ _ _ _ _ ) = True
                isRangeMatch _ = False

                bestMatch :: Maybe (MatchText String, (TMRule, Bool))
                bestMatch = selectBest matches

                (segments, remainingInput) = case bestMatch of
                    Just (matchText, (r, True)) | isRangeMatch r ->
                        -- The best match was the end regex of a RangeMatch.
                        let (segs1, remainingLine) = mkCaptures matchText (r, True)
                        in (segs1, remainingLine:remainingLines)
                    Just (matchText, (r, False)) | isRangeMatch r ->
                        -- The best match was a start regex of a RangeMatch. Hence,
                        -- firstly match inside this RangeMatch, then continue
                        -- matching in the current state.
                        
                        -- TODO: contentName
                        let
                            (segs1, remainingLine) = mkCaptures matchText (r, False)
                            input' = remainingLine:remainingLines
                            (segs2, input'') = matchLines (reduceRules $ rmRules r, Just r) input'
                            (segs3, input''') = matchLines state input''
                        in (HLTag (name r) (segs1++segs2) : segs3, input''')
                    Just (matchText, (r, False)) ->
                        -- Just a normal match. Compute the captures and then
                        -- continue matching
                        let 
                            (segs1, remainingLine) = mkCaptures matchText (r, False)
                            (segs2, input') = matchLines state (remainingLine:remainingLines)
                        in (segs1++segs2, input')
                    Nothing -> 
                        -- No match found, just output a plain string and
                        -- consume this line.
                        case matchLines state remainingLines of
                            (segs1, input') -> (HLString line : HLNewLine : segs1, input')
                    _ -> error "Invalid best match."
            in case bestMatch of
                Nothing -> (segments, remainingInput)
                Just (mt, _) ->
                    let
                        (_, (offset, _)) = mt!0
                        (start, _) = splitAt offset line
                    in if offset == 0 then (segments, remainingInput)
                        else (HLString start : segments, remainingInput)
        
        splitLines = split (keepDelimsR $ oneOf "\n") input
        (segs, _) = matchLines (reduceRules $ patterns syntaxFile, Nothing) splitLines
    in HLFile [HLTag (scopeName syntaxFile) $ init segs]

data HLFile = HLFile [HLSegment]
    deriving Show

data HLSegment = 
    HLNewLine
    | HLString String
    | HLTag String [HLSegment]
    deriving Show
