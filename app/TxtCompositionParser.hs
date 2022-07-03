module TxtCompositionParser (
    parseComposition,
) where

import Text.Read (readMaybe)

import Data.Strings (strToUpper)

import qualified TOP
import Common


-- | Data structure to represent states of the composition from text parser.
data ParserState
    = Init  -- ^ Initial (and (the only) final state). Expects either `"Rest"` or `"Note"`

    | RestExpectDur  -- ^ Now parsing a rest entry. Expect a duration as next input.

    | NoteExpectOctave  -- ^ Now parsing a note. Expect an octave as next input.
    | NoteExpectPitch Octave  -- ^ Now parsing a note. Already got octave, now expecting a pitch.
    | NoteExpectDur Octave Pitch  -- ^ Now parsing a note. Already got octave and pitch, now expacting duration.

isFinal :: ParserState -> Bool
isFinal Init = True
isFinal _ = False

-- | Transition function for the parser.
transition :: ParserState -> String -> Maybe (ParserState, Maybe Symbol)
transition Init "Rest" = Just (RestExpectDur, Nothing)
transition Init "Note" = Just (NoteExpectOctave, Nothing)

transition RestExpectDur s =
    case parseDuration s of
         Nothing  -> Nothing
         Just dur -> Just (Init, Just (Rest dur))

transition NoteExpectOctave s =
    case parseOctave s of
         Nothing  -> Nothing
         Just oct -> Just (NoteExpectPitch oct, Nothing)
transition (NoteExpectPitch oct) s =
    case parsePitch s of
         Nothing    -> Nothing
         Just pitch -> Just (NoteExpectDur oct pitch, Nothing)
transition (NoteExpectDur oct pitch) s =
    case parseDuration s of
         Nothing  -> Nothing
         Just dur -> Just (Init, Just (Note oct pitch dur))

transition _ _ = Nothing

-- | Convert a string to a duration, if possible
parseDuration :: String -> Maybe Duration
parseDuration = readMaybe

-- | Convert a string to an octave number, if possible.
--
-- Note: only octaves 0 to 9 are allowed.
parseOctave :: String -> Maybe Octave
parseOctave s =
    case readMaybe s of
         Nothing -> Nothing
         Just n -> if 0 <= n && n <= 9 then Just n else Nothing

-- | Parse pitch in a case-insensitive manner, if possible
parsePitch :: String -> Maybe Pitch  -- Note:
parsePitch s = parsePitchUpper (strToUpper s)
    where
        parsePitchUpper "C"  = Just C
        parsePitchUpper "CS" = Just Cs
        parsePitchUpper "D"  = Just D
        parsePitchUpper "DS" = Just Ds
        parsePitchUpper "E"  = Just E
        parsePitchUpper "F"  = Just F
        parsePitchUpper "FS" = Just Fs
        parsePitchUpper "G"  = Just G
        parsePitchUpper "GS" = Just Gs
        parsePitchUpper "A"  = Just A
        parsePitchUpper "AS" = Just As
        parsePitchUpper "B"  = Just B
        parsePitchUpper _    = Nothing


pretokenize :: String -> [String]
pretokenize s = dropEmpty (splitBy [' ', '\r', '\n'] s)
    where
        dropEmpty = filter notEmpty
        notEmpty [] = False
        notEmpty _ = True

-- | Splits a sequence of elements ("alphas") by specified alphas.
-- |
-- | Given the sequence of alphas that should serve as splitters and
-- | an input (that is a list of alphas), splits the input by splitters
-- | and returns a list of lists of alphas.
splitBy
    :: Eq alpha
    => [alpha]    -- ^ Splitters
    -> [alpha]    -- ^ Input
    -> [[alpha]]  -- ^ Split result
splitBy _splitters [] = []
splitBy splitters (i:rest) =
    if elem i splitters
       then [] : splitBy splitters rest
       else case splitBy splitters rest of
                 [] -> [[i]]
                 (tok:toks) -> (i:tok):toks


-- | Try to parse the given string to a composition.
-- | On success return `Right composition`. On failure return the tokens that
-- | were not successfully parsed.
parseComposition :: String -> Either ([String], Maybe String) Composition
parseComposition s =
    let pretokens = pretokenize s in
        case TOP.parse pretokens Init isFinal transition of
             Left err  -> Left err
             Right out -> Right (Composition out)
