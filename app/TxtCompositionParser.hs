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
         Just dur -> Just (Init, Just $ Rest dur)

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
         Just dur -> Just (Init, Just $ Note oct pitch dur)

transition _ _ = Nothing

-- | Convert a string to a duration, if possible
parseDuration :: String -> Maybe Duration
parseDuration = readMaybe

-- | Convert a string to an octave number, if possible.
--
-- Note: only octaves 2 to 8 are allowed.
parseOctave :: String -> Maybe Octave
parseOctave s =
    case readMaybe s of
         Nothing -> Nothing
         Just n -> if 2 <= n && n <= 8 then Just n else Nothing

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


-- Just an ugly way to split by whitespace (TODO: improve)
pretokenize :: String -> [String]
pretokenize "" = []
pretokenize (' ':s) = "":pretokenize s
pretokenize ('\r':s) = "":pretokenize s
pretokenize ('\n':s) = "":pretokenize s
pretokenize (c:s) =
    case pretokenize s of
         [] -> [c:""]
         (tok:toks) -> (c:tok):toks


-- | Try to parse the given string to a composition.
-- | On success return `Right composition`. On failure return the tokens that
-- | were not successfully parsed.
parseComposition :: String -> Either ([String], Maybe String) Composition
parseComposition s =
    let pretokens = pretokenize s in
        case TOP.parse pretokens Init isFinal transition of
             Left err  -> Left err
             Right out -> Right $ Composition out
