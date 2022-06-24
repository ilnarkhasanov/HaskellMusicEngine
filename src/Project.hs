{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module Project where

import CodeWorld
import Data.List as DL
import Data.Maybe
import Codec.Midi
-- import System.Posix.Internals (o_EXCL)
-- import Data.Text

type Octave = Int
data Pitch = C | Cs | D | Ds | E | F | Fs | G | Gs | A | As | B
data Duration = Whole | Half | Quarter | Eight | Sixteen
data Symbol = Note Octave Pitch Duration | Rest Duration
data Composition = Composition [Symbol]


staffRenderer :: Int -> Picture
staffRenderer 0 = blank
staffRenderer n = staff <> (translated 0 (-2) (staffRenderer (n-1)))

compositionRenderer :: Composition -> Double -> Picture
compositionRenderer (Composition []) _ = blank
compositionRenderer (Composition (symbol:symbols)) n = translated (n - (24*(xToY n)*(-1/2))) 
  (xToY n) (symbolToPicture symbol)
  <> compositionRenderer (Composition symbols) (n+durToDouble symbol)
  

xToY :: Double -> Double
xToY x = (-2 * (fromIntegral (floor (x/24))))

durSum :: Composition -> Double
durSum (Composition []) = 0
durSum (Composition (symbol:symbols)) = durToDouble symbol + durSum (Composition symbols)

trebleClef :: Picture
trebleClef = scaled 1 1.2 (translated (-12) 0.27 (lettering "𝄞"))

line :: Double -> Picture
line dy = translated 0 (dy/5) (solidRectangle 25 0.015)

tactLine :: Double -> Picture
tactLine dx = translated (12.5 - dx*6) (2/5) (solidRectangle 0.03 0.8)

staff :: Picture
staff = (trebleClef <> DL.foldr (<>) blank (DL.take 5 (DL.map line [0..]))) 
  <> (DL.foldr (<>) blank (DL.take 4 (DL.map tactLine [0..])))

wholeNote :: Picture
wholeNote = lettering "𝅝"

halfNote :: Picture
halfNote = lettering "𝅗𝅥"

quarterNote :: Picture 
quarterNote = lettering "𝅘𝅥"

eightNote :: Picture
eightNote = lettering "𝅘𝅥𝅮"

sixteenNote :: Picture
sixteenNote = lettering "𝅘𝅥𝅯"

wholeRest :: Picture
wholeRest = translated 0 0.172 (scaled 1 0.8 (lettering "𝄻"))

halfRest :: Picture
halfRest = translated 0 0.105 (scaled 1 0.8  (lettering "𝄼"))

quarterRest :: Picture 
quarterRest = translated 0 0.1 (lettering "𝄽")

eightRest :: Picture
eightRest = translated 0 0.25 (lettering "𝄾")

sixteenRest :: Picture
sixteenRest = translated 0 0.15 (lettering "𝄿")

sharpSign :: Picture
sharpSign = scaled 0.6 0.6 (lettering "♯")

flatSign :: Picture 
flatSign = scaled 0.85 0.85 (lettering "♭")

pitchToInteger :: Pitch -> Int
pitchToInteger C = 0
pitchToInteger Cs = 1
pitchToInteger D = 2
pitchToInteger Ds = 3
pitchToInteger E = 4
pitchToInteger F = 5
pitchToInteger Fs = 6
pitchToInteger G = 7
pitchToInteger Gs = 8
pitchToInteger A = 9
pitchToInteger As = 10
pitchToInteger B = 11
  
durToDouble :: Symbol -> Double
durToDouble (Note _ _ Whole) = 6
durToDouble (Note _ _ Half) = 3
durToDouble (Note _ _ Quarter) = 1.5
durToDouble (Note _ _ Eight) = 0.75
durToDouble (Note _ _ Sixteen) = 0.375
durToDouble (Rest Whole) = 6
durToDouble (Rest Half) = 3
durToDouble (Rest Quarter) = 1.5
durToDouble (Rest Eight) = 0.75
durToDouble (Rest Sixteen) = 0.375

symbolToPicture :: Symbol -> Picture
symbolToPicture (Note octave pitch duration) = addLines (Note octave pitch duration)
  <> translated (-1/5) ((pitchToDouble pitch) + ((octaveToInt octave) * 0.7)) (noteDurToPicture duration 
  <> (translated (-1/10) (-1/5) (pitchToSharp pitch)))
symbolToPicture (Rest duration) = translated (-1/5) 0 (restDurToPicture duration)
-- symbolToPicture (note:duration:notes) = wholeNote : symbolToPicture notes


octaveToInt :: Octave -> Double
octaveToInt 0 = -5
octaveToInt 1 = -4
octaveToInt 2 = -3
octaveToInt 3 = -2
octaveToInt 4 = -1
octaveToInt 5 = 0
octaveToInt 6 = 1
octaveToInt 7 = 2
octaveToInt 8 = 3
octaveToInt 9 = 4
octaveToInt _ = 0


pitchToSharp :: Pitch -> Picture
pitchToSharp Cs = sharpSign
pitchToSharp Ds = sharpSign
pitchToSharp Fs = sharpSign
pitchToSharp Gs = sharpSign
pitchToSharp As = sharpSign
pitchToSharp _ = blank

pitchToDouble :: Pitch -> Double
pitchToDouble C = (-1/25)
pitchToDouble Cs = (-1/25)
pitchToDouble D = (-1/25) + (0.1)
pitchToDouble Ds = (-1/25) + (0.1)
pitchToDouble E = (-1/25) + 2 * (0.1)
pitchToDouble F = (-1/25) + 3 * (0.1)
pitchToDouble Fs = (-1/25) + 3 * (0.1)
pitchToDouble G = (-1/25) + 4 * (0.1)
pitchToDouble Gs = (-1/25) + 4 * (0.1)
pitchToDouble A = (-1/25) + 5 * (0.1)
pitchToDouble As = (-1/25) + 5 * (0.1)
pitchToDouble B = (-1/25) + 6 * (0.1)

lowerLine :: Double -> Picture
lowerLine dy = translated 0 ((-1/5) + (-1/5) * dy) (solidRectangle 0.5 0.015)

upperLine :: Double -> Picture
upperLine dy = translated 0 (1 + (1/5) * dy) (solidRectangle 0.5 0.015)

addLines :: Symbol -> Picture
addLines (Note octave pitch duration)
  | octaveToInt octave > 0
    = DL.foldr (<>) blank (DL.take (symbolUpperLinesNum (Note octave pitch duration)) (DL.map upperLine [0..]))

  | otherwise = DL.foldr (<>) blank (DL.take (symbolLowerLinesNum (Note octave pitch duration)) (DL.map lowerLine [0..]))

addLines (Rest duration) = blank

symbolUpperLinesNum :: Symbol -> Int
symbolUpperLinesNum (Note 6 A _) = 1
symbolUpperLinesNum (Note 6 As _) = 1
symbolUpperLinesNum (Note 6 B _) = 1
symbolUpperLinesNum (Note octave pitch duration) = 2 + pitchUpperLine pitch + (round (octaveToInt octave - 2) * 3)
symbolUpperLinesNum (Rest duration) = 0

symbolLowerLinesNum :: Symbol -> Int
symbolLowerLinesNum (Note 5 C _) = 1
symbolLowerLinesNum (Note 5 Cs _) = 1
symbolLowerLinesNum (Note 5 D  _) = 0
symbolLowerLinesNum (Note 5 Ds _) = 0
symbolLowerLinesNum (Note octave pitch _) = pitchLowerLine pitch - (round (octaveToInt octave + 1) * 3)
symbolLowerLinesNum (Rest _) = 0

pitchUpperLine :: Pitch -> Int
pitchUpperLine C = 0
pitchUpperLine Cs = 0
pitchUpperLine D = 0
pitchUpperLine Ds = 0
pitchUpperLine E = 1
pitchUpperLine F = 1
pitchUpperLine Fs = 1
pitchUpperLine G = 2
pitchUpperLine Gs = 2
pitchUpperLine A = 2
pitchUpperLine As = 2
pitchUpperLine B = 3

pitchLowerLine :: Pitch -> Int
pitchLowerLine C = 1
pitchLowerLine Cs = 1
pitchLowerLine B = 1
pitchLowerLine A = 2
pitchLowerLine As = 2
pitchLowerLine G = 2
pitchLowerLine Gs = 2
pitchLowerLine F = 3
pitchLowerLine Fs = 3
pitchLowerLine E = 3
pitchLowerLine D = 4
pitchLowerLine Ds = 4

noteDurToPicture :: Duration -> Picture
noteDurToPicture Whole = wholeNote
noteDurToPicture Half = halfNote
noteDurToPicture Quarter = quarterNote
noteDurToPicture Eight = eightNote
noteDurToPicture Sixteen = sixteenNote

restDurToPicture :: Duration -> Picture
restDurToPicture Whole = translated 0 (6/30) wholeRest
restDurToPicture Half = translated 0 (5/30) halfRest
restDurToPicture Quarter = translated 0 (3/15) quarterRest
restDurToPicture Eight = translated 0 (1/15) eightRest
restDurToPicture Sixteen = translated 0 (2/15) sixteenRest

myComposition :: Composition
myComposition = Composition [Rest Quarter,
                             Note 5 G Quarter,
                             Note 6 C Quarter,
                             Note 6 Ds Quarter,
                             Note 6 D Quarter,
                             Note 6 C Quarter,
                             Note 6 Ds Quarter,
                             Note 6 C Quarter,
                             Note 6 D Quarter,
                             Note 6 C Quarter,
                             Note 5 Gs Quarter,
                             Note 5 As Quarter,
                             Note 5 G Quarter,
                             Rest Quarter,
                             Rest Half
                             ]


durationToInteger :: Duration -> Int
durationToInteger Half = 48
durationToInteger Quarter = 24
durationToInteger Whole = 96
durationToInteger Eight = 12
durationToInteger Sixteen = 6

compositionToMidi :: Composition -> [(Int, Message)]
compositionToMidi (Composition []) = [(0, TrackEnd)]
compositionToMidi (Composition ((Note octave pitch duration):symbols))
    = [(0,  NoteOn 0 (octave*12 + pitchToInteger pitch) 80)
        , (durationToInteger duration, NoteOff 0 (octave*12 + pitchToInteger pitch) 80)] ++ compositionToMidi (Composition symbols)
compositionToMidi (Composition ((Rest duration):symbols))
    = [(0,  NoteOff 0 60 80)
        , (durationToInteger duration, NoteOff 0 60 80)] ++ compositionToMidi (Composition symbols)


myMidi :: Midi
myMidi = Midi { fileType = MultiTrack, 
                timeDiv  = TicksPerBeat 24, 
                tracks   = [compositionToMidi myComposition] 
    }

convertToMidi :: IO ()
convertToMidi = exportFile "my-midi.mid" myMidi

visualise :: IO ()
visualise = drawingOf (translated (-11) (0) (compositionRenderer myComposition 0) 
  <> staffRenderer (ceiling ((durSum (myComposition))/24)))

run :: IO ()
run = visualise