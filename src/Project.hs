{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module Project where

-- We will use these libraries for the project
import CodeWorld
import Data.List as DL
import Data.Maybe
import Codec.Midi

run :: IO ()
run = convertToMidi

convertToMidi :: IO ()
convertToMidi = exportFile "my-midi.mid" myMidi

visualise :: IO ()
visualise = drawingOf (translated (-11) (0) (compositionRenderer myComposition 0) 
  <> staffRenderer (ceiling ((durSum (myComposition))/24)))

-- This is our composition
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
                      Note 5 G Half,
                      Rest Half,
                      Rest Quarter,
                      Note 5 G Quarter,
                      Note 6 C Quarter,
                      Note 6 Ds Quarter,
                      Note 6 D Quarter,
                      Note 6 C Quarter,
                      Note 6 Ds Quarter,
                      Note 6 C Quarter,
                      Note 6 D Quarter,
                      Note 6 C Quarter,
                      Note 5 G Quarter,
                      Note 5 Fs Quarter,
                      Note 5 F Half,
                      Rest Half,
                      Rest Quarter,
                      Note 5 F Quarter,
                      Note 5 Gs Quarter,
                      Note 5 B Quarter,
                      Note 6 D Whole,
                      Rest Quarter,
                      Note 5 F Quarter,
                      Note 5 Gs Quarter,
                      Note 5 B Quarter,
                      Note 6 C Whole]

-- This is the data types for the music representation that is called Melody
type Octave = Int
data Pitch = C | Cs | D | Ds | E | F | Fs | G | Gs | A | As | B
data Duration = Whole | Half | Quarter | Eight | Sixteen
data Symbol = Note Octave Pitch Duration | Rest Duration
data Composition = Composition [Symbol]

-- This function renders n staffs on the screen (n depends on the length of the melody)
staffRenderer :: Int -> Picture
staffRenderer 0 = blank
staffRenderer n = staff <> (translated 0 (-2) (staffRenderer (n-1)))

-- This function this gets the composition in Melody and renders the picture correspondingly
compositionRenderer :: Composition -> Double -> Picture
compositionRenderer (Composition []) _ = blank
compositionRenderer (Composition (symbol:symbols)) n = translated (n - (24*(getRowNumber n)*(-1/2))) 
  (getRowNumber n) (symbolToPicture symbol)
  <> compositionRenderer (Composition symbols) (n+durToDouble symbol)
  
-- This function gets the number of the row according to the x-axis coordinate
getRowNumber :: Double -> Double
getRowNumber x = (-2 * (fromIntegral (floor (x/24))))

-- This function counts the total durarion of the composition
durSum :: Composition -> Double
durSum (Composition []) = 0
durSum (Composition (symbol:symbols)) = durToDouble symbol + durSum (Composition symbols)

-- This is the sprite for the treble clef
trebleClef :: Picture
trebleClef = scaled 1 1.2 (translated (-12) 0.27 (lettering "𝄞"))

-- This is the sprite for the line
line :: Double -> Picture
line dy = translated 0 (dy/5) (solidRectangle 25 0.015)

-- This is the sprite for the tact line
tactLine :: Double -> Picture
tactLine dx = translated (12.5 - dx*6) (2/5) (solidRectangle 0.03 0.8)

-- This is the sprite for the staff
staff :: Picture
staff = (trebleClef <> DL.foldr (<>) blank (DL.take 5 (DL.map line [0..]))) 
  <> (DL.foldr (<>) blank (DL.take 4 (DL.map tactLine [0..])))

-- This is the sprite for the whole note
wholeNote :: Picture
wholeNote = lettering "𝅝"

-- This is the sprite for the half note
halfNote :: Picture
halfNote = lettering "𝅗𝅥"

-- This is the sprite for the quarter note
quarterNote :: Picture 
quarterNote = lettering "𝅘𝅥"

-- This is the sprite for the eight note
eightNote :: Picture
eightNote = lettering "𝅘𝅥𝅮"

-- This is the sprite for the sixteen note
sixteenNote :: Picture
sixteenNote = lettering "𝅘𝅥𝅯"

-- This is the sprite for the whole rest
wholeRest :: Picture
wholeRest = translated 0 0.172 (scaled 1 0.8 (lettering "𝄻"))

-- This is the sprite for the half rest
halfRest :: Picture
halfRest = translated 0 0.105 (scaled 1 0.8  (lettering "𝄼"))

-- This is the sprite for the quarter rest
quarterRest :: Picture 
quarterRest = translated 0 0.1 (lettering "𝄽")

-- This is the sprite for the eight rest
eightRest :: Picture
eightRest = translated 0 0.25 (lettering "𝄾")

-- This is the sprite for the sixteen rest
sixteenRest :: Picture
sixteenRest = translated 0 0.15 (lettering "𝄿")

-- This is the sprite for the sharp sign
sharpSign :: Picture
sharpSign = scaled 0.6 0.6 (lettering "♯")

-- This is the sprite for the flat sigh
flatSign :: Picture 
flatSign = scaled 0.85 0.85 (lettering "♭")

-- This is the function that corresponds each pitch to some integer
-- We will need it for the exporting the sound
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

-- This is the function that corresponds each symbol to soe double number
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

-- This function renders symbol to the picture
symbolToPicture :: Symbol -> Picture
symbolToPicture (Note octave pitch duration) = addLines (Note octave pitch duration)
  <> translated (-1/5) ((pitchToDouble pitch) + ((octaveToDouble octave) * 0.7)) (noteDurToPicture duration 
  <> (translated (-1/10) (-1/5) (pitchToSharp pitch)))
symbolToPicture (Rest duration) = translated (-1/5) 0 (restDurToPicture duration)

-- This function shifts each to (-5)
octaveToDouble :: Octave -> Double
octaveToDouble 0 = -5
octaveToDouble 1 = -4
octaveToDouble 2 = -3
octaveToDouble 3 = -2
octaveToDouble 4 = -1
octaveToDouble 5 = 0
octaveToDouble 6 = 1
octaveToDouble 7 = 2
octaveToDouble 8 = 3
octaveToDouble 9 = 4
octaveToDouble _ = 0

-- This function checks if the pitch is sharped, then sharp sign appends
pitchToSharp :: Pitch -> Picture
pitchToSharp Cs = sharpSign
pitchToSharp Ds = sharpSign
pitchToSharp Fs = sharpSign
pitchToSharp Gs = sharpSign
pitchToSharp As = sharpSign
pitchToSharp _ = blank

-- This function returns the height of the note in the staff
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

-- This is the spring for the extra line if the symbol is too low
lowerLine :: Double -> Picture
lowerLine dy = translated 0 ((-1/5) + (-1/5) * dy) (solidRectangle 0.5 0.015)

-- This is the spring for the extra line if the symbol is too high
upperLine :: Double -> Picture
upperLine dy = translated 0 (1 + (1/5) * dy) (solidRectangle 0.5 0.015)

-- This function adds the extra lines to the picture
addLines :: Symbol -> Picture
addLines (Note octave pitch duration)
  | octaveToDouble octave > 0
    = DL.foldr (<>) blank (DL.take (symbolUpperLinesNum (Note octave pitch duration)) (DL.map upperLine [0..]))
  | otherwise = DL.foldr (<>) blank (DL.take (symbolLowerLinesNum (Note octave pitch duration)) (DL.map lowerLine [0..]))
addLines (Rest duration) = blank

-- This function returns the number of extra high lines
symbolUpperLinesNum :: Symbol -> Int
symbolUpperLinesNum (Note 6 A _) = 1
symbolUpperLinesNum (Note 6 As _) = 1
symbolUpperLinesNum (Note 6 B _) = 1
symbolUpperLinesNum (Note octave pitch duration) = 2 + pitchUpperLine pitch + (round (octaveToDouble octave - 2) * 3)
symbolUpperLinesNum (Rest duration) = 0

-- This function returns the number of extra low lines
symbolLowerLinesNum :: Symbol -> Int
symbolLowerLinesNum (Note 5 C _) = 1
symbolLowerLinesNum (Note 5 Cs _) = 1
symbolLowerLinesNum (Note 5 D  _) = 0
symbolLowerLinesNum (Note 5 Ds _) = 0
symbolLowerLinesNum (Note octave pitch _) = pitchLowerLine pitch - (round (octaveToDouble octave + 1) * 3)
symbolLowerLinesNum (Rest _) = 0

-- This function returns the number of required lines for each high pitch
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

-- This function returns the number of required lines for each low pitch
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

-- This function returns the picture for each duration of the note
noteDurToPicture :: Duration -> Picture
noteDurToPicture Whole = wholeNote
noteDurToPicture Half = halfNote
noteDurToPicture Quarter = quarterNote
noteDurToPicture Eight = eightNote
noteDurToPicture Sixteen = sixteenNote

-- This functions returns the picture for each duration of the rest
restDurToPicture :: Duration -> Picture
restDurToPicture Whole = translated 0 (6/30) wholeRest
restDurToPicture Half = translated 0 (5/30) halfRest
restDurToPicture Quarter = translated 0 (3/15) quarterRest
restDurToPicture Eight = translated 0 (1/15) eightRest
restDurToPicture Sixteen = translated 0 (2/15) sixteenRest

-- This function corresponds each duration to integer
-- We will use it for the exporting the sound
durationToInteger :: Duration -> Int
durationToInteger Half = 48
durationToInteger Quarter = 24
durationToInteger Whole = 96
durationToInteger Eight = 12
durationToInteger Sixteen = 6

-- This function translates the composition from Melody to Midi
compositionToMidi :: Composition -> [(Int, Message)]
compositionToMidi (Composition []) = [(0, TrackEnd)]
compositionToMidi (Composition ((Note octave pitch duration):symbols))
    = [(0,  NoteOn 0 (octave*12 + pitchToInteger pitch) 80)
        , (durationToInteger duration, NoteOff 0 (octave*12 + pitchToInteger pitch) 80)] ++ compositionToMidi (Composition symbols)
compositionToMidi (Composition ((Rest duration):symbols))
    = [(0,  NoteOff 0 60 80)
        , (durationToInteger duration, NoteOff 0 60 80)] ++ compositionToMidi (Composition symbols)

-- This function gets the composition in Midi and other settings and exports the sound file
myMidi :: Midi
myMidi = Midi { fileType = MultiTrack, 
                timeDiv  = TicksPerBeat 24, 
                tracks   = [compositionToMidi myComposition] 
    }