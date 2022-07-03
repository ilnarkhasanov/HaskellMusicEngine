module Renderer.Renderer where

import CodeWorld

import Common
import Sprites
import Renderer.ExtraLines
import Renderer.Translators

-- This function renders n staffs on the screen (n depends on the length of the melody)
staffRenderer :: Int -> Picture
staffRenderer 0 = blank
staffRenderer n = staff <> translated 0 (-2) (staffRenderer (n-1))

-- This function this gets the composition in Melody and renders the picture correspondingly
compositionRenderer :: Composition -> Double -> Picture
compositionRenderer (Composition []) _ = blank
compositionRenderer (Composition (symbol:symbols)) n = translated (n - (24 * getRowNumber n * (-1 / 2)))
  (getRowNumber n) (symbolToPicture symbol)
  <> compositionRenderer (Composition symbols) (n + symbolLength symbol)

-- This function gets the number of the row according to the x-axis coordinate
getRowNumber :: Double -> Double
getRowNumber x = -2 * fromIntegral (floor (x/24))

-- This function counts the total durarion of the composition
durSum :: Composition -> Double
durSum (Composition composition) = sum (map symbolLength composition)
-- Old:
-- durSum (Composition []) = 0
-- durSum (Composition (symbol:symbols)) = symbolLength symbol + durSum (Composition symbols)

-- This function renders symbol to the picture
symbolToPicture :: Symbol -> Picture
symbolToPicture (Note octave pitch duration) = addLines (Note octave pitch duration)
  <> translated (-1/5) (pitchHeight pitch + (fromIntegral (octave - 5) * 0.7)) (noteDurToPicture duration
  <> translated (-1/10) (-1/5) (pitchSharped pitch))
symbolToPicture (Rest duration) = translated (-1/5) 0 (restDurToPicture duration)


-- This function checks if the pitch is sharped, then sharp sign appends
pitchSharped :: Pitch -> Picture
pitchSharped Cs = sharpSign
pitchSharped Ds = sharpSign
pitchSharped Fs = sharpSign
pitchSharped Gs = sharpSign
pitchSharped As = sharpSign
pitchSharped _ = blank

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
