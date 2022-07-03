module Renderer.ExtraLines where

import CodeWorld

import Common

-- This is the spring for the extra line if the symbol is too low
lowerLine :: Double -> Picture
lowerLine dy = translated 0 ((-1/5) + (-1/5) * dy) (solidRectangle 0.5 0.015)

-- This is the spring for the extra line if the symbol is too high
upperLine :: Double -> Picture
upperLine dy = translated 0 (1 + (1/5) * dy) (solidRectangle 0.5 0.015)

-- This function adds the extra lines to the picture
addLines :: Symbol -> Picture
addLines (Note octave pitch duration) = foldMap lineFunc [0..end]
    where
        lineFunc = if (octave - 5) > 0 then upperLine else lowerLine
        end = fromIntegral (symbolLowerLinesNum (Note octave pitch duration))
addLines (Rest _) = blank

-- This function returns the number of extra high lines
symbolUpperLinesNum :: Symbol -> Int
symbolUpperLinesNum (Note 6 A _) = 1
symbolUpperLinesNum (Note 6 As _) = 1
symbolUpperLinesNum (Note 6 B _) = 1
symbolUpperLinesNum (Note octave pitch _) = 2 + pitchUpperLine pitch + ((octave - 7) * 3)
symbolUpperLinesNum (Rest _) = 0

-- This function returns the number of extra low lines
symbolLowerLinesNum :: Symbol -> Int
symbolLowerLinesNum (Note 5 C _) = 1
symbolLowerLinesNum (Note 5 Cs _) = 1
symbolLowerLinesNum (Note 5 D  _) = 0
symbolLowerLinesNum (Note 5 Ds _) = 0
symbolLowerLinesNum (Note octave pitch _) = pitchLowerLine pitch - ((octave - 4) * 3)
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
