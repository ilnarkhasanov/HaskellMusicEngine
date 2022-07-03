module Renderer.ExtraLines where

import Data.List as DL
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
addLines (Note octave pitch duration)
  | (octave - 5)> 0
    = DL.foldr (<>) blank (DL.take (symbolUpperLinesNum (Note octave pitch duration)) (DL.map upperLine [0..]))
  | otherwise = DL.foldr (<>) blank (DL.take (symbolLowerLinesNum (Note octave pitch duration)) (DL.map lowerLine [0..]))
addLines (Rest duration) = blank

-- This function returns the number of extra high lines
symbolUpperLinesNum :: Symbol -> Int
symbolUpperLinesNum (Note 6 A _) = 1
symbolUpperLinesNum (Note 6 As _) = 1
symbolUpperLinesNum (Note 6 B _) = 1
symbolUpperLinesNum (Note octave pitch duration) = 2 + pitchUpperLine pitch + ((octave - 7) * 3)
symbolUpperLinesNum (Rest duration) = 0

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
