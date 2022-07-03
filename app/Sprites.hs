module Sprites where

import CodeWorld

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

-- This is the sprite for the staff
staff :: Picture
staff = (trebleClef <> DL.foldr (<>) blank (DL.take 5 (DL.map line [0..]))) 
  <> (DL.foldr (<>) blank (DL.take 4 (DL.map tactLine [0..])))
  
-- This is the sprite for the treble clef
trebleClef :: Picture
trebleClef = scaled 1 1.2 (translated (-12) 0.27 (lettering "𝄞"))

-- This is the sprite for the line
line :: Double -> Picture
line dy = translated 0 (dy/5) (solidRectangle 25 0.015)

-- This is the sprite for the tact line
tactLine :: Double -> Picture
tactLine dx = translated (12.5 - dx*6) (2/5) (solidRectangle 0.03 0.8)