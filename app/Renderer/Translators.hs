module Renderer.Translators where

import Common

-- This is the function that corresponds each pitch to some integer
-- We will need it for the exporting the sound
pitchToInt :: Pitch -> Int
pitchToInt C = 0
pitchToInt Cs = 1
pitchToInt D = 2
pitchToInt Ds = 3
pitchToInt E = 4
pitchToInt F = 5
pitchToInt Fs = 6
pitchToInt G = 7
pitchToInt Gs = 8
pitchToInt A = 9
pitchToInt As = 10
pitchToInt B = 11

-- This is the function that corresponds each symbol to double number
symbolLength :: Symbol -> Double
symbolLength (Note _ _ Whole) = 6
symbolLength (Note _ _ Half) = 3
symbolLength (Note _ _ Quarter) = 1.5
symbolLength (Note _ _ Eight) = 0.75
symbolLength (Note _ _ Sixteen) = 0.375
symbolLength (Rest Whole) = 6
symbolLength (Rest Half) = 3
symbolLength (Rest Quarter) = 1.5
symbolLength (Rest Eight) = 0.75
symbolLength (Rest Sixteen) = 0.375

-- This function returns the height of the note in the staff
pitchHeight :: Pitch -> Double
pitchHeight C = -1/25
pitchHeight Cs = -1/25
pitchHeight D = (-1/25) + 0.1
pitchHeight Ds = (-1/25) + 0.1
pitchHeight E = (-1/25) + 2 * 0.1
pitchHeight F = (-1/25) + 3 * 0.1
pitchHeight Fs = (-1/25) + 3 * 0.1
pitchHeight G = (-1/25) + 4 * 0.1
pitchHeight Gs = (-1/25) + 4 * 0.1
pitchHeight A = (-1/25) + 5 * 0.1
pitchHeight As = (-1/25) + 5 * 0.1
pitchHeight B = (-1/25) + 6 * 0.1
