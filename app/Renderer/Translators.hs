module Translators where

import CodeWorld
import Common

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

