-- | Common types and values that will be used by other modules.
module Common where

-- This is the data types for the music representation that is called Melody
type Octave = Int
data Pitch = C | Cs | D | Ds | E | F | Fs | G | Gs | A | As | B
           deriving Show
data Duration = Whole | Half | Quarter | Eight | Sixteen
              deriving (Show, Read)
data Symbol = Note Octave Pitch Duration | Rest Duration
            deriving Show
newtype Composition = Composition [Symbol]
                    deriving Show
