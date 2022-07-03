module MelodyMidi where

import Common
import Codec.Midi
import Renderer.Translators

-- This function corresponds each duration to integer
-- We will use it for the exporting the sound
durationToInt :: Duration -> Int
durationToInt Half = 48
durationToInt Quarter = 24
durationToInt Whole = 96
durationToInt Eight = 12
durationToInt Sixteen = 6

-- This function translates the composition from Melody to Midi
compositionToMidi :: Composition -> [(Int, Message)]
compositionToMidi (Composition []) = [(0, TrackEnd)]
compositionToMidi (Composition ((Note octave pitch duration):symbols))
    = [(0,  NoteOn 0 (octave*12 + pitchToInt pitch) 80)
        , (durationToInt duration, NoteOff 0 (octave*12 + pitchToInt pitch) 80)] ++ compositionToMidi (Composition symbols)
compositionToMidi (Composition ((Rest duration):symbols))
    = [(0,  NoteOff 0 60 80)
        , (durationToInt duration, NoteOff 0 60 80)] ++ compositionToMidi (Composition symbols)
