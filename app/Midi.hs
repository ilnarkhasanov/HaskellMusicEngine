module Midi where

import Common

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

myMidi :: Midi
myMidi = Midi { fileType = MultiTrack, 
                timeDiv  = TicksPerBeat 24, 
                tracks   = [compositionToMidi myComposition] 
    }