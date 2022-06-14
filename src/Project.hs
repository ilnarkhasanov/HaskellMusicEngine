{-# LANGUAGE OverloadedStrings #-}
module Project where

import           CodeWorld
import Codec.Midi

line :: Double -> Picture
line dy = translated 0 (dy/7) (solidRectangle 15 0.03)

trebleClef :: Picture
trebleClef = translated (-7) 0.27 (lettering ("𝄞"))

staff :: Picture
staff = trebleClef <> foldr (<>) blank (take 5 (map line [0..]))


track0 = [(0,  NoteOn 0 60 80),
          (24, NoteOff 0 60 0),
          (0,  TrackEnd)]

track1 = [(0,  NoteOn 0 64 80),
          (24, NoteOn 0 64 0),
          (0,  TrackEnd)]

myMidi = Midi { fileType = MultiTrack, 
                timeDiv  = TicksPerBeat 24, 
                tracks   = [track0, track1] }

run :: IO ()
run = exportFile "my-midi.mid" myMidi
