module Main where

import Codec.Midi

import SampleModule
import Common
import MelodyMidi
import Renderer.Renderer
import Transposed

import CodeWorld

sampleModuleMain :: IO ()
sampleModuleMain = showSamplePicture

myMidi :: Midi
myMidi = Midi { fileType = MultiTrack, 
                timeDiv  = TicksPerBeat 24, 
                tracks   = [compositionToMidi myComposition] 
    }

convertToMidi :: IO ()
convertToMidi = exportFile "my-midi.mid" myMidi

visualise :: Composition -> IO ()
visualise composition = drawingOf (translated (-11) 0 (compositionRenderer composition 0) 
  <> staffRenderer (ceiling (durSum composition / 24)))

-- This is our composition
myComposition :: Composition
myComposition = Composition [Rest Quarter,
                      Note 5 G Quarter,
                      Note 6 C Quarter,
                      Note 6 Ds Quarter,
                      Note 6 D Quarter,
                      Note 6 C Quarter,
                      Note 6 Ds Quarter,
                      Note 6 C Quarter,
                      Note 6 D Quarter,
                      Note 6 C Quarter,
                      Note 5 Gs Quarter,
                      Note 5 As Quarter,
                      Note 5 G Half,
                      Rest Half,
                      Rest Quarter,
                      Note 5 G Quarter,
                      Note 6 C Quarter,
                      Note 6 Ds Quarter,
                      Note 6 D Quarter,
                      Note 6 C Quarter,
                      Note 6 Ds Quarter,
                      Note 6 C Quarter,
                      Note 6 D Quarter,
                      Note 6 C Quarter,
                      Note 5 G Quarter,
                      Note 5 Fs Quarter,
                      Note 5 F Half,
                      Rest Half,
                      Rest Quarter,
                      Note 5 F Quarter,
                      Note 5 Gs Quarter,
                      Note 5 B Quarter,
                      Note 6 D Whole,
                      Rest Quarter,
                      Note 5 F Quarter,
                      Note 5 Gs Quarter,
                      Note 5 B Quarter,
                      Note 6 C Whole]

main :: IO ()
main = return ()  -- No main function yet
