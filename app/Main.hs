module Main where

import Codec.Midi

import SampleModule
import Common
import MelodyMidi
import Renderer.Renderer
import System.IO

import SampleModule
import TxtCompositionParser

import CodeWorld
import System.Environment (getArgs)

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

txtCompositionParserMain :: String -> IO ()
txtCompositionParserMain path = do
    f <- openFile path ReadMode
    content <- hGetContents f
    print (parseComposition content)


main :: IO ()
main = do
  args <- getArgs
  txtCompositionParserMain (head args)  -- No main function yet
