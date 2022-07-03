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

exportMidi :: String -> Midi -> IO ()
exportMidi path midi = exportFile path midi

visualize :: Composition -> IO ()
visualize composition = drawingOf (translated (-11) 0 (compositionRenderer composition 0) 
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
main = do
    cmdLineArgs <- getArgs
    let filePath = head cmdLineArgs
    f <- openFile filePath ReadMode
    content <- hGetContents f

    case parseComposition content of
         Left err -> do
             putStrLn "Failed to parse the composition. Additional info:"
             print err
         Right composition -> do
             print composition
             -- exportMidi (filePath ++ ".midi") (compositionToMidi composition)
             visualize composition
