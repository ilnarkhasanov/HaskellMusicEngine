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





data State = State {
  time :: Double,
  posX :: Double,
  posY :: Double,
  line :: Double,
  composition :: Composition
}
drawLine :: State -> Picture
drawLine (State _ px py l _) = translated px (py-l) (colored black (solidRectangle 0.07 1.5))

initState :: Composition -> State
initState comp = State 0 (-11) (0.3) 0 comp

updateState :: State -> State
updateState (State t px py l comp)
  | px > 8 = State (t+1) (-11) py (l+2) comp
  | otherwise = State (t+1) (px + 0.07) py l comp

handleState :: Event -> State -> State
handleState (TimePassing _) state = updateState state
handleState _ state = state


sampleModuleMain :: IO ()
sampleModuleMain = showSamplePicture

myMidi :: Midi
myMidi = Midi { fileType = MultiTrack,
                timeDiv  = TicksPerBeat 24,
                tracks   = [compositionToMidi myComposition]
    }

exportMidi :: String -> Midi -> IO ()
exportMidi path midi = exportFile path midi

visualize :: State -> Picture
visualize (State t px py l composition) = translated (-11) 0 (compositionRenderer composition 0)
  <> staffRenderer (ceiling (durSum composition / 24)) <> drawLine (State t px py l composition)

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
         Right comp -> do
             print comp
             -- exportMidi (filePath ++ ".midi") (compositionToMidi composition)
             activityOf (initState comp) handleState visualize
