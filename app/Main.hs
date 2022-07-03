module Main where

import System.IO

import SampleModule
import TxtCompositionParser


sampleModuleMain :: IO ()
sampleModuleMain = showSamplePicture


txtCompositionParserMain :: IO ()
txtCompositionParserMain = do
    f <- openFile "sample_composition.txt" ReadMode
    content <- hGetContents f
    print (parseComposition content)


main :: IO ()
main = return ()  -- No main function yet
