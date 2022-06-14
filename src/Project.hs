{-# LANGUAGE OverloadedStrings #-}
module Project where

import           CodeWorld

line :: Double -> Picture
line dy = translated 0 (dy/10) (solidRectangle 15 0.03)

staff :: Picture
staff = foldr (<>) blank (take 5 (map line [0..]))

-- >>> 

run :: IO ()
run = drawingOf (staff)
