-- | A SampleModule to show the project structure.
-- | To be removed.
module SampleModule where

import CodeWorld

-- | Sample picture: a green circle.
samplePicture :: Picture
samplePicture = colored green $ solidCircle 1

-- | A sample program that shows the picture.
showSamplePicture :: IO ()
showSamplePicture = drawingOf samplePicture
