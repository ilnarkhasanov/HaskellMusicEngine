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

wholeNote :: Picture
wholeNote = lettering ("𝅝")

halfNote :: Picture
halfNote = lettering ("𝅗𝅥")

fourNote :: Picture 
fourNote = lettering ("♩")

eightNote :: Picture
eightNote = lettering ("♪")

sixteenNote :: Picture
sixteenNote = lettering ("𝅘𝅥𝅯")

wholeRest :: Picture
wholeRest = lettering ("𝄻")

halfRest :: Picture
halfRest = lettering ("𝄼")

fourRest :: Picture 
fourRest = lettering ("𝄽")

eightRest :: Picture
eightRest = lettering ("𝄾")

sixteenRest :: Picture
sixteenRest = lettering ("𝄿")

sharpSign :: Picture
sharpSign = lettering ("♯")

flatSign :: Picture 
flatSign = lettering ("♭")

track0 = [(0,  NoteOn 0 60 80),
          (24, NoteOff 0 60 0),
          (0,  TrackEnd)]

track1 = [(0,  NoteOn 0 64 80),
          (24, NoteOn 0 64 0),
          (0,  TrackEnd)]

myMidi = Midi { fileType = MultiTrack, 
                timeDiv  = TicksPerBeat 24, 
                tracks   = [notes] }

notes :: [(Int, Message)]
notes = [(0,  NoteOn 0 64 80),
          (24, NoteOff 0 64 0),
          (0,  NoteOn 0 60 80),
          (24, NoteOff 0 60 0),
          (0,  NoteOn 0 64 80),
          (24, NoteOff 0 64 0),
          (0,  NoteOn 0 64 80),
          (24, NoteOff 0 64 0),
          (0,  NoteOn 0 64 80),
          (24, NoteOff 0 64 0),
          (0,  NoteOn 0 64 80),
          (24, NoteOff 0 64 0),
          (0,  NoteOn 0 64 80),
          (24, NoteOff 0 64 0),
          (0,  NoteOn 0 64 80),
          (24, NoteOff 0 64 0),
          (0, TrackEnd)]

noteToPicture :: [(Int, Message)] -> [Picture]
noteToPicture [(0, TrackEnd)] = [blank]
noteToPicture (note:_:notes) = wholeNote : noteToPicture notes

noteRenderer :: Double -> [Picture] -> Picture
noteRenderer _ [] = blank
noteRenderer n (picture:pictures) = translated n 0 picture <> noteRenderer (n+1) pictures

-- updateWorld :: Double -> Mode -> Mode
-- updateWorld _dt = id

-- handleWorld :: Event -> Mode -> Mode
-- handleWorld (TimePassing dt) prev = updateWorld dt prev
-- handleWorld (KeyPress "Up") prev = applyAction (Just Up) isEqual elevator prev
-- handleWorld (KeyPress "Down") prev = applyAction (Just Down) isEqual elevator prev
-- handleWorld (KeyPress " ") prev = applyAction (Just Stop) isEqual elevator prev
-- handleWorld _anyEvent coords = coords

run :: IO ()
-- run = drawingOf (noteRenderer 0 (noteToPicture notes))
run = drawingOf (staff <> translated (-6) 0 (noteRenderer 0 (noteToPicture notes)))
-- run = exportFile "my-midi.mid" myMidi
