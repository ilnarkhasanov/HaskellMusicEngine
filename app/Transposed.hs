module Transposed where

import Common
import CodeWorld

transposeComposition :: Composition -> Int -> Composition
transposeComposition composition 0 = composition
transposeComposition (Composition (symbols)) n
  | n > 0 = transposeComposition (Composition (map noteTransposeUp (symbols))) (n-1)
  | n < 0 = transposeComposition (Composition (map noteTransposeDown (symbols))) (n+1)

noteTransposeUp :: Symbol -> Symbol
noteTransposeUp (Note octave C duration) =(Note octave Cs duration)
noteTransposeUp (Note octave Cs duration) = (Note octave D duration)
noteTransposeUp (Note octave D duration) = (Note octave Ds duration)
noteTransposeUp (Note octave Ds duration) = (Note octave E duration)
noteTransposeUp (Note octave E duration) = (Note octave F duration)
noteTransposeUp (Note octave F duration) = (Note octave Fs duration)
noteTransposeUp (Note octave Fs duration) = (Note octave G duration)
noteTransposeUp (Note octave G duration) = (Note octave Gs duration)
noteTransposeUp (Note octave Gs duration) = (Note octave A duration)
noteTransposeUp (Note octave A duration) = (Note octave As duration)
noteTransposeUp (Note octave As duration) = (Note octave B duration)
noteTransposeUp (Note octave B duration) = (Note (octave + 1) C duration)
noteTransposeUp (Rest duration) = (Rest duration)


noteTransposeDown :: Symbol -> Symbol
noteTransposeDown (Note octave C duration) =(Note (octave - 1) B duration)
noteTransposeDown (Note octave B duration) = (Note octave As duration)
noteTransposeDown (Note octave As duration) = (Note octave A duration)
noteTransposeDown (Note octave A duration) = (Note octave Gs duration)
noteTransposeDown (Note octave Gs duration) = (Note octave G duration)
noteTransposeDown (Note octave G duration) = (Note octave Fs duration)
noteTransposeDown (Note octave Fs duration) = (Note octave F duration)
noteTransposeDown (Note octave F duration) = (Note octave E duration)
noteTransposeDown (Note octave E duration) = (Note octave Ds duration)
noteTransposeDown (Note octave Ds duration) = (Note octave D duration)
noteTransposeDown (Note octave D duration) = (Note octave Cs duration)
noteTransposeDown (Note octave Cs duration) = (Note octave C duration)
noteTransposeDown (Rest duration) = (Rest duration)