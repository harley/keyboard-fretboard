module Helper where

import Euterpea

-- For debugging
import System.IO.Unsafe
debug s x = unsafePerformIO (print s) `seq` x
debugMaybe s x = if s /= "" then debug s x else x

-- For sound testing
fj1, fj2, fj3, fj4 :: Music Pitch
fj0 = c 4 qn :+: c 4 qn :+: c 4 qn
fj1 = c 4 qn :+: d 4 qn :+: e 4 qn :+: c 4 qn
fj2 = e 4 qn :+: f 4 qn :+: g 4 hn
fj3 = g 4 en :+: a 4 en :+: g 4 en :+: f 4 en :+: e 4 qn :+: c 4 qn
fj4 = c 4 qn :+: g 3 qn :+: c 4 hn

--Then define a function to play a phrase twice:

two :: Music a -> Music a
two m = m :+: m

--Then put the whole meolody together:

fj :: Music Pitch
fj  = two fj1 :+: two fj2 :+: two fj3 :+: two fj4

--And finally put it into rounds:

fjfj :: Music Pitch
fjfj = (Modify (Tempo 4)
          (Modify (Instrument AcousticGrandPiano) fj))

