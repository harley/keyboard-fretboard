{-# OPTIONS -fglasgow-exts  #-} -- needed for mdo notation
--Make sure the path to Euterpea code is correct in .ghci file

module InstrumentBase where

import Euterpea hiding (transpose)       -- for testMidi and MidiMessage datatype
import qualified Codec.Midi as Midi
import Data.Maybe                       -- fromJust
import Euterpea.UI.SOE                   -- SOE Graphic library
import qualified Data.List as List      -- List.elemIndex

import Euterpea.UI
-- the following three modules are hidden so -i<path_to_Euterpea_src> is required
import Euterpea.UI.Signal
import Euterpea.UI.UIMonad
import Euterpea.UI.Widget

import Helper

type PlayerUI     = EMM -> UI EMM
type EMM          = EventS [MidiMessage]

data KeyData = KeyData { pressed :: Maybe Bool, notation :: Maybe String, offset :: Int } deriving (Show, Eq)
data KeyBool  = KeyBool { keypad:: Bool,
                          mouse :: Bool,
                          song  :: Bool} deriving (Show,Eq)
data InstrumentData = InstrumentData {showNotation::Bool, keyPairs :: Maybe [(AbsPitch, Bool)], transpose :: AbsPitch, pedal :: Bool}

isKeyDown (KeyBool False False False) = False
isKeyDown _ = True

isKeyPlay (KeyBool False False _) = False
isKeyPlay _ = True

vel = 127

{- playerBegin and playerEnd are used in my obscure syntax, as in this example:
      devId <- selectOutput
      msg   <- playerBegin >>=
                 (selectSong [fjfj]) >>=
                 addEcho >>=
                 (selectInstrument 0) >>=
                 keyboard >>=
                 keyboard >>=
                 playerEnd
      midiOut devId msg
-}

playerBegin :: UI EMM
playerBegin = return (lift0 Nothing)

playerEnd :: PlayerUI
playerEnd msg = return msg

nullEMM = constant Nothing
defaultInstrumentData = constant $ InstrumentData False Nothing 0 False

-- *****************************************************************************
--   NOTATION widget
-- *****************************************************************************
addNotation :: Char -> Signal InstrumentData -> UI (Signal InstrumentData)
addNotation c insD = do
  nota <- checkboxc "(N)otation" c False
  return $ lift2 update insD nota
 where
  update i b = i{showNotation = b}

-- *****************************************************************************
--   TRANSPOSE widget
-- *****************************************************************************
addTranspose :: Signal InstrumentData -> UI (Signal InstrumentData)
addTranspose insD = do
  tp <- withDisplay "  Transpose       " $ hiSlider 1 (-6,6) 0
  return $ lift2 instrUpdateTrans insD tp

-- *****************************************************************************
--   PEDAL widget
-- *****************************************************************************
addPedal :: Char -> Signal InstrumentData -> UI (Signal InstrumentData)
addPedal c insD = do
  pedal <- checkboxc "(P)edal" c False
  return $ lift2 instrUpdatePedal insD pedal

-- *****************************************************************************
--   ECHO widget
-- *****************************************************************************
addEcho :: PlayerUI
addEcho msg = do
    t <- time
    (isEcho, r, f) <- leftRight $ do
        a <- checkbox "(E)cho" False
        b <- title "Decay rate" $ withDisplay "" (hSlider (0, 0.9) 0.8)
        c <- title "Echoing frequency" $ withDisplay "" (hSlider (1,10) 7)
        return (a,b,c)
    return (ifS isEcho (echof t r f msg) msg)

echof :: Signal Time -> Signal Double -> Signal Double -> EventS [MidiMessage] -> EventS [MidiMessage]
echof t r f m = m'
 where
  m' = m |+| s
  s = delayt t (1.0 / f) (snapshot m' r =>> k)
  k (ns,r) = mapMaybe (decay 0.1 r) ns
  decay :: Time -> Double -> MidiMessage -> Maybe MidiMessage
  decay dur r m =
    let f c k v d = if v > 0
                    then Just (ANote c k (truncate (fromIntegral v * r)) d)
                    else Nothing
    in case m of
        ANote c k v d -> f c k v d
        Std (Midi.NoteOn c k v) -> f c k v dur
        _ -> Nothing

-- *****************************************************************************
--   SELECT INSTRUMENT
-- *****************************************************************************
selectInstrument :: Midi.Channel -> Int -> PlayerUI
selectInstrument chn i msg = title "Instrument: " $ do
	instrNum <- rightLeft $ do
					val <- hiSlider 1 (0,127) i
					display' (lift1 (toEnum::Int->InstrumentName) val)
					return val
	return $ (msg |+| (unique instrNum =>> (\i -> [Std $ Midi.ProgramChange chn i])))

-- *****************************************************************************
--   SELCT SONG
-- *****************************************************************************
selectSong :: [Music Pitch] -> UI EMM
selectSong songList = do
  t <- time
  songChoice <- pickSong (map (("Song number " ++) . show) $ take (length songList) [1..])
  let (songs, midi_msg) = songToPair t $ songChoice =>> (songList !!)
--  display' songs
  return midi_msg

pickSong :: [String] -> UI (EventS Int)
pickSong []   = title "No songs imported" $ return $ constant Nothing
pickSong list = title "Avalaible songs " $ leftRight $ do
   i <- radio list 0
   x <- button "Play"
   return $ snapshot_ (edge x) i

songToPair :: Signal Time -> EventS (Music Pitch) -> (EventS [(Int,Bool)], EventS [MidiMessage])
songToPair t mp = (msgToKey msg, msg =>> (map Std))
  where msg = songToMsg t mp

msgToKey :: EventS [Midi.Message] -> EventS [(Int,Bool)]
msgToKey (Signal em) = Signal (f em)
  where
   f ~(m:ms) = (case m of
     Nothing -> Nothing
     Just msglist -> Just (mapMaybe keyCheck msglist)):(f ms)
   keyCheck :: Midi.Message -> Maybe (Int,Bool)
   keyCheck m = case m of
    Midi.NoteOn _ k _ -> Just (k,True)
    Midi.NoteOff _ k _ -> Just (k,False)
    _ -> Nothing

songToMsg :: Signal Time -> EventS (Music Pitch) -> EventS [Midi.Message]
songToMsg t mp = trackToMsgFlat t (mp =>> musicToTrack)

musicToTrack :: Music Pitch -> Midi.Track Time -- Track Time is same as [(Time, Midi.Messasge)]
musicToTrack m = map (\(i,m) -> (fromIntegral i/96,m)) $ Midi.toAbsTime $ head $ Midi.tracks (testMidi m)

-- This one is used so that when a NoteOn and NoteOff are sent for the same note,
-- they arrive at different time
trackToMsgFlat :: Signal Time -> EventS (Midi.Track Time) -> EventS [Midi.Message]
trackToMsgFlat time trks = msgs
  where
    (msgs, states) = unzipS $ (lift3 aux) time trks (initS [] states)
    aux t (Just tr) [] = peel t (map (\(t0,m) -> (t0 + t, m)) tr)
    aux t _ trk = peel t trk
    peel _ []                        = (Nothing, [])
    peel t ((t0,m0):trk) | t0 <= t   = (Just [m0], trk)
                         | otherwise = (Nothing, (t0,m0):trk)

-- Not used anymore, replaced by trackToMsgFlat. see reason above
trackToMsg :: Signal Time -> EventS (Midi.Track Time) -> EventS [Midi.Message]
trackToMsg time trks = msgs
  where
    (msgs, states) = unzipS $ (lift3 aux) time trks (initS [] states)
    aux t (Just tr) [] = peel [] t (map (\(t0,m) -> (t0 + t, m)) tr)
    aux t _ trk = peel [] t trk
    peel s _ []                        = (send s, [])
    peel s t ((t0,m0):trk) | t0 <= t  = peel (m0:s) t trk
                           | otherwise = (send s, (t0,m0):trk)
    send [] = Nothing
    send s  = Just (reverse s)

-- *****************************************************************************
--   AUX FUNCTIONS
-- *****************************************************************************
instrUpdateEMM :: Signal InstrumentData -> EMM -> Signal InstrumentData
instrUpdateEMM i emm = lift2 up i (emm =>> mmToPair)
  where
    up i pair = i{keyPairs = pair}

instrUpdateNotation :: InstrumentData -> Bool -> InstrumentData
instrUpdateNotation i n = i{showNotation = n}

instrUpdateTrans :: InstrumentData -> AbsPitch -> InstrumentData
instrUpdateTrans i tp =  i{transpose=tp}

instrUpdatePedal :: InstrumentData -> Bool -> InstrumentData
instrUpdatePedal i p =  i{pedal=p}

getKeyData :: AbsPitch -> Signal InstrumentData -> Signal KeyData
getKeyData ap = lift1 f
  where
    f (InstrumentData isshow pairs trans _) = KeyData (if isNothing pairs then Nothing
                                                       else (maybe Nothing Just (lookup ap (fromJust pairs))))
                                                      (if isshow then (Just (show $ fst $ pitch ap)) else Nothing)
                                                      (ap + trans)

pairToMsg :: Midi.Channel -> [(AbsPitch, Bool)] -> [MidiMessage]
pairToMsg ch msg = map f msg
  where
   f     (ap, b) | b==True  = Std (Midi.NoteOn ch ap vel)
                 | b==False = Std (Midi.NoteOff ch ap vel)

mmToPair :: [MidiMessage] -> [(AbsPitch, Bool)]
mmToPair [] = []
mmToPair (Std (Midi.NoteOn chn k v) : rest) = (k, True)  : mmToPair rest
mmToPair (Std (Midi.NoteOff chn k v) : rest)= (k, False) : mmToPair rest
mmToPair ((ANote ch k v d):rest) = error "ANote not implemented"
mmToPair (other:rest) = mmToPair rest

-- helpers
-- These two functions are extend .|. to work on list
(*:) ::  EventS a ->  EventS [a] ->  EventS [a]
(*:) = lift2 aux
  where
    aux Nothing y = y
    aux (Just x)  Nothing = Just [x]
    aux (Just n) (Just ns) = Just (n:ns)

(|+|) :: EventS [a] -> EventS [a] -> EventS [a]
(|+|) = lift2 f
  where
   f (Just [])  (Just [])  = Nothing
   f (Just ns1) (Just ns2) = Just (ns1 ++ ns2)
   f n1         Nothing    = n1
   f Nothing    n2         = n2

--display functions borrowed from GMIExamples.lhs
display' :: Show a => Signal a -> UI ()
display' = display . lift1 show

withDisplay s w = leftRight $ label s >> w >>= \w -> display' w >> return w

ifS predicate stm1 stm2 = lift3 cond predicate stm1 stm2
 where
  cond True e1 _ = e1
  cond False _ e2 = e2

checkboxc :: String -> Char -> Bool -> UI (Signal Bool)
checkboxc label c state = mdo
    let v = accum state (edge s ->> not)
    s <- togglec c state d draw v
    return v
    where
      (tw, th) = (8 * length label, 16)
      (minw, minh) = (tw + padding * 2, th + padding * 2)
      d = Layout 1 0 0 minh minw minh
      draw ((x,y), (w,h)) down =
        let x' = x + padding + 16
            y' = y + (h - th) `div` 2
            b = ((x + padding + 2, y + h `div` 2 - 6), (12, 12))
        in (withColor Black $ text (x', y') label) //
           (if down
              then withColor' gray3 (polyline
                [(x + padding + 5, y + h `div` 2),
                 (x + padding + 7, y + h `div` 2 + 3),
                 (x + padding + 11, y + h `div` 2 - 2)])
              else nullGraphic) //
              box pushed b // (withColor White $ block b)

--Some temporary modification: I changed toogle and checkbox to allow keyboard shortcut.
togglec c init layout draw =
  mkUI init layout draw (const nullSound) zipS process unzipS
  where
    process ctx ((s,s'), (evt, sys)) = ((on,s), markDirty sys' (s /= s'))
      where
        (on, sys') = case evt of
          UIEvent (Key c' down) -> (c==c' && down, sys)
          UIEvent (Button pt True down) | pt `inside` bbx -> (down, sys)
          _ -> (False, sys)
          where
            bbx = computeBBX ctx layout

