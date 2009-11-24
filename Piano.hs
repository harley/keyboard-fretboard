module Piano where

import Euterpea hiding (line)
import Euterpea.UI hiding (button)
import Euterpea.UI.SOE
import Euterpea.UI.UIMonad
import Euterpea.UI.Widget
import Euterpea.UI.Signal
import InstrumentBase
import qualified Codec.Midi as Midi
import Data.Maybe
import qualified Data.List as List
import Helper

data KeyType = White1 | White2 | White3 | Black1 deriving (Show, Eq)
defaultKeyLayout = cycle [White1, Black1, White2, Black1, White3, White1, Black1, White2, Black1, White2, Black1, White3]

-- *****************************************************************************
--   Key properties
-- *****************************************************************************
{-
White keys
  ww1 = ww - bw/2
  ww2 = ww - bw

  ww1        bw       ww2
+----+     +----+     +-+     +----+     +----+
| w  |     |b   |     | |     |    |     |    |
| h  |     |l  n|     | |     |    |     |    |
| i  |     |a  o|     | |     |    |     |    |
| t  |bh   |c  t|     | |     |    |     |    |
| e  |     |k  e|     | |     |    |     |    | wh
| n  +--+  +----+  +--+ +--+  +----+  +--+    +
| o     |          |       |          |       |
| t     |          |       |          |       |
| e     |          |       |          |       |
+-------+          +-------+          +-------+
   ww
-}
-- Width Height of White and Black notes
(ww, wh) = (35, 100)
(bw, bh) = (25, 60)
(tw, th) = (8, 16)              --fixed font size

topW Black1 = bw `div` 2
topW White1 = ww - bw `div` 2
topW White2 = ww - bw `div` 2
topW White3 = ww

insideKey :: KeyType -> (Int,Int) -> ((Int,Int),(Int,Int)) -> Bool
insideKey Black1 pt b@((x, y), (w, h)) = pt `inside` ((x,y),(bw,bh))
insideKey White1 pt b@((x, y), (w, h)) =
    let b1 = ((x,y), (ww - bw `div` 2,  bh))
        b2 = ((x,  y+bh), (ww, wh-bh))
    in (pt `inside` b1) || (pt `inside` b2)
insideKey White2 pt b@((x, y), (w, h)) =
    let b1 = ((x+bw `div` 2,y), (ww - bw,  bh))
        b2 = ((x,  y+bh), (ww, wh-bh))
    in (pt `inside` b1) || (pt `inside` b2)
insideKey White3 pt b@((x, y), (w, h)) =
    let b1 = ((x+bw `div` 2,y), (bw `div` 2,  bh))
        b2 = ((x,  y+bh), (ww, wh-bh))
    in (pt `inside` b1) || (pt `inside` b2)

isBlack Black1 = True
isBlack _      = False


-- *****************************************************************************
--   Drawing routines for each key type
-- *****************************************************************************
drawBox kt | kt == White1 = white1
           | kt == White2 = white2
           | kt == White3 = white3
           | kt == Black1 = black1

white1 [] ((x, y), (w, h)) = nullGraphic
white1 ((t, b):cs) ((x, y), (w, h)) =
  let x' = x + w - bw `div` 2
      y' = y + bh
  in
      white1 cs ((x + 1, y + 1), (w - 2, h - 2)) //
      withColor' t (line (x, y) (x, y + h - 1) //
                    line (x, y) (x' - 2, y) //
                    line (x' - 2, y+bh) (x + w - 2, y+bh)) //

      withColor' b (line (x + 1, y + h - 1) (x + w - 1, y + h - 1) //
                    line (x + w - 2 - bw `div` 2, y) (x + w - 2 - bw `div` 2, y+bh) //
                    line (x + w - 1, y + bh) (x + w - 1, y + h - 1))

white2 [] ((x, y), (w, h)) = nullGraphic
white2 ((t, b):cs) ((x, y), (w, h)) =
  let x1 = x + bw `div` 2
      x2 = x + w - bw `div` 2
      y' = y + bh
  in
      white2 cs ((x + 1, y + 1), (w - 2, h - 2)) //
      withColor' t (line (x1+2, y) (x1+2, y' - 1) //
                    line (x1+2, y) (x2 - 2, y) //
                    line (x - 2, y') (x1 - 2, y') //
                    line (x2- 2, y') (x + w - 2, y')) //

      withColor' b (line (x + 1, y + h - 1) (x + w - 1, y + h - 1) //
                    line (x2 - 1, y) (x2 - 1, y') //
                    line (x + w - 1, y + bh) (x + w - 1, y + h - 1))

black1 [] ((x, y), (w, h)) = nullGraphic
black1 ((t, b):cs) ((x, y), (w, h)) =
  black1 cs ((x + 1, y + 1), (w - 2, h - 2)) //
  withColor' t (line (x, y) (x, y + h - 1) //
                line (x, y) (x + w - 2, y)) //
  withColor' b (line (x + 1, y + h - 1) (x + w - 1, y + h - 1) //
                line (x + w - 1, y) (x + w - 1, y + h - 1))


white3 [] ((x, y), (w, h)) = nullGraphic
white3 ((t, b):cs) ((x, y), (w, h)) =
  let x1 = x + bw `div` 2
      y' = y + bh
  in
      white3 cs ((x + 1, y + 1), (w - 2, h - 2)) //
      withColor' t (line (x1+2, y) (x1+2, y' - 1) //
                    line (x1+2, y) (x + w - 2, y) //
                    line (x - 2, y') (x1 - 2, y')) //

      withColor' b (line (x + 1, y + h - 1) (x + w - 1, y + h - 1) //
                    line (x + w - 1, y) (x + w - 1, y') //
                    line (x + w - 1, y + bh) (x + w - 1, y + h - 1))

colorKey Black1 b = withColor Black $ block b
colorKey kt ((x,y), (w,h)) = withColor White $ block ((x, y+bh), (ww, wh-bh)) // (f kt)
                where f White1 = block ((x,y), (ww- bw `div` 2, bh))
                      f White2 = block ((x+ bw `div` 2, y), (ww-bw, bh))
                      f White3 = block ((x+ bw `div` 2, y), (ww-bw `div` 2, bh))


-- *****************************************************************************
--   Single-key widget: handles key/mouse input and check if the song is playing
-- *****************************************************************************
mkKey :: Char -> KeyType -> Signal KeyData -> UI (Signal KeyBool)
mkKey c kt kiE =
  mkUI (KeyBool False False False, Nothing) d draw (const nullSound) inputInj
       process outputProj kiE
  where
    -- general type: inputInj :: (a -> Signal s -> Signal s1)
    inputInj :: Signal KeyData -> Signal (KeyBool, Maybe String) -> Signal (KeyBool, Maybe String)
    -- we update internal state of UI monad with KeyData stuff
    inputInj a s = lift2 update a s
     where
      update kd (kb,_) | isJust (pressed kd) = (kb{song= fromJust $ pressed kd}, notation kd)
                       | otherwise   = (kb, notation kd)
    outputProj s = (fstS s, s)      --outputProj :: (Signal s2 -> (b, Signal s))
    minh | isBlack kt = bh
         | otherwise =  wh
    minw = topW kt

    realBBX b@((x,y), (w,h)) = let (w', h') | isBlack kt = (bw, bh)
                                            | otherwise  = (ww, wh)
                               in ((x,y), (w',h'))
    d = Layout 0 0 0 minh minw minh -- computeBBX uses this
    draw b (kb, showNote) =
          let isdown = isKeyDown kb
              b'@((x,y),(w,h)) = realBBX b
              x' = x + (w - tw) `div` 2 + if isdown then 0 else -1
              y' = y + h `div` 3 + (h - th) `div` 2 + if isdown then 0 else -1
              drawNotation s = withColor Red $ text (x'+(1-length s)*tw `div` 2, y'- th + 2) s
          in (withColor (if (isBlack kt) then White else Black) $ text (x', y') [c]) //
             (maybe nullGraphic drawNotation showNote) //
             (withColor White $ (drawBox kt) (if isdown then pushed else popped) b') //
             (colorKey kt b')

    process ctx ((kb,showNote), (evt, sys)) = ((kb',showNote), markDirty sys' True )
      where
        (kb', sys') = case evt of
          UIEvent (Key c' down) -> (if c==c' then kb{keypad=down} else kb, sys)
          UIEvent (Button pt True down) -> case (focused, mouse kb, down) of
            (True, False, True) -> (kb{mouse=True}, sys)
            (True, True, False) -> (kb{mouse=False}, sys)
            _ -> (kb, sys)
          UIEvent (MouseMove pt) -> if (insideKey kt pt bbx)
            then (kb, if focused then sys else sys { nextFocus = Just myid })
            else (kb{mouse=False}, if focused then sys { focus = Nothing } else sys)
          _ -> (kb, sys)
          where
            bbx = computeBBX ctx d
            myid = uid ctx
            focused = focus sys == Just myid

-- *****************************************************************************
--   Group all keys together
-- *****************************************************************************
mkKeys :: [(Char, KeyType, AbsPitch)] -> Signal InstrumentData -> UI (EventS [(AbsPitch, Bool)])
mkKeys cka iS = do msgs <- (mapM aux cka)
                   return (foldl (|+|) (constant Nothing) msgs)
 where
  uniqueJ kbs pedal ap = unique (join kbs pedal) =>>
                            (\ (on, ped) -> case (on,ped) of
                              (True,_) -> [(ap, True)]
                              (False,False) -> [(ap, False)]
                              otherwise -> [])
  aux (c, kt, ap) = do
    kbS <- mkKey c kt (getKeyData ap iS)
    return $ uniqueJ (lift1 isKeyPlay kbS) (lift1 pedal iS) ap

-- *****************************************************************************
--   Main widget: piano that takes a map (string) of characters to map to notes
--   and the pitch of the first note
--   two default maps are provided so that two piano can be loaded concurrently
-- *****************************************************************************
type PianoKeyMap = (String, Pitch)
defaultMap1, defaultMap2, defaultMap0 :: PianoKeyMap
defaultMap1 = ("q2w3er5t6y7uQ@W#ERT^Y&U*", (C,3))
defaultMap2 = ("zsxdcvgbhnjmZSXDCVGBHNJM", (C,4))
defaultMap0 = (fst defaultMap1 ++ fst defaultMap2, (C,4))

piano :: PianoKeyMap -> Midi.Channel -> Signal InstrumentData -> EMM -> UI EMM
piano (s, p) chn instrData emm = do
  let instrData' = lift2 updateIns instrData apES
  octave <- withDisplay   "  Lowest Octave   " $ hiSlider 1 (1,8) 3
  keys <- leftRight $ mkKeys (zip3 s defaultKeyLayout (iterate (1+) (absPitch p))) instrData'
  return ((keys =>> pairToMsg chn) |+| emm)
 where
  apES = emm =>> mmToPair
  updateIns i a = i{keyPairs=a}


-- *****************************************************************************
--   Test/Demo: run "ghci Piano" and "main" from the prompt
-- *****************************************************************************
main = runUIEx (500,1000) "Testing Widget" $ do
      devId   <- selectOutput
      piano1 <- title "Keyboard 1" $ do
        songEMM <- selectSong [fjfj]
        pedalData <- addPedal 'P' defaultInstrumentData
        msg <- title "Keyboard 1" $ piano defaultMap0 0 pedalData songEMM
        addEcho msg

      piano2  <- title "Keyboard 2" $ do
        msg <- piano defaultMap2 1 defaultInstrumentData nullEMM
        selectInstrument 1 15 msg

      midiOut devId (piano1 |+| piano2)

