{-# OPTIONS -fglasgow-exts  #-} -- needed for mdo notation

module Guitar where

import Euterpea hiding (line)
import InstrumentBase
import Euterpea.UI.SOE
import Euterpea.UI.UIMonad
import Euterpea.UI.Widget
import Euterpea.UI.Signal
import qualified Codec.Midi as Midi
import Data.Maybe
import Helper

-- first fret's width and height
(fw, fh) = (90, 45)
(tw, th) = (8, 16)              --fixed font size

type KeyType = Int
type GuitarKeyMap = [([Char], Pitch, Char)]
-- *****************************************************************************
--   Drawing routines for each key type
-- *****************************************************************************
drawFret [] ((x, y), (w, h)) = nullGraphic
drawFret ((t, b):cs) ((x, y), (w, h)) =
  drawFret cs ((x + 1, y + 1), (w - 2, h )) //
  withColor' t (line (x, y) (x, y + h)) //
  withColor' b (line (x + w - 1, y) (x + w - 1, y + h))

drawString down ((x, y), (w, h)) =
  withColor Black (if down then (arc (x,midY+2) (x+w, midY-2) (-180) 180)
                    else (line (x-1, y+ h `div` 2) (x+w, y+h `div` 2))) //
  if down then withColor Blue (ellipse (midX - d, midY - d) (midX + d, midY + d)) else nullGraphic
  where d = 10
        midX = x + w `div` 2
        midY = y + h `div` 2

-- draw the guitar head, take number of strings as argument
drawHead :: Int -> UI ()
drawHead 0 = return ()
drawHead n = topDown $ do
    UI aux
    drawHead (n-1)
  where
     (minw, minh) = (fw, fh)
     d = Layout 0 0 minw minh minw minh
     drawit b@((x, y), (w, h)) = withColor Black $ line (x, y+h `div` 2 + 5 * (3-n)) (x+w, y+ h `div` 2)
     aux ctx inp = (out, (d, ()))
       where
         out = fmap (\(evt, sys) -> ((drawit bbx, return ()), sys)) inp
         bbx = computeBBX ctx d

-- *****************************************************************************
--   Single-key widget: handles key/mouse input and check if the song is playing
-- *****************************************************************************
mkKey :: Char -> KeyType -> Signal KeyData -> UI (Signal KeyBool)
mkKey c n kdS =
  mkUI (KeyBool False False False, Nothing) d draw (const nullSound) inputInj process outputProj kdS
  where
    -- general type: inputInj :: (a -> Signal s -> Signal s1)
    inputInj a s = lift2 update a s
     where
      update kd (kb,_) | isJust (pressed kd) = (kb{song= fromJust $ pressed kd}, notation kd)
                       | otherwise   = (kb, notation kd)
    outputProj s = (fstS s, s)      --outputProj :: (Signal s2 -> (b, Signal s))
    (minh, minw) = (fh, fw - n * 3)
    d = Layout 0 0 0 minh minw minh -- computeBBX uses this
    draw b (kb, showNote) =
          let
              isdown = isKeyDown kb
              b'@((x,y),(w,h)) = id b
              x' = x + (w - tw) `div` 2 + if isdown then 0 else -1
              y' = y + h `div` 5 + (h - th) `div` 2 + if isdown then 0 else -1
              drawNotation s = withColor Red $ text (x'+(1-length s)*tw `div` 2, y'- th) s
          in (withColor Blue $ text (x', y') [c]) //
             (maybe nullGraphic drawNotation showNote) //
             (drawString isdown b') //
             (drawFret popped b')

    process ctx ((kb,showNote), (evt, sys)) = ((kb', showNote), markDirty sys' True)
      where
        (kb', sys') = case evt of
          UIEvent (Key c' down) -> (if c==c' then kb{keypad=down} else kb, sys)
          UIEvent (Button pt True down) -> case (focused, mouse kb, down) of
            (True, False, True) -> (kb{mouse=True}, sys)
            (True, True, False) -> (kb{mouse=False}, sys)
            _ -> (kb, sys)
          UIEvent (MouseMove pt) -> if (pt `inside` bbx)
            then (kb, if focused then sys else sys { nextFocus = Just myid })
            else (kb{mouse=False}, if focused then sys { focus = Nothing } else sys)
          _ -> (kb, sys)
          where bbx = computeBBX ctx d
                myid = uid ctx
                focused = focus sys == Just myid

mkKeys :: AbsPitch -> Signal Bool -> [(Char, KeyType, AbsPitch)] -> Signal InstrumentData -> UI (EventS [(AbsPitch, Bool)])
mkKeys free pluckS cka iS = do msgs <- (mapM aux cka)
                               return (foldl (|+|) (constant Nothing) msgs)
 where
  aux (c, kt, ap) = do
    kbS <- mkKey c kt (getKeyData ap iS)
    return $ unique (join (lift1 isKeyPlay kbS) pluckS) =>>
                (\(on, pik) -> if pik then (if on then [(ap, True)] else [(free, True)]) else [(ap,False)])

mkString :: ([Char], Pitch, Char) -> Signal InstrumentData -> UI (EventS [(AbsPitch, Bool)])
mkString (frets, freePitch, p) insData = leftRight $ do
  ispluck <- pluckString p
  let freeap = absPitch freePitch
  mkKeys freeap ispluck (zip3 frets [1..] [freeap+1..]) insData

-- listen to check if a string is picked with right hand
pluckString :: Char -> UI (Signal Bool)
pluckString c =
   mkUI False d draw (const nullSound) (const id)
        process dup (constant ())
   where
     d = nullLayout
     draw b@((x,y), (w,h)) down =
       let x' = x + (w - tw) `div` 2 + if down then 0 else -1
           y' = y + (h - th) `div` 2 + if down then 0 else -1
       in (if down then (withColor White) else (withColor Black)) $ block ((0,0), (10,10))
     process ctx (s, (evt, sys)) = (s', markDirty sys' (s /= s'))
       where
         (s', sys') = case evt of
           UIEvent (Button pt True down) -> (down, sys)
           UIEvent (Key c' down) -> (c==c' && down, sys)
           _ -> (s, sys)

--TODO: add strum key to tuple
guitar :: GuitarKeyMap -> Midi.Channel -> Signal InstrumentData -> EMM -> UI EMM
guitar spcList chn iD emm = topDown $ mdo
  let instrData = instrUpdateEMM iD emm
  leftRight $ do
    drawHead (length spcList)
    topDown $ do
      pairs <- mapM (\x -> mkString x instrData) spcList
      let msgs = map (\p -> p =>> pairToMsg chn) pairs
      return $ (foldl (|+|) (constant Nothing) msgs) |+| emm

string6 = ("1qaz__________", (E,6), '\b')
string5 = ("2wsx__________", (B,5), '=')
string4 = ("3edc__________", (G,5), '-')
string3 = ("4rfv__________", (D,5), '0')
string2 = ("5tgb__________", (A,4), '9')
string1 = ("6yhn__________", (D,4), '8')

main = runUIEx (1200,700) "Demo guitar" $ do
      devId <- selectOutput
      songMsg   <- (selectSong [fjfj])
      msg <- guitar [string1, string2, string3, string4, string5, string6] 1 defaultInstrumentData songMsg
      midiOut devId msg

