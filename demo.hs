module Demo where

import Euterpea.UI (runUIEx, selectOutput, title, pad, midiOut)
import InstrumentBase
import Piano hiding (main)
import Guitar hiding (main)
import Helper

demo1 = runUIEx (1000,1200) "Demo Virtual Instruments" $ do
      devId   <- selectOutput

      piano1 <- title "Keyboard 1" $ do
        songEMM <- selectSong [fjfj]
        pedalData <- addPedal 'P' defaultInstrumentData
        msg <- piano defaultMap0 0 pedalData songEMM
        addEcho msg

      guitar1 <- pad (0,50,0,0) $ title "Guitar 1" $ do
        notationData <- addNotation 'N' defaultInstrumentData
        guitar [string1, string2, string3, string4, string5, string6] 1 notationData nullEMM

      midiOut devId (piano1 |+| guitar1)

demo2 = runUIEx (550,1000) "Demo 2 keyboards" $ do
      devId   <- selectOutput

      piano1 <- title "Keyboard 1" $ do
        songEMM <- selectSong [fjfj]
        pedalData <- addPedal 'P' defaultInstrumentData
        msg <- title "Keyboard 1" $ piano defaultMap1 0 pedalData songEMM
        msgWithEcho <- addEcho msg
        selectInstrument 1 15 msgWithEcho

      piano2  <- pad (0,50,0,0) $ title "Keyboard 2" $ do
        notationData <- addNotation 'N' defaultInstrumentData
        piano defaultMap2 1 notationData nullEMM

      midiOut devId (piano1 |+| piano2)

main = demo1

