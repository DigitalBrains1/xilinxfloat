-- Run a test bench on actual hardware
--
-- led[0], marked LD4 on the board, turns on when done
-- led[1], marked LD5 on the board, turns off when not okay
--
-- In other words, when both are on everything is fine. If LD4 is on but LD5 is
-- off then at least one of the samples was incorrect.

{-# OPTIONS_GHC -Wno-orphans #-}

module PhysTB where

import Clash.Prelude
import qualified Clash.Explicit.Prelude as CEP
import Clash.Xilinx.ClockGen

import Clash.Explicit.Testbench

import qualified Clash.Cores.Xilinx.Floating as F

import Floating
import Floating.TH

createDomain vXilinxSystem{vName = "DomInput"}

-- XXX: Need to fix 'clockWizard' first.
{-
physAddTB
  :: Clock DomInput
  -> Signal DomInput Bool
  -> Signal XilinxSystem (Vec 2 Bool)
physAddTB clkIn rstIn = physAddTB0 clk rst
 where
  (clk, pllStable) =
    clockWizard (SSymbol @"clkWizard100") clkIn (unsafeFromLowPolarity rstIn)
  rst = resetSynchronizer clk (unsafeFromLowPolarity pllStable)
-}

physAddTB
  :: Clock XilinxSystem
  -> Signal XilinxSystem Bool
  -> Signal XilinxSystem (Vec 2 Bool)
physAddTB clk rstIn = physAddTB0 clk rst
 where
  rst = resetSynchronizer clk (unsafeFromLowPolarity rstIn)
{-# ANN physAddTB
        (Synthesize { t_name = "physAddTB"
                    , t_inputs =
                        [ PortName "CLK100MHZ"
                        , PortName "ck_rst"
                        ]
                    , t_output = PortName "led" }) #-}

physAddTB0
  :: Clock XilinxSystem
  -> Reset XilinxSystem
  -> Signal XilinxSystem (Vec 2 Bool)
physAddTB0 clk rst = bundle (okay :> done :> Nil)
 where
  (done, samples) = playSampleRom clk rst $(memBlobTH Nothing addBasicSamples)
  (inputX, inputY, expectedOutput) = unbundle samples
  okay =
    CEP.register clk rst en True $
      okayF <$> okay <*> done <*> out <*> expectedOutput
  okayF okay0 done0 out0 expect =
    okay0 && (done0 || FloatVerifier out0 == FloatVerifier expect)
  out =
    ignoreFor clk rst en d11 0 . toSignal $
      comp (fromSignal inputX) (fromSignal inputY)
  comp = withClock clk $ withEnable en $ F.add
  en = enableGen

tbTB
  :: Signal XilinxSystem (Vec 2 Bool)
tbTB = out
 where
  out = physAddTB clk rst
  done = CEP.unsafeSynchronizer clockGen clockGen $ fmap (!! (1 :: Int)) out
  clk = tbClockGen (not <$> done)
  rst = unsafeToLowPolarity resetGen
{-# ANN tbTB (TestBench 'physAddTB) #-}
