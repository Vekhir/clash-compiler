{-|
Copyright  :  (C) 2017, Google Inc,
                  2023, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

PLL and other clock-related components for Xilinx FPGAs
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Clash.Xilinx.ClockGen where

import Clash.Annotations.Primitive (hasBlackBox)
import Clash.Clocks (clocks, clocksResetSynchronizer)
import Clash.Magic (setName)
import Clash.Promoted.Symbol (SSymbol(..))
import Clash.Signal.Internal

clockWizardSync ::
  forall domIn domOut .
  ( KnownDomain domIn
  , KnownDomain domOut
  ) =>
  Clock domIn ->
  Reset domIn ->
  (Clock domOut, Reset domOut)
clockWizardSync clkIn rstIn =
  clocksResetSynchronizer (unsafeClockWizard clkIn rstIn) clkIn

-- | A clock source that corresponds to the Xilinx MMCM component created
-- with the \"Clock Wizard\" with settings to provide a stable 'Clock' from
-- a single free-running clock input.
--
-- You can use type applications to specify the output clock domain, e.g.:
--
-- @
-- createDomain vXilinxSystem{vName=\"Dom100MHz\", vPeriod=10000}
--
-- -- Outputs a clock running at 100 MHz
-- clockWizard \@_ \@Dom100MHz (SSymbol \@\"clkWizard50to100\") clk50 rst
-- @
--
-- See also the [Clocking Wizard LogiCORE IP Product Guide](https://docs.xilinx.com/r/en-US/pg065-clk-wiz)
clockWizard ::
  forall domIn domOut name .
  ( KnownDomain domIn
  , KnownDomain domOut
  ) =>
  -- | Name of the component instance
  --
  -- Instantiate as follows: @(SSymbol \@\"clockWizard50\")@
  SSymbol name ->
  -- | Free running clock (i.e. a clock pin connected to a crystal)
  Clock domIn ->
  -- | Reset for the PLL
  Reset domIn ->
  -- | (Stable PLL clock, PLL lock)
  (Clock domOut, Signal domOut Bool)
clockWizard _ = setName @name unsafeClockWizard
{-# INLINE clockWizard #-}
{-# DEPRECATED clockWizard "This function is unsafe. Please see documentation of the function for alternatives." #-}

-- | A clock source that corresponds to the Xilinx MMCM component created
-- with the \"Clock Wizard\" with settings to provide a stable 'Clock' from
-- a single free-running clock input.
--
-- Generates Tcl according to Clash\<-\>Tcl API
-- You can use type applications to specify the output clock domain, e.g.:
--
-- @
-- createDomain vXilinxSystem{vName=\"Dom100MHz\", vPeriod=10000}
--
-- -- Outputs a clock running at 100 MHz
-- clockWizard \@_ \@Dom100MHz (SSymbol \@\"clkWizard50to100\") clk50 rst
-- @
--
-- See also the [Clocking Wizard LogiCORE IP Product Guide](https://docs.xilinx.com/r/en-US/pg065-clk-wiz)
unsafeClockWizard ::
  forall domIn domOut pllLock .
  ( KnownDomain domIn
  , KnownDomain domOut
  , KnownDomain pllLock
  ) =>
  -- | Free running clock (i.e. a clock pin connected to a crystal)
  Clock domIn ->
  -- | Reset for the PLL
  Reset domIn ->
  -- | (Stable PLL clock, PLL lock)
  (Clock domOut, Signal pllLock Bool)
unsafeClockWizard = clocks
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE unsafeClockWizard #-}
{-# ANN unsafeClockWizard hasBlackBox #-}

clockWizardDifferentialSync ::
  forall domIn domOut .
  ( KnownDomain domIn
  , KnownDomain domOut
  ) =>
  DiffClock domIn ->
  Reset domIn ->
  (Clock domOut, Reset domOut)
clockWizardDifferentialSync clkIn@(DiffClock clkInP _) rstIn =
  clocksResetSynchronizer (unsafeClockWizardDifferential clkIn rstIn) clkInP

-- | A clock source that corresponds to the Xilinx MMCM component created
-- with the \"Clock Wizard\", with settings to provide a stable 'Clock'
-- from a free-running differential clock input.
--
-- You can use type applications to specify the output clock domain, e.g.:
--
-- @
-- createDomain vXilinxSystem{vName=\"Dom100MHz\", vPeriod=10000}
--
-- -- Outputs a clock running at 100 MHz
-- clockWizardDifferential \@_ \@Dom100MHz (SSymbol \@\"clkWizard50to100\") clk50 rst
-- @
--
-- See also the [Clocking Wizard LogiCORE IP Product Guide](https://docs.xilinx.com/r/en-US/pg065-clk-wiz)
clockWizardDifferential ::
  forall domIn domOut name .
  ( KnownDomain domIn
  , KnownDomain domOut
  ) =>
  -- | Name of the component instance
  --
  -- Instantiate as follows: @(SSymbol \@\"clockWizardD50\")@
  SSymbol name ->
  -- | Free running clock
  DiffClock domIn ->
  -- | Reset for the PLL
  Reset domIn ->
  -- | (Stable PLL clock, PLL lock)
  (Clock domOut, Signal domOut Bool)
clockWizardDifferential _ = setName @name unsafeClockWizardDifferential
{-# INLINE clockWizardDifferential #-}
{-# DEPRECATED clockWizardDifferential "This function is unsafe. Please see documentation of the function for alternatives." #-}

-- | A clock source that corresponds to the Xilinx MMCM component created
-- with the \"Clock Wizard\", with settings to provide a stable 'Clock'
-- from a free-running differential clock input.
--
-- You can use type applications to specify the output clock domain, e.g.:
--
-- @
-- createDomain vXilinxSystem{vName=\"Dom100MHz\", vPeriod=10000}
--
-- -- Outputs a clock running at 100 MHz
-- clockWizardDifferential \@_ \@Dom100MHz (SSymbol \@\"clkWizard50to100\") clk50 rst
-- @
--
-- See also the [Clocking Wizard LogiCORE IP Product Guide](https://docs.xilinx.com/r/en-US/pg065-clk-wiz)
unsafeClockWizardDifferential ::
  forall domIn domOut pllLock .
  ( KnownDomain domIn
  , KnownDomain domOut
  , KnownDomain pllLock
  ) =>
  -- | Free running clock
  DiffClock domIn ->
  -- | Reset for the PLL
  Reset domIn ->
  -- | (Stable PLL clock, PLL lock)
  (Clock domOut, Signal pllLock Bool)
unsafeClockWizardDifferential (DiffClock clk _) = clocks clk
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE unsafeClockWizardDifferential #-}
{-# ANN unsafeClockWizardDifferential hasBlackBox #-}
