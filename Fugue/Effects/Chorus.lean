/-
  Fugue.Effects.Chorus - Chorus and flanger effects

  Effects that create thickness by mixing modulated delay lines.
-/
import Fugue.Core.Signal
import Fugue.Osc.Sine

namespace Fugue.Effects

open Fugue
open Fugue.Osc (twoPi)

/-- Chorus configuration parameters. -/
structure ChorusConfig where
  /-- LFO frequency in Hz (typical: 0.5-3 Hz) -/
  rate   : Float := 1.5
  /-- Modulation depth in seconds (typical: 0.001-0.003) -/
  depth  : Float := 0.002
  /-- Number of chorus voices (typical: 2-4) -/
  voices : Nat   := 3
  /-- Wet/dry mix (0.0 = dry, 1.0 = wet) -/
  mix    : Float := 0.5
  /-- Base delay time in seconds -/
  baseDelay : Float := 0.025
  deriving Repr, Inhabited

/-- Chorus effect - multiple detuned copies for thickness.
    Each voice is a delay line with slightly different LFO rate and phase,
    creating subtle pitch/time variations that simulate multiple performers. -/
def chorus (config : ChorusConfig := {}) (sig : Signal Float) : Signal Float :=
  fun t => Id.run do
    let dry := sig.sample t
    let mut wet := 0.0

    for i in [:config.voices] do
      -- Each voice has a different phase offset and slightly different rate
      let phase := i.toFloat / config.voices.toFloat
      let lfoFreq := config.rate * (1.0 + 0.1 * i.toFloat)
      let modulation := Float.sin (twoPi * lfoFreq * t + phase * twoPi) * config.depth
      let delayedT := t - (config.baseDelay + modulation)
      if delayedT >= 0.0 then
        wet := wet + sig.sample delayedT

    let wetNormalized := wet / config.voices.toFloat
    dry * (1.0 - config.mix) + wetNormalized * config.mix

/-- Simple chorus with just rate and depth parameters. -/
def chorusSimple (rate : Float := 1.5) (depth : Float := 0.002)
    (sig : Signal Float) : Signal Float :=
  chorus { rate := rate, depth := depth } sig

/-- Flanger - short modulated delay with feedback.
    Similar to chorus but with shorter delays and feedback for resonance.
    - rate: LFO frequency in Hz
    - depth: Modulation depth in seconds (typically 0.0005-0.002)
    - feedback: Resonance amount (0.0-0.9) -/
def flanger (rate : Float := 0.5) (depth : Float := 0.001) (feedback : Float := 0.7)
    (sig : Signal Float) : Signal Float :=
  fun t => Id.run do
    let baseDelay := 0.005  -- 5ms base delay for flanger
    let modulation := Float.sin (twoPi * rate * t) * depth
    let currentDelay := baseDelay + modulation

    -- Compute with feedback (unrolled)
    let mut sum := sig.sample t
    let maxEchoes := 5
    for i in [1:maxEchoes + 1] do
      let echoTime := t - (i.toFloat * currentDelay)
      if echoTime >= 0.0 then
        let attenuation := Float.pow feedback i.toFloat
        sum := sum + sig.sample echoTime * attenuation

    sum

/-- Flanger with mix control. -/
def flangerMix (rate : Float := 0.5) (depth : Float := 0.001)
    (feedback : Float := 0.7) (mix : Float := 0.5)
    (sig : Signal Float) : Signal Float :=
  fun t =>
    let dry := sig.sample t
    let wet := flanger rate depth feedback sig |>.sample t
    dry * (1.0 - mix) + (wet - dry) * mix

/-- Ensemble effect - rich chorus with spread voices.
    Creates a lush, wide sound with multiple detuned voices. -/
def ensemble (spread : Float := 0.5) (sig : Signal Float) : Signal Float :=
  let config : ChorusConfig := {
    rate := 0.8
    depth := 0.003 * spread
    voices := 4
    mix := 0.6
    baseDelay := 0.030
  }
  chorus config sig

/-- Doubler - subtle chorus for vocal/instrument doubling.
    Simulates a second performer playing slightly out of sync. -/
def doubler (sig : Signal Float) : Signal Float :=
  let config : ChorusConfig := {
    rate := 0.3
    depth := 0.001
    voices := 1
    mix := 0.5
    baseDelay := 0.020
  }
  chorus config sig

/-- Detune - static pitch shifting without modulation.
    Creates a fixed detuned copy mixed with original. -/
def detune (cents : Float := 10.0) (sig : Signal Float) : Signal Float :=
  fun t =>
    -- Detune by stretching/compressing time slightly
    -- 100 cents = 1 semitone, 1200 cents = 1 octave
    let ratio := Float.pow 2.0 (cents / 1200.0)
    let detuned := sig.sample (t * ratio)
    (sig.sample t + detuned) * 0.5

/-- Vibrato chorus - combines chorus with pitch vibrato. -/
def vibratoChorus (rate : Float := 4.0) (chorusDepth : Float := 0.002)
    (vibratoDepth : Float := 0.002) (sig : Signal Float) : Signal Float :=
  fun t =>
    -- Apply vibrato first (pitch modulation via time shifting)
    let vibratoSig : Signal Float := fun t' =>
      let modT := t' + Float.sin (twoPi * rate * t') * vibratoDepth
      if modT >= 0.0 then sig.sample modT else 0.0

    -- Then apply chorus
    let config : ChorusConfig := {
      rate := rate * 0.5
      depth := chorusDepth
      voices := 2
      mix := 0.5
      baseDelay := 0.020
    }
    chorus config vibratoSig |>.sample t

end Fugue.Effects
