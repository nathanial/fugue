/-
  Fugue.Effects.Reverb - Algorithmic reverb effects

  Simulates acoustic space using delay-based algorithms.
-/
import Fugue.Core.Signal

namespace Fugue.Effects

open Fugue

/-- Reverb configuration parameters. -/
structure ReverbConfig where
  /-- Room size (0.0 = small, 1.0 = large hall) -/
  roomSize : Float := 0.5
  /-- High frequency damping (0.0 = bright, 1.0 = dark) -/
  damping  : Float := 0.5
  /-- Wet/dry mix (0.0 = dry, 1.0 = wet) -/
  wetDry   : Float := 0.3
  /-- Pre-delay in seconds (time before reverb starts) -/
  preDelay : Float := 0.0
  deriving Repr, Inhabited

/-- Comb filter - delay with feedback, fundamental reverb building block.
    Creates a series of decaying echoes at regular intervals. -/
private def combFilter (delayTime : Float) (feedback : Float) (maxEchoes : Nat)
    (sig : Signal Float) : Signal Float :=
  fun t => Id.run do
    let mut sum := 0.0
    for i in [:maxEchoes + 1] do
      let echoTime := t - (i.toFloat * delayTime)
      if echoTime >= 0.0 then
        let attenuation := Float.pow feedback i.toFloat
        sum := sum + sig.sample echoTime * attenuation
    sum

/-- Allpass filter approximation - adds diffusion without changing frequency content.
    Uses a combination of delayed signal and feedforward. -/
private def allpassFilter (delayTime : Float) (feedback : Float := 0.5)
    (sig : Signal Float) : Signal Float :=
  fun t =>
    let direct := sig.sample t
    if t >= delayTime then
      let delayed := sig.sample (t - delayTime)
      -- Allpass: output = -g*input + delayed + g*delayed_output
      -- Simplified approximation: mix direct and delayed
      delayed * feedback + direct * (1.0 - feedback)
    else
      direct

/-- Simple algorithmic reverb using parallel comb filters.
    Based on Schroeder reverb architecture with prime-related delay times. -/
def reverb (config : ReverbConfig := {}) (sig : Signal Float) : Signal Float :=
  fun t => Id.run do
    let dry := sig.sample t

    -- Apply pre-delay
    let t' := t - config.preDelay

    if t' < 0.0 then
      return dry

    -- Comb filter delay times (prime-related ratios scaled by room size)
    -- These create the characteristic reverb tail
    let baseDelays := #[0.0297, 0.0371, 0.0411, 0.0437, 0.0483, 0.0531]
    let scaledDelays := baseDelays.map (· * (0.5 + config.roomSize))

    -- Feedback decreases with damping (damping reduces high freq content simulation)
    let baseFeedback := 0.84 - config.damping * 0.2
    let maxEchoes := 12 + (config.roomSize * 8).toUInt64.toNat

    -- Sum parallel comb filters
    let mut wet := 0.0
    for i in [:scaledDelays.size] do
      let delayTime := scaledDelays[i]!
      -- Slightly vary feedback for each comb
      let fb := baseFeedback - (i.toFloat * 0.02)
      let comb := combFilter delayTime fb maxEchoes sig
      wet := wet + comb.sample t'

    -- Normalize by number of combs
    let wetNormalized := wet / scaledDelays.size.toFloat

    -- Apply diffusion via allpass filters
    let diffused := allpassFilter 0.005 0.5 (fun _ => wetNormalized) |>.sample t'
    let diffused2 := allpassFilter 0.0017 0.5 (fun _ => diffused) |>.sample t'

    -- Mix dry and wet
    dry * (1.0 - config.wetDry) + diffused2 * config.wetDry

/-- Early reflections only - room character without reverb tail.
    Simulates first few reflections off walls. -/
def earlyReflections (roomSize : Float := 0.3) (sig : Signal Float) : Signal Float :=
  fun t => Id.run do
    let dry := sig.sample t

    -- Early reflection times based on room size (typical room dimensions)
    let reflections := #[
      (0.010 * roomSize, 0.8),   -- First wall
      (0.018 * roomSize, 0.6),   -- Second wall
      (0.025 * roomSize, 0.5),   -- Ceiling
      (0.033 * roomSize, 0.4),   -- Back wall
      (0.042 * roomSize, 0.3)    -- Combined reflections
    ]

    let mut wet := 0.0
    for (delayTime, level) in reflections do
      if t >= delayTime then
        wet := wet + sig.sample (t - delayTime) * level

    pure (dry * 0.7 + wet * 0.3)

/-- Plate reverb - simulates metal plate reverberator.
    Brighter and denser than room reverb. -/
def plateReverb (decay : Float := 0.7) (sig : Signal Float) : Signal Float :=
  let config : ReverbConfig := {
    roomSize := 0.3 + decay * 0.4
    damping := 0.2  -- Plates are bright
    wetDry := 0.4
    preDelay := 0.01
  }
  reverb config sig

/-- Hall reverb - large concert hall simulation.
    Long decay with smooth tail. -/
def hallReverb (size : Float := 0.8) (sig : Signal Float) : Signal Float :=
  let config : ReverbConfig := {
    roomSize := 0.7 + size * 0.3
    damping := 0.6  -- Halls absorb high frequencies
    wetDry := 0.35
    preDelay := 0.025
  }
  reverb config sig

/-- Room reverb - small to medium room.
    Shorter decay, more defined reflections. -/
def roomReverb (size : Float := 0.4) (sig : Signal Float) : Signal Float :=
  let config : ReverbConfig := {
    roomSize := size
    damping := 0.4
    wetDry := 0.25
    preDelay := 0.005
  }
  reverb config sig

/-- Spring reverb simulation - characteristic "boing" sound.
    Simulates spring-based reverb units. -/
def springReverb (tension : Float := 0.5) (sig : Signal Float) : Signal Float :=
  fun t => Id.run do
    let dry := sig.sample t

    -- Springs have characteristic modal behavior
    -- Simulate with modulated delays
    let springDelay := 0.030 + tension * 0.020
    let wobbleRate := 2.0 + tension * 3.0

    let mut wet := 0.0
    let maxBounces := 8

    for i in [1:maxBounces + 1] do
      let wobble := Float.sin (3.14159 * 2.0 * wobbleRate * t + i.toFloat) * 0.002
      let echoTime := t - (i.toFloat * (springDelay + wobble))
      if echoTime >= 0.0 then
        let decay := Float.pow 0.6 i.toFloat
        wet := wet + sig.sample echoTime * decay

    pure (dry * 0.6 + wet * 0.4)

/-- Shimmer reverb - reverb with pitch-shifted feedback.
    Creates ethereal, evolving textures. -/
def shimmerReverb (shift : Float := 0.5) (sig : Signal Float) : Signal Float :=
  fun t =>
    -- Base reverb
    let config : ReverbConfig := {
      roomSize := 0.8
      damping := 0.3
      wetDry := 0.5
      preDelay := 0.03
    }
    let baseReverb := reverb config sig |>.sample t

    -- Add pitched component (octave up simulation via time stretching)
    let pitchRatio := 1.0 + shift  -- shift=0.5 gives ~1.5x = ~7 semitones up
    let pitched := if t > 0.1 then sig.sample ((t - 0.1) / pitchRatio) * 0.3 else 0.0

    baseReverb * 0.7 + pitched * 0.3

end Fugue.Effects
