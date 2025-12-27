/-
  Fugue.Filter.Envelope - Time-varying filter cutoff modulation

  Filters with modulated cutoff frequencies for dynamic effects
  like filter sweeps, auto-wah, and synth-style enveloped filters.
-/
import Fugue.Core.Signal
import Fugue.Osc.Sine
import Fugue.Filter.Biquad
import Fugue.Filter.OnePole

namespace Fugue.Filter

open Fugue
open Fugue.Osc (twoPi)

/-- Clamp a value to a range. -/
private def clamp (lo hi x : Float) : Float :=
  if x < lo then lo else if x > hi then hi else x

/-- Filter with LFO-modulated cutoff (wah-wah/auto-filter effect).
    The cutoff frequency oscillates sinusoidally.

    - baseCutoff: Center cutoff frequency in Hz
    - depth: Modulation depth (0.0-1.0, fraction of baseCutoff)
    - rate: LFO frequency in Hz
    - Q: Filter resonance -/
def lfoFilter (baseCutoff : Float) (depth : Float) (rate : Float)
    (Q : Float := 2.0) (sampleRate : Float := 44100.0)
    (sig : Signal Float) : Signal Float :=
  fun t =>
    -- Calculate time-varying cutoff
    let lfo := Float.sin (twoPi * rate * t)
    let cutoff := baseCutoff * (1.0 + depth * lfo)
    -- Clamp cutoff to valid range
    let clampedCutoff := clamp 20.0 (sampleRate * 0.45) cutoff
    -- Calculate coefficients for this instant
    let coeffs := calcLowpassCoeffs clampedCutoff Q sampleRate
    -- Apply filter
    biquad coeffs sampleRate sig |>.sample t

/-- Filter with arbitrary cutoff signal.
    Allows any signal to control the filter cutoff frequency.

    - cutoffSig: Signal providing cutoff frequency in Hz -/
def envFilter (cutoffSig : Signal Float) (Q : Float := 2.0)
    (sampleRate : Float := 44100.0) (sig : Signal Float) : Signal Float :=
  fun t =>
    let cutoff := clamp 20.0 (sampleRate * 0.45) (cutoffSig.sample t)
    let coeffs := calcLowpassCoeffs cutoff Q sampleRate
    biquad coeffs sampleRate sig |>.sample t

/-- Envelope follower - extracts amplitude envelope from signal.
    Useful for controlling other parameters based on input level.

    - attackTime: Rise time constant in seconds
    - releaseTime: Fall time constant in seconds -/
def envelopeFollower (attackTime : Float := 0.01) (releaseTime : Float := 0.1)
    (sampleRate : Float := 44100.0) (sig : Signal Float) : Signal Float :=
  let T := 1.0 / sampleRate
  -- Convert times to coefficients
  let attackCoeff := Float.exp (-T / attackTime)
  let releaseCoeff := Float.exp (-T / releaseTime)
  fun t =>
    -- Get absolute value of current sample
    let x := if sig.sample t >= 0.0 then sig.sample t else -(sig.sample t)
    if t < T then
      x
    else
      -- Use absolute value of previous input as approximation for envelope
      let prevInput := sig.sample (t - T)
      let prev := if prevInput >= 0.0 then prevInput else -prevInput
      -- Attack when input rises, release when it falls
      if x > prev then
        (1.0 - attackCoeff) * x + attackCoeff * prev
      else
        (1.0 - releaseCoeff) * x + releaseCoeff * prev

/-- Simplified envelope follower using one-pole lowpass on rectified signal. -/
def envelopeFollowerSimple (smoothTime : Float := 0.05)
    (sampleRate : Float := 44100.0) (sig : Signal Float) : Signal Float :=
  -- Rectify the signal (absolute value)
  let rectified : Signal Float := fun t =>
    let v := sig.sample t
    if v >= 0.0 then v else -v
  -- Smooth with lowpass
  let cutoff := 1.0 / (twoPi * smoothTime)
  lowpass1 cutoff sampleRate rectified

/-- Auto-wah effect: envelope follower controlling filter cutoff.
    Louder input = higher filter cutoff.

    - sensitivity: How much input level affects cutoff (0.0-2.0)
    - minCutoff: Minimum cutoff frequency in Hz
    - maxCutoff: Maximum cutoff frequency in Hz -/
def autoWah (sensitivity : Float := 1.0) (minCutoff : Float := 200.0)
    (maxCutoff : Float := 2000.0) (Q : Float := 4.0)
    (sampleRate : Float := 44100.0) (sig : Signal Float) : Signal Float :=
  fun t =>
    -- Get envelope of input
    let env := envelopeFollowerSimple 0.02 sampleRate sig |>.sample t
    -- Map envelope to cutoff range
    let envScaled := clamp 0.0 1.0 (env * sensitivity)
    let cutoff := minCutoff + envScaled * (maxCutoff - minCutoff)
    -- Apply resonant filter
    let coeffs := calcLowpassCoeffs cutoff Q sampleRate
    biquad coeffs sampleRate sig |>.sample t

/-- Classic synth filter: lowpass with ADSR-modulated cutoff.
    The filter opens during attack and closes during decay/release.

    - baseCutoff: Starting cutoff frequency in Hz
    - envAmount: How much envelope affects cutoff (in Hz)
    - attack, decay, sustain, release: ADSR envelope parameters -/
def synthFilter (baseCutoff : Float) (envAmount : Float)
    (attack : Float) (decay : Float) (sustain : Float) (release : Float)
    (Q : Float := 4.0) (sampleRate : Float := 44100.0)
    (sig : Signal Float) : Signal Float :=
  fun t =>
    -- Simple ADSR calculation (matches Fugue.Env.ADSR pattern)
    let envVal :=
      if t < attack then
        t / attack
      else if t < attack + decay then
        let decayT := t - attack
        1.0 - (1.0 - sustain) * (decayT / decay)
      else if t < attack + decay + 0.5 then  -- Assumed hold time
        sustain
      else
        let releaseT := t - (attack + decay + 0.5)
        if releaseT < release then
          sustain * (1.0 - releaseT / release)
        else
          0.0
    -- Calculate modulated cutoff
    let cutoff := clamp 20.0 (sampleRate * 0.45) (baseCutoff + envAmount * envVal)
    let coeffs := calcLowpassCoeffs cutoff Q sampleRate
    biquad coeffs sampleRate sig |>.sample t

/-- Filter sweep - linear sweep from start to end frequency.

    - startCutoff: Starting cutoff frequency in Hz
    - endCutoff: Ending cutoff frequency in Hz
    - sweepTime: Duration of sweep in seconds -/
def filterSweep (startCutoff : Float) (endCutoff : Float) (sweepTime : Float)
    (Q : Float := 2.0) (sampleRate : Float := 44100.0)
    (sig : Signal Float) : Signal Float :=
  fun t =>
    let progress := clamp 0.0 1.0 (t / sweepTime)
    let cutoff := startCutoff + progress * (endCutoff - startCutoff)
    let clampedCutoff := clamp 20.0 (sampleRate * 0.45) cutoff
    let coeffs := calcLowpassCoeffs clampedCutoff Q sampleRate
    biquad coeffs sampleRate sig |>.sample t

/-- Exponential filter sweep (more natural sounding).

    - startCutoff: Starting cutoff frequency in Hz
    - endCutoff: Ending cutoff frequency in Hz
    - sweepTime: Duration of sweep in seconds -/
def filterSweepExp (startCutoff : Float) (endCutoff : Float) (sweepTime : Float)
    (Q : Float := 2.0) (sampleRate : Float := 44100.0)
    (sig : Signal Float) : Signal Float :=
  fun t =>
    let progress := clamp 0.0 1.0 (t / sweepTime)
    -- Exponential interpolation in log space
    let logStart := Float.log startCutoff
    let logEnd := Float.log endCutoff
    let cutoff := Float.exp (logStart + progress * (logEnd - logStart))
    let clampedCutoff := clamp 20.0 (sampleRate * 0.45) cutoff
    let coeffs := calcLowpassCoeffs clampedCutoff Q sampleRate
    biquad coeffs sampleRate sig |>.sample t

/-- Triggered filter envelope - filter opens on trigger and decays.
    Useful for drum sounds and plucks.

    - peakCutoff: Maximum cutoff during attack
    - sustainCutoff: Cutoff to decay to
    - attackTime: Rise time in seconds
    - decayTime: Decay time in seconds -/
def triggerFilter (peakCutoff : Float) (sustainCutoff : Float)
    (attackTime : Float := 0.01) (decayTime : Float := 0.3)
    (Q : Float := 4.0) (sampleRate : Float := 44100.0)
    (sig : Signal Float) : Signal Float :=
  fun t =>
    let cutoff :=
      if t < attackTime then
        sustainCutoff + (peakCutoff - sustainCutoff) * (t / attackTime)
      else
        let decayT := t - attackTime
        let decayProgress := clamp 0.0 1.0 (decayT / decayTime)
        peakCutoff - (peakCutoff - sustainCutoff) * decayProgress
    let clampedCutoff := clamp 20.0 (sampleRate * 0.45) cutoff
    let coeffs := calcLowpassCoeffs clampedCutoff Q sampleRate
    biquad coeffs sampleRate sig |>.sample t

/-- Random modulation filter - cutoff wanders randomly.
    Creates evolving, organic textures.

    - baseCutoff: Center cutoff frequency
    - depth: Modulation depth (fraction of baseCutoff)
    - rate: How fast the modulation changes (Hz) -/
def randomFilter (baseCutoff : Float) (depth : Float) (rate : Float)
    (Q : Float := 2.0) (sampleRate : Float := 44100.0)
    (sig : Signal Float) : Signal Float :=
  fun t =>
    -- Simple pseudo-random using multiple sine waves at irrational ratios
    let r1 := Float.sin (twoPi * rate * t)
    let r2 := Float.sin (twoPi * rate * 1.618 * t)  -- Golden ratio
    let r3 := Float.sin (twoPi * rate * 2.236 * t)  -- sqrt(5)
    let rand := (r1 + r2 * 0.5 + r3 * 0.25) / 1.75
    let cutoff := baseCutoff * (1.0 + depth * rand)
    let clampedCutoff := clamp 20.0 (sampleRate * 0.45) cutoff
    let coeffs := calcLowpassCoeffs clampedCutoff Q sampleRate
    biquad coeffs sampleRate sig |>.sample t

/-- Formant filter - simulates vowel sounds using bandpass filters.
    Creates vocal-like filtering effects. -/
inductive Vowel
  | a  -- "ah" as in "father"
  | e  -- "eh" as in "bed"
  | i  -- "ee" as in "see"
  | o  -- "oh" as in "go"
  | u  -- "oo" as in "boot"
  deriving Repr, BEq

/-- Get formant frequencies for a vowel sound. -/
private def vowelFormants (v : Vowel) : Float × Float × Float :=
  match v with
  | .a => (800.0, 1200.0, 2500.0)
  | .e => (400.0, 2000.0, 2600.0)
  | .i => (250.0, 2200.0, 3000.0)
  | .o => (450.0, 800.0, 2500.0)
  | .u => (325.0, 700.0, 2500.0)

/-- Vowel formant filter.
    Applies three bandpass filters at formant frequencies. -/
def vowelFilter (v : Vowel) (Q : Float := 5.0) (sampleRate : Float := 44100.0)
    (sig : Signal Float) : Signal Float :=
  let (f1, f2, f3) := vowelFormants v
  fun t =>
    let bp1 := bandpass f1 Q sampleRate sig |>.sample t
    let bp2 := bandpass f2 Q sampleRate sig |>.sample t
    let bp3 := bandpass f3 Q sampleRate sig |>.sample t
    -- Mix formants with decreasing amplitudes
    bp1 * 0.5 + bp2 * 0.35 + bp3 * 0.15

end Fugue.Filter
