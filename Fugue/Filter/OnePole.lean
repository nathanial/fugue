/-
  Fugue.Filter.OnePole - Simple one-pole filters

  One-pole filters provide gentle 6dB/octave rolloff.
  They're computationally efficient and useful as building blocks.
-/
import Fugue.Core.Signal
import Fugue.Osc.Sine

namespace Fugue.Filter

open Fugue
open Fugue.Osc (twoPi)

/-- One-pole lowpass filter (6dB/octave rolloff).
    Uses exponential smoothing: y[n] = b*x[n] + a*y[n-1]
    where a = exp(-2π * cutoff * T), b = 1 - a

    - cutoff: -3dB frequency in Hz
    - sampleRate: Sample rate in Hz (default: 44100) -/
def lowpass1 (cutoff : Float) (sampleRate : Float := 44100.0)
    (sig : Signal Float) : Signal Float :=
  let T := 1.0 / sampleRate
  let a := Float.exp (-twoPi * cutoff * T)
  let b := 1.0 - a
  fun t =>
    let x := sig.sample t
    -- Use previous input as proxy for previous output (stable approximation)
    let xPrev := if t >= T then sig.sample (t - T) else x
    b * x + a * xPrev

/-- One-pole highpass filter (6dB/octave rolloff).
    Implemented as: HP = input - LP

    - cutoff: -3dB frequency in Hz
    - sampleRate: Sample rate in Hz -/
def highpass1 (cutoff : Float) (sampleRate : Float := 44100.0)
    (sig : Signal Float) : Signal Float :=
  let lp := lowpass1 cutoff sampleRate sig
  fun t => sig.sample t - lp.sample t

/-- Cascaded one-pole lowpass for steeper rolloff.
    - order=1: 6dB/octave
    - order=2: 12dB/octave
    - order=4: 24dB/octave

    Each additional pole adds 6dB/octave rolloff. -/
def lowpassN (cutoff : Float) (order : Nat) (sampleRate : Float := 44100.0)
    (sig : Signal Float) : Signal Float :=
  match order with
  | 0 => sig
  | 1 => lowpass1 cutoff sampleRate sig
  | n + 1 => lowpass1 cutoff sampleRate (lowpassN cutoff n sampleRate sig)

/-- Cascaded one-pole highpass for steeper rolloff. -/
def highpassN (cutoff : Float) (order : Nat) (sampleRate : Float := 44100.0)
    (sig : Signal Float) : Signal Float :=
  match order with
  | 0 => sig
  | 1 => highpass1 cutoff sampleRate sig
  | n + 1 => highpass1 cutoff sampleRate (highpassN cutoff n sampleRate sig)

/-- DC blocker - removes DC offset from signal.
    Uses a highpass filter at a very low frequency (5 Hz). -/
def dcBlock (sampleRate : Float := 44100.0) (sig : Signal Float) : Signal Float :=
  highpass1 5.0 sampleRate sig

/-- Simple moving average lowpass filter.
    Averages samples over a time window.

    - windowTime: Size of averaging window in seconds -/
def movingAverage (windowTime : Float) (sampleRate : Float := 44100.0)
    (sig : Signal Float) : Signal Float :=
  let T := 1.0 / sampleRate
  let numSamples := (windowTime * sampleRate).toUInt64.toNat
  let n := if numSamples < 1 then 1 else numSamples
  fun t => Id.run do
    let mut sum := 0.0
    for i in [:n] do
      let sampleT := t - i.toFloat * T
      if sampleT >= 0.0 then
        sum := sum + sig.sample sampleT
      else
        sum := sum + sig.sample 0.0
    sum / n.toFloat

/-- Exponential moving average with explicit smoothing factor.
    - alpha: Smoothing factor (0.0 to 1.0)
      - Lower alpha = more smoothing (slower response)
      - Higher alpha = less smoothing (faster response) -/
def ema (alpha : Float) (sampleRate : Float := 44100.0)
    (sig : Signal Float) : Signal Float :=
  let T := 1.0 / sampleRate
  fun t =>
    let x := sig.sample t
    let xPrev := if t >= T then sig.sample (t - T) else x
    alpha * x + (1.0 - alpha) * xPrev

/-- Convert cutoff frequency to smoothing alpha for EMA.
    This allows specifying EMA behavior in terms of frequency. -/
def cutoffToAlpha (cutoff : Float) (sampleRate : Float := 44100.0) : Float :=
  let T := 1.0 / sampleRate
  1.0 - Float.exp (-twoPi * cutoff * T)

/-- Slew rate limiter - limits how fast a signal can change.
    Useful for smoothing control signals.

    - riseRate: Maximum rise rate per second
    - fallRate: Maximum fall rate per second -/
def slewLimit (riseRate : Float) (fallRate : Float) (sampleRate : Float := 44100.0)
    (sig : Signal Float) : Signal Float :=
  let T := 1.0 / sampleRate
  let maxRise := riseRate * T
  let maxFall := fallRate * T
  fun t =>
    let current := sig.sample t
    if t < T then
      current
    else
      let prev := sig.sample (t - T)
      let diff := current - prev
      if diff > maxRise then
        prev + maxRise
      else if diff < -maxFall then
        prev - maxFall
      else
        current

/-- Symmetric slew limiter with same rise and fall rate. -/
def slew (rate : Float) (sampleRate : Float := 44100.0)
    (sig : Signal Float) : Signal Float :=
  slewLimit rate rate sampleRate sig

/-- Lag filter - one-pole lowpass with time constant.
    Easier to think about in terms of response time.

    - lagTime: Time constant in seconds (time to reach ~63% of target) -/
def lag (lagTime : Float) (sampleRate : Float := 44100.0)
    (sig : Signal Float) : Signal Float :=
  -- cutoff = 1 / (2π * lagTime)
  let cutoff := 1.0 / (twoPi * lagTime)
  lowpass1 cutoff sampleRate sig

end Fugue.Filter
