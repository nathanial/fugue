/-
  Fugue.Effects.Delay - Echo and delay effects

  Time-based effects that create echoes and repetitions.
-/
import Fugue.Core.Signal
import Fugue.Osc.Sine

namespace Fugue.Effects

open Fugue
open Fugue.Osc (twoPi)

/-- Simple delay - single echo at specified time offset.
    Values before the delay time are zero (silence). -/
@[inline]
def delay (delayTime : Float) (sig : Signal Float) : Signal Float :=
  fun t =>
    if t < delayTime then 0.0
    else sig.sample (t - delayTime)

/-- Delay with feedback - multiple decaying echoes.
    - delayTime: Time between echoes in seconds
    - feedback: Decay factor per echo (0.0 to <1.0)
    - maxEchoes: Maximum number of echoes to compute
    Uses finite unrolling since feedback < 1.0 causes exponential decay. -/
def delayWithFeedback (delayTime : Float) (feedback : Float) (maxEchoes : Nat := 10)
    (sig : Signal Float) : Signal Float :=
  fun t => Id.run do
    let mut sum := sig.sample t  -- Dry signal
    for i in [1:maxEchoes + 1] do
      let echoTime := t - (i.toFloat * delayTime)
      if echoTime >= 0.0 then
        let attenuation := Float.pow feedback i.toFloat
        sum := sum + sig.sample echoTime * attenuation
    sum

/-- Delay with feedback and wet/dry mix.
    - mix: 0.0 = all dry, 1.0 = all wet -/
def delayMix (delayTime : Float) (feedback : Float) (mix : Float := 0.5)
    (maxEchoes : Nat := 10) (sig : Signal Float) : Signal Float :=
  fun t => Id.run do
    let dry := sig.sample t
    let mut wet := 0.0
    for i in [1:maxEchoes + 1] do
      let echoTime := t - (i.toFloat * delayTime)
      if echoTime >= 0.0 then
        let attenuation := Float.pow feedback i.toFloat
        wet := wet + sig.sample echoTime * attenuation
    dry * (1.0 - mix) + wet * mix

/-- Slapback delay - single short echo typical of rockabilly.
    Fixed short delay time around 75-250ms with no feedback. -/
@[inline]
def slapback (delayTime : Float := 0.12) (level : Float := 0.7)
    (sig : Signal Float) : Signal Float :=
  fun t =>
    let dry := sig.sample t
    let wet := if t >= delayTime then sig.sample (t - delayTime) * level else 0.0
    dry + wet

/-- Modulated delay - delay time varies with LFO.
    Creates chorus-like effect when combined with dry signal.
    - baseDelay: Center delay time in seconds
    - modDepth: Modulation depth in seconds
    - modFreq: LFO frequency in Hz -/
@[inline]
def modulatedDelay (baseDelay : Float) (modDepth : Float) (modFreq : Float)
    (sig : Signal Float) : Signal Float :=
  fun t =>
    let modulation := Float.sin (twoPi * modFreq * t) * modDepth
    let currentDelay := baseDelay + modulation
    if t >= currentDelay then sig.sample (t - currentDelay) else 0.0

/-- Multi-tap delay - multiple echoes at different times.
    - taps: List of (delay time, level) pairs -/
def multiTapDelay (taps : List (Float × Float)) (sig : Signal Float) : Signal Float :=
  fun t =>
    let dry := sig.sample t
    let echoes := taps.foldl (fun acc (delayTime, level) =>
      if t >= delayTime then acc + sig.sample (t - delayTime) * level
      else acc) 0.0
    dry + echoes

/-- Ping-pong delay simulation (mono version).
    Alternates echo levels for stereo-like effect.
    In true stereo, echoes would alternate L/R. -/
def pingPongDelay (delayTime : Float) (feedback : Float) (maxEchoes : Nat := 8)
    (sig : Signal Float) : Signal Float :=
  fun t => Id.run do
    let mut sum := sig.sample t
    for i in [1:maxEchoes + 1] do
      let echoTime := t - (i.toFloat * delayTime)
      if echoTime >= 0.0 then
        let attenuation := Float.pow feedback i.toFloat
        -- Alternate between full and reduced level to simulate L/R
        let panFactor := if i % 2 == 0 then 0.7 else 1.0
        sum := sum + sig.sample echoTime * attenuation * panFactor
    sum

/-- Ducking delay - echo level drops when input is present.
    Creates space for the main signal. -/
def duckingDelay (delayTime : Float) (feedback : Float) (threshold : Float := 0.3)
    (maxEchoes : Nat := 8) (sig : Signal Float) : Signal Float :=
  fun t => Id.run do
    let dry := sig.sample t
    let dryLevel := if dry > 0.0 then dry else -dry
    -- Duck the wet signal when dry is loud
    let duckAmount := if dryLevel > threshold then 0.3 else 1.0
    let mut wet := 0.0
    for i in [1:maxEchoes + 1] do
      let echoTime := t - (i.toFloat * delayTime)
      if echoTime >= 0.0 then
        let attenuation := Float.pow feedback i.toFloat
        wet := wet + sig.sample echoTime * attenuation
    dry + wet * duckAmount

end Fugue.Effects
