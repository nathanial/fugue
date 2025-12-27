/-
  Fugue.Effects.Modulation - Amplitude and pitch modulation effects

  Effects that periodically vary amplitude (tremolo) or pitch (vibrato).
-/
import Fugue.Core.Signal
import Fugue.Osc.Sine

namespace Fugue.Effects

open Fugue
open Fugue.Osc (twoPi)

/-- Tremolo - periodic amplitude modulation with unipolar LFO.
    - rate: Modulation frequency in Hz (typical: 3-10 Hz)
    - depth: Modulation amount (0.0 = none, 1.0 = full)
    The signal volume varies smoothly but never inverts phase. -/
@[inline]
def tremolo (rate : Float := 5.0) (depth : Float := 0.5)
    (sig : Signal Float) : Signal Float :=
  fun t =>
    -- Unipolar LFO: oscillates between (1-depth) and 1
    let lfo := 1.0 - depth * 0.5 * (1.0 + Float.sin (twoPi * rate * t))
    sig.sample t * lfo

/-- Tremolo with triangle wave LFO for more angular modulation. -/
@[inline]
def tremoloTriangle (rate : Float := 5.0) (depth : Float := 0.5)
    (sig : Signal Float) : Signal Float :=
  fun t =>
    let phase := (t * rate) - Float.floor (t * rate)
    let tri := if phase < 0.5 then 4.0 * phase - 1.0 else 3.0 - 4.0 * phase
    let lfo := 1.0 - depth * 0.5 * (1.0 + tri)
    sig.sample t * lfo

/-- Tremolo with square wave LFO for choppy gating effect. -/
@[inline]
def tremoloSquare (rate : Float := 5.0) (depth : Float := 0.5)
    (sig : Signal Float) : Signal Float :=
  fun t =>
    let phase := (t * rate) - Float.floor (t * rate)
    let sq := if phase < 0.5 then 1.0 else -1.0
    let lfo := 1.0 - depth * 0.5 * (1.0 + sq)
    sig.sample t * lfo

/-- Tremolo with custom LFO signal.
    The LFO should produce values in [-1, 1] range. -/
@[inline]
def tremoloWith (lfo : Signal Float) (depth : Float := 0.5)
    (sig : Signal Float) : Signal Float :=
  fun t =>
    let modulation := 1.0 - depth * 0.5 * (1.0 + lfo.sample t)
    sig.sample t * modulation

/-- Vibrato - pitch modulation via time shifting.
    - rate: Modulation frequency in Hz (typical: 4-8 Hz)
    - depth: Maximum time shift in seconds (typical: 0.001-0.005)
    Creates subtle pitch wobble by varying playback position. -/
@[inline]
def vibrato (rate : Float := 5.0) (depth : Float := 0.003)
    (sig : Signal Float) : Signal Float :=
  fun t =>
    let modulation := Float.sin (twoPi * rate * t) * depth
    let modulatedT := t + modulation
    if modulatedT >= 0.0 then sig.sample modulatedT else 0.0

/-- Ring modulation - multiply by bipolar carrier frequency.
    Creates sum and difference frequencies (metallic, bell-like sounds).
    - carrierFreq: Modulator frequency in Hz -/
@[inline]
def ringMod (carrierFreq : Float) (sig : Signal Float) : Signal Float :=
  fun t =>
    let carrier := Float.sin (twoPi * carrierFreq * t)
    sig.sample t * carrier

/-- Amplitude modulation with modulator signal.
    Similar to ring mod but uses any signal as modulator. -/
@[inline]
def am (modulator : Signal Float) (sig : Signal Float) : Signal Float :=
  fun t => sig.sample t * modulator.sample t

/-- Auto-pan simulation - returns panned signal.
    Since we're mono, this creates a volume sweep effect.
    For stereo, would return (left, right) pair. -/
@[inline]
def autoPan (rate : Float := 1.0) (sig : Signal Float) : Signal Float :=
  fun t =>
    -- Simulate pan by fading in/out with slow LFO
    let pan := 0.5 * (1.0 + Float.sin (twoPi * rate * t))
    sig.sample t * pan

/-- Frequency modulation helper.
    Modulates the phase of a carrier based on modulator.
    - modIndex: Modulation depth (higher = more sidebands)
    - carrierFreq: Base carrier frequency -/
def fm (modulator : Signal Float) (modIndex : Float) (carrierFreq : Float) : Signal Float :=
  fun t =>
    let modValue := modulator.sample t * modIndex
    Float.sin (twoPi * carrierFreq * t + modValue)

end Fugue.Effects
