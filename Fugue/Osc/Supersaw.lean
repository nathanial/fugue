/-
  Fugue.Osc.Supersaw - Multiple detuned sawtooth oscillators

  Creates thick, rich sounds by layering multiple slightly-detuned
  sawtooth waves. Classic trance/EDM sound.
-/
import Fugue.Core.Signal

namespace Fugue.Osc

open Fugue

/-- Supersaw: Multiple detuned sawtooth waves for thick, lush sound.
    - freq: Base frequency in Hz
    - detune: Detuning amount in cents (100 cents = 1 semitone)
    - voices: Number of oscillators (odd number recommended for center voice)

    The voices are spread symmetrically around the base frequency. -/
def supersaw (freq : Float) (detune : Float := 20.0) (voices : Nat := 7) : Signal Float :=
  fun t => Id.run do
    if voices == 0 then return 0.0
    let mut sum := 0.0
    for i in [:voices] do
      -- Spread detuning symmetrically: center voice at 0, others spread out
      let offset := if voices == 1 then 0.0
        else (i.toFloat - (voices.toFloat - 1.0) / 2.0) / ((voices.toFloat - 1.0) / 2.0)
      let cents := offset * detune
      let detuneRatio := Float.pow 2.0 (cents / 1200.0)
      let voiceFreq := freq * detuneRatio
      -- Sawtooth formula: phase in [0,1) maps to [-1,1)
      let phase := (t * voiceFreq) - Float.floor (t * voiceFreq)
      sum := sum + (2.0 * phase - 1.0)
    sum / voices.toFloat

/-- Supersaw configuration for fine-grained control. -/
structure SupersawConfig where
  /-- Detuning amount in cents (100 cents = 1 semitone) -/
  detune : Float := 20.0
  /-- Number of voices -/
  voices : Nat := 7
  /-- Spread factor (1.0 = normal, <1.0 = tighter, >1.0 = wider) -/
  spread : Float := 1.0
  /-- Mix between center voice and detuned (0.0 = all center, 1.0 = all detuned) -/
  mix : Float := 0.7
  deriving Repr, Inhabited

/-- Supersaw with configuration struct. -/
def supersawConfig (config : SupersawConfig := {}) (freq : Float) : Signal Float :=
  fun t => Id.run do
    if config.voices == 0 then return 0.0

    -- Center voice (undetuned)
    let centerPhase := (t * freq) - Float.floor (t * freq)
    let centerSaw := 2.0 * centerPhase - 1.0

    -- Detuned voices
    let mut detunedSum := 0.0
    let detuneVoices := config.voices - 1
    if detuneVoices > 0 then
      for i in [:detuneVoices] do
        let offset := (i.toFloat - (detuneVoices.toFloat - 1.0) / 2.0) /
          (if detuneVoices == 1 then 1.0 else (detuneVoices.toFloat - 1.0) / 2.0)
        let cents := offset * config.detune * config.spread
        let detuneRatio := Float.pow 2.0 (cents / 1200.0)
        let voiceFreq := freq * detuneRatio
        let phase := (t * voiceFreq) - Float.floor (t * voiceFreq)
        detunedSum := detunedSum + (2.0 * phase - 1.0)
      detunedSum := detunedSum / detuneVoices.toFloat

    -- Mix center and detuned
    centerSaw * (1.0 - config.mix) + detunedSum * config.mix

/-- Supersaw with modulated detuning for movement/animation. -/
def supersawMod (freq : Float) (detuneSig : Signal Float) (voices : Nat := 7) : Signal Float :=
  fun t =>
    let detune := detuneSig.sample t
    supersaw freq detune voices |>.sample t

/-- Hypersaw: Even more voices for extreme thickness.
    Uses 15 voices with wider detuning. -/
def hypersaw (freq : Float) (detune : Float := 30.0) : Signal Float :=
  supersaw freq detune 15

/-- Unison oscillator: Multiple slightly detuned voices of any waveform.
    Generic version that works with any oscillator function. -/
def unison (osc : Float → Signal Float) (freq : Float)
    (detune : Float := 15.0) (voices : Nat := 5) : Signal Float :=
  fun t => Id.run do
    if voices == 0 then return 0.0
    let mut sum := 0.0
    for i in [:voices] do
      let offset := if voices == 1 then 0.0
        else (i.toFloat - (voices.toFloat - 1.0) / 2.0) / ((voices.toFloat - 1.0) / 2.0)
      let cents := offset * detune
      let detuneRatio := Float.pow 2.0 (cents / 1200.0)
      let voiceFreq := freq * detuneRatio
      sum := sum + (osc voiceFreq).sample t
    sum / voices.toFloat

/-- Detuned square waves (PWM-like thickness). -/
def supersquare (freq : Float) (detune : Float := 15.0) (voices : Nat := 5) : Signal Float :=
  fun t => Id.run do
    if voices == 0 then return 0.0
    let mut sum := 0.0
    for i in [:voices] do
      let offset := if voices == 1 then 0.0
        else (i.toFloat - (voices.toFloat - 1.0) / 2.0) / ((voices.toFloat - 1.0) / 2.0)
      let cents := offset * detune
      let detuneRatio := Float.pow 2.0 (cents / 1200.0)
      let voiceFreq := freq * detuneRatio
      let phase := (t * voiceFreq) - Float.floor (t * voiceFreq)
      sum := sum + (if phase < 0.5 then 1.0 else -1.0)
    sum / voices.toFloat

/-- Detuned pulse waves with variable width. -/
def superpulse (freq : Float) (duty : Float := 0.5)
    (detune : Float := 15.0) (voices : Nat := 5) : Signal Float :=
  fun t => Id.run do
    if voices == 0 then return 0.0
    let mut sum := 0.0
    for i in [:voices] do
      let offset := if voices == 1 then 0.0
        else (i.toFloat - (voices.toFloat - 1.0) / 2.0) / ((voices.toFloat - 1.0) / 2.0)
      let cents := offset * detune
      let detuneRatio := Float.pow 2.0 (cents / 1200.0)
      let voiceFreq := freq * detuneRatio
      let phase := (t * voiceFreq) - Float.floor (t * voiceFreq)
      sum := sum + (if phase < duty then 1.0 else -1.0)
    sum / voices.toFloat

end Fugue.Osc
