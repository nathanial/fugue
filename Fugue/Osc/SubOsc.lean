/-
  Fugue.Osc.SubOsc - Sub-oscillator for bass reinforcement

  Generates tones octaves below the fundamental frequency.
  Essential for fat bass sounds, synth leads, and 808-style kicks.
-/
import Fugue.Core.Signal
import Fugue.Osc.Sine
import Fugue.Osc.Square
import Fugue.Osc.Sawtooth
import Fugue.Osc.Triangle

namespace Fugue.Osc

open Fugue

/-- Sub-oscillator sine wave: pure sine octaves below fundamental.
    - freq: Fundamental frequency in Hz
    - octaves: How many octaves below (1 = half freq, 2 = quarter freq) -/
def subSine (freq : Float) (octaves : Nat := 1) : Signal Float :=
  let subFreq := freq / Float.pow 2.0 octaves.toFloat
  sine subFreq

/-- Sub-oscillator square wave: punchy, defined sub.
    - freq: Fundamental frequency in Hz
    - octaves: How many octaves below -/
def subSquare (freq : Float) (octaves : Nat := 1) : Signal Float :=
  let subFreq := freq / Float.pow 2.0 octaves.toFloat
  square subFreq

/-- Sub-oscillator sawtooth wave: harmonically rich sub.
    - freq: Fundamental frequency in Hz
    - octaves: How many octaves below -/
def subSaw (freq : Float) (octaves : Nat := 1) : Signal Float :=
  let subFreq := freq / Float.pow 2.0 octaves.toFloat
  sawtooth subFreq

/-- Sub-oscillator triangle wave: soft, mellow sub.
    - freq: Fundamental frequency in Hz
    - octaves: How many octaves below -/
def subTriangle (freq : Float) (octaves : Nat := 1) : Signal Float :=
  let subFreq := freq / Float.pow 2.0 octaves.toFloat
  triangle subFreq

/-- Mix any oscillator with a sub-oscillator.
    - fundamental: The main oscillator signal
    - freq: Frequency used for the sub (should match fundamental)
    - subLevel: Sub-oscillator mix level (0.0-1.0)
    - octaves: How many octaves below for the sub -/
def withSub (fundamental : Signal Float) (freq : Float)
    (subLevel : Float := 0.5) (octaves : Nat := 1) : Signal Float :=
  let sub := subSine freq octaves
  fun t =>
    let fundVal := fundamental.sample t
    let subVal := sub.sample t
    -- Normalize to prevent clipping
    (fundVal + subLevel * subVal) / (1.0 + subLevel)

/-- Mix oscillator with square sub (more defined bass). -/
def withSquareSub (fundamental : Signal Float) (freq : Float)
    (subLevel : Float := 0.5) (octaves : Nat := 1) : Signal Float :=
  let sub := subSquare freq octaves
  fun t =>
    let fundVal := fundamental.sample t
    let subVal := sub.sample t
    (fundVal + subLevel * subVal) / (1.0 + subLevel)

/-- Classic bass patch: sawtooth lead + square sub.
    Great for bass lines and synth bass. -/
def bassPatch (freq : Float) (subLevel : Float := 0.5) : Signal Float :=
  let lead := sawtooth freq
  let sub := subSquare freq 1
  fun t =>
    let leadVal := lead.sample t
    let subVal := sub.sample t
    (leadVal + subLevel * subVal) / (1.0 + subLevel)

/-- Reese bass: two slightly detuned saws + sub.
    Classic DnB/dubstep sound. -/
def reeseBass (freq : Float) (detune : Float := 5.0) (subLevel : Float := 0.4) : Signal Float :=
  let detuneRatio := Float.pow 2.0 (detune / 1200.0)
  let saw1 := sawtooth freq
  let saw2 := sawtooth (freq * detuneRatio)
  let sub := subSine freq 1
  fun t =>
    let v1 := saw1.sample t
    let v2 := saw2.sample t
    let subVal := sub.sample t
    let leadMix := (v1 + v2) / 2.0
    (leadMix + subLevel * subVal) / (1.0 + subLevel)

/-- 808-style sub bass: pure sine sub with gentle attack shape.
    The envelope should be applied separately for proper 808 behavior.
    This just generates the tone. -/
def sub808 (freq : Float) : Signal Float :=
  -- 808 subs are typically one octave below the note
  -- with a slight pitch drop at the start (implemented via external envelope)
  subSine freq 1

/-- 808 kick with pitch envelope built-in.
    Starts higher and drops to target frequency.
    - freq: Target bass frequency
    - pitchDecay: How fast pitch drops (in seconds) -/
def sub808Pitched (freq : Float) (pitchDecay : Float := 0.05) : Signal Float :=
  fun t =>
    -- Exponential pitch drop from 4x freq to target
    let pitchMult := 1.0 + 3.0 * Float.exp (-t / pitchDecay)
    let currentFreq := freq * pitchMult
    let phase := (t * currentFreq) - Float.floor (t * currentFreq)
    Float.sin (twoPi * phase)

/-- Layered sub: multiple octaves for massive low end.
    - freq: Fundamental frequency
    - levels: Volume levels for each octave [oct1, oct2, oct3...] -/
def layeredSub (freq : Float) (levels : List Float := [0.7, 0.3]) : Signal Float :=
  fun t =>
    let total := levels.foldl (init := 0.0) fun acc level =>
      acc + level
    if total == 0.0 then 0.0
    else
      -- Manually track index with a fold over (index, sum) pair
      let (_, sum) := levels.foldl (init := (0, 0.0)) fun (idx, acc) level =>
        let octaves := idx + 1
        let subFreq := freq / Float.pow 2.0 octaves.toFloat
        let phase := (t * subFreq) - Float.floor (t * subFreq)
        (idx + 1, acc + level * Float.sin (twoPi * phase))
      sum / total

/-- Sub-bass configuration for fine-grained control. -/
structure SubConfig where
  /-- Octaves below fundamental -/
  octaves : Nat := 1
  /-- Sub oscillator waveform: "sine", "square", "saw", "triangle" -/
  waveform : String := "sine"
  /-- Mix level (0.0 = no sub, 1.0 = equal to fundamental) -/
  level : Float := 0.5
  deriving Repr, Inhabited

/-- Get sub oscillator based on config waveform. -/
private def subOscFor (config : SubConfig) (freq : Float) : Signal Float :=
  match config.waveform with
  | "square" => subSquare freq config.octaves
  | "saw" => subSaw freq config.octaves
  | "triangle" => subTriangle freq config.octaves
  | _ => subSine freq config.octaves

/-- Mix fundamental with sub using configuration. -/
def withSubConfig (config : SubConfig := {}) (fundamental : Signal Float)
    (freq : Float) : Signal Float :=
  let sub := subOscFor config freq
  fun t =>
    let fundVal := fundamental.sample t
    let subVal := sub.sample t
    (fundVal + config.level * subVal) / (1.0 + config.level)

/-- Polyphonic sub: adds sub to each note in a chord.
    - freqs: List of frequencies for each note
    - osc: Oscillator function to use for each note
    - subLevel: Sub-oscillator level -/
def polySub (freqs : List Float) (osc : Float → Signal Float)
    (subLevel : Float := 0.3) : Signal Float :=
  fun t =>
    if freqs.isEmpty then 0.0
    else
      let sum := freqs.foldl (init := 0.0) fun acc freq =>
        let note := osc freq
        let sub := subSine freq 1
        let noteVal := note.sample t
        let subVal := sub.sample t
        acc + (noteVal + subLevel * subVal)
      sum / (freqs.length.toFloat * (1.0 + subLevel))

end Fugue.Osc
