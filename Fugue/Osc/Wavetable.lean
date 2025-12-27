/-
  Fugue.Osc.Wavetable - Wavetable synthesis

  Lookup-based oscillator that can play any waveform stored in a table.
  Supports linear interpolation and morphing between tables.
-/
import Fugue.Core.Signal
import Fugue.Osc.Sine

namespace Fugue.Osc

open Fugue

/-- Wavetable: Array of waveform samples for lookup-based synthesis.
    One complete cycle is stored, indexed by phase [0, 1). -/
structure Wavetable where
  samples : FloatArray
  deriving Inhabited

/-- Get the size of the wavetable. -/
def Wavetable.size (wt : Wavetable) : Nat := wt.samples.size

/-- Create wavetable from a periodic function.
    The function should map [0, 1) to [-1, 1]. -/
def Wavetable.fromFunction (f : Float → Float) (size : Nat := 256) : Wavetable :=
  let samples := Id.run do
    let mut arr := FloatArray.empty
    for i in [:size] do
      let phase := i.toFloat / size.toFloat
      arr := arr.push (f phase)
    arr
  { samples := samples }

/-- Pre-built sine wavetable. -/
def Wavetable.sine (size : Nat := 256) : Wavetable :=
  Wavetable.fromFunction (fun phase => Float.sin (twoPi * phase)) size

/-- Pre-built square wavetable. -/
def Wavetable.square (size : Nat := 256) : Wavetable :=
  Wavetable.fromFunction (fun phase => if phase < 0.5 then 1.0 else -1.0) size

/-- Pre-built sawtooth wavetable. -/
def Wavetable.sawtooth (size : Nat := 256) : Wavetable :=
  Wavetable.fromFunction (fun phase => 2.0 * phase - 1.0) size

/-- Pre-built triangle wavetable. -/
def Wavetable.triangle (size : Nat := 256) : Wavetable :=
  Wavetable.fromFunction (fun phase =>
    if phase < 0.5 then
      4.0 * phase - 1.0
    else
      3.0 - 4.0 * phase) size

/-- Pre-built pulse wavetable with configurable duty cycle. -/
def Wavetable.pulse (duty : Float := 0.5) (size : Nat := 256) : Wavetable :=
  Wavetable.fromFunction (fun phase => if phase < duty then 1.0 else -1.0) size

/-- Sample a wavetable with linear interpolation.
    Phase should be in [0, 1). -/
def Wavetable.sampleAt (wt : Wavetable) (phase : Float) : Float :=
  if wt.samples.size == 0 then 0.0
  else
    let indexF := phase * wt.samples.size.toFloat
    let idx := indexF.toUInt64.toNat
    let frac := indexF - idx.toFloat
    let i0 := idx % wt.samples.size
    let i1 := (idx + 1) % wt.samples.size
    let s0 := wt.samples.get! i0
    let s1 := wt.samples.get! i1
    s0 + frac * (s1 - s0)

/-- Wavetable oscillator with linear interpolation.
    - wt: The wavetable to use
    - freq: Frequency in Hz -/
def wavetable (wt : Wavetable) (freq : Float) : Signal Float :=
  fun t =>
    let phase := (t * freq) - Float.floor (t * freq)
    wt.sampleAt phase

/-- Wavetable oscillator with phase offset.
    - phase: Phase offset in [0, 1] -/
def wavetablePhase (wt : Wavetable) (freq : Float) (phase : Float) : Signal Float :=
  fun t =>
    let p := (t * freq + phase) - Float.floor (t * freq + phase)
    wt.sampleAt p

/-- Morph between two wavetables.
    - morphAmount: 0.0 = first table, 1.0 = second table -/
def wavetableMorph (wt1 wt2 : Wavetable) (morphAmount : Float) (freq : Float) : Signal Float :=
  fun t =>
    let phase := (t * freq) - Float.floor (t * freq)
    let s1 := wt1.sampleAt phase
    let s2 := wt2.sampleAt phase
    s1 * (1.0 - morphAmount) + s2 * morphAmount

/-- Morph with time-varying morph amount. -/
def wavetableMorphSig (wt1 wt2 : Wavetable) (morphSig : Signal Float) (freq : Float) : Signal Float :=
  fun t =>
    let phase := (t * freq) - Float.floor (t * freq)
    let morph := morphSig.sample t
    let s1 := wt1.sampleAt phase
    let s2 := wt2.sampleAt phase
    s1 * (1.0 - morph) + s2 * morph

/-- Wavetable with frequency modulation. -/
def wavetableFM (wt : Wavetable) (freqSig : Signal Float) : Signal Float :=
  fun t =>
    let freq := freqSig.sample t
    let phase := (t * freq) - Float.floor (t * freq)
    wt.sampleAt phase

/-- Create a wavetable from harmonic content (additive synthesis).
    Harmonics are specified as (harmonic number, amplitude) pairs. -/
def Wavetable.fromHarmonics (harmonics : List (Nat × Float)) (size : Nat := 256) : Wavetable :=
  Wavetable.fromFunction (fun phase =>
    harmonics.foldl (fun acc (n, amp) =>
      acc + amp * Float.sin (twoPi * n.toFloat * phase)
    ) 0.0
  ) size

/-- Create a "bright" wavetable with many harmonics. -/
def Wavetable.bright (numHarmonics : Nat := 16) (size : Nat := 256) : Wavetable :=
  let harmonics := List.range numHarmonics |>.map fun i =>
    (i + 1, 1.0 / (i + 1).toFloat)
  Wavetable.fromHarmonics harmonics size

/-- Create a wavetable with only odd harmonics (hollow sound). -/
def Wavetable.hollow (numHarmonics : Nat := 8) (size : Nat := 256) : Wavetable :=
  let harmonics := List.range numHarmonics |>.map fun i =>
    (2 * i + 1, 1.0 / (2 * i + 1).toFloat)
  Wavetable.fromHarmonics harmonics size

end Fugue.Osc
