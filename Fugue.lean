/-
  Fugue - Sound Synthesis Library for Lean 4

  An algebra of sounds where signals are first-class values that compose naturally.

  ## Quick Start

  ```lean
  import Fugue

  open Fugue Osc Env Combine Render FFI

  def main : IO Unit := do
    -- Create an ADSR envelope
    let env := ADSR.mk (attack := 0.02) (decay := 0.1) (sustain := 0.7) (release := 0.3)

    -- Create a note with envelope
    let note := applyEnvelope env (sine 440.0)

    -- Render to samples and play
    let samples := renderDSignal cdQuality note
    let player ← AudioPlayer.create 44100.0
    player.play samples
  ```

  ## Core Concepts

  - `Signal α`: A function from time (Float) to a value
  - `DSignal α`: A signal with known finite duration
  - `Audio`: Alias for `Signal Float` (values in [-1, 1])

  ## Modules

  - `Fugue.Core`: Signal and DSignal types
  - `Fugue.Osc`: Oscillators (sine, square, sawtooth, triangle, noise, wavetable, supersaw, sub)
  - `Fugue.Combine`: Mixing, scaling, sequencing
  - `Fugue.Env`: ADSR envelopes
  - `Fugue.Effects`: Audio effects (distortion, delay, reverb, chorus, modulation)
  - `Fugue.Filter`: Digital filters (lowpass, highpass, bandpass, resonant, enveloped)
  - `Fugue.Theory`: Music theory (MIDI, notes, scales, chords, tempo)
  - `Fugue.Render`: Signal to sample buffer conversion
  - `Fugue.FFI`: macOS audio playback
-/

-- Core types
import Fugue.Core.Signal
import Fugue.Core.Duration

-- Oscillators
import Fugue.Osc.Sine
import Fugue.Osc.Square
import Fugue.Osc.Sawtooth
import Fugue.Osc.Triangle
import Fugue.Osc.Noise
import Fugue.Osc.Wavetable
import Fugue.Osc.Supersaw
import Fugue.Osc.SubOsc

-- Combinators
import Fugue.Combine.Mix
import Fugue.Combine.Scale
import Fugue.Combine.Sequence

-- Envelopes
import Fugue.Env.ADSR

-- Effects
import Fugue.Effects.Distortion
import Fugue.Effects.Modulation
import Fugue.Effects.Delay
import Fugue.Effects.Chorus
import Fugue.Effects.Reverb

-- Filters
import Fugue.Filter.OnePole
import Fugue.Filter.Biquad
import Fugue.Filter.Envelope

-- Music Theory
import Fugue.Theory.Note
import Fugue.Theory.Scale
import Fugue.Theory.Chord
import Fugue.Theory.Tempo

-- Rendering
import Fugue.Render.Config
import Fugue.Render.Render

-- FFI (audio playback)
import Fugue.FFI.Types
import Fugue.FFI.AudioQueue
