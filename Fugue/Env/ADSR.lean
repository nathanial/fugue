/-
  Fugue.Env.ADSR - ADSR Envelope Generator

  Attack-Decay-Sustain-Release envelope for shaping sound dynamics.
-/
import Fugue.Core.Signal
import Fugue.Core.Duration

namespace Fugue.Env

/-- ADSR (Attack-Decay-Sustain-Release) envelope.

    - attack: Time to rise from 0 to 1 (seconds)
    - decay: Time to fall from 1 to sustain level (seconds)
    - sustain: Level to hold at during sustain phase (0 to 1)
    - release: Time to fall from sustain to 0 (seconds)

    Total duration = attack + decay + release
    (sustain is a level, not a time - the note holds at this level
    until release begins)
-/
structure ADSR where
  attack  : Float  -- Attack time (seconds)
  decay   : Float  -- Decay time (seconds)
  sustain : Float  -- Sustain level (0 to 1)
  release : Float  -- Release time (seconds)
  deriving Repr, Inhabited

namespace ADSR

/-- Create an ADSR envelope with named parameters. -/
def create (attack : Float := 0.01) (decay : Float := 0.1)
           (sustain : Float := 0.7) (release : Float := 0.2) : ADSR :=
  { attack, decay, sustain, release }

/-- Quick attack envelope (for percussive sounds). -/
def percussive (decay : Float := 0.3) : ADSR :=
  create (attack := 0.001) (decay := decay) (sustain := 0.0) (release := 0.01)

/-- Pad envelope (slow attack and release). -/
def pad (attack : Float := 0.5) (release : Float := 1.0) : ADSR :=
  create (attack := attack) (decay := 0.2) (sustain := 0.8) (release := release)

/-- Pluck envelope (fast attack, medium decay). -/
def pluck (decay : Float := 0.5) : ADSR :=
  create (attack := 0.005) (decay := decay) (sustain := 0.3) (release := 0.1)

/-- Total duration of the envelope (without sustain hold time). -/
def duration (env : ADSR) : Float :=
  env.attack + env.decay + env.release

/-- Sample the envelope at time t.
    This assumes the note is held through attack+decay, then released. -/
def sample (env : ADSR) (t : Float) : Float :=
  if t < 0.0 then
    0.0
  else if t < env.attack then
    -- Attack phase: rise from 0 to 1
    t / env.attack
  else if t < env.attack + env.decay then
    -- Decay phase: fall from 1 to sustain level
    let decayProgress := (t - env.attack) / env.decay
    1.0 - (1.0 - env.sustain) * decayProgress
  else if t < env.attack + env.decay + env.release then
    -- Release phase: fall from sustain to 0
    let releaseProgress := (t - env.attack - env.decay) / env.release
    env.sustain * (1.0 - releaseProgress)
  else
    0.0

/-- Sample the envelope with a custom sustain hold time.
    sustainTime: how long to hold at sustain level before release. -/
def sampleWithHold (env : ADSR) (sustainTime : Float) (t : Float) : Float :=
  if t < 0.0 then
    0.0
  else if t < env.attack then
    t / env.attack
  else if t < env.attack + env.decay then
    let decayProgress := (t - env.attack) / env.decay
    1.0 - (1.0 - env.sustain) * decayProgress
  else if t < env.attack + env.decay + sustainTime then
    -- Sustain phase: hold at sustain level
    env.sustain
  else if t < env.attack + env.decay + sustainTime + env.release then
    -- Release phase
    let releaseProgress := (t - env.attack - env.decay - sustainTime) / env.release
    env.sustain * (1.0 - releaseProgress)
  else
    0.0

/-- Convert to a signal. -/
def toSignal (env : ADSR) : Signal Float :=
  fun t => env.sample t

/-- Convert to a signal with custom sustain hold time. -/
def toSignalWithHold (env : ADSR) (sustainTime : Float) : Signal Float :=
  fun t => env.sampleWithHold sustainTime t

/-- Convert to a duration-aware signal (no sustain hold). -/
def toDSignal (env : ADSR) : DSignal Float :=
  { signal := env.toSignal, duration := env.duration }

/-- Convert to a duration-aware signal with sustain hold. -/
def toDSignalWithHold (env : ADSR) (sustainTime : Float) : DSignal Float :=
  { signal := env.toSignalWithHold sustainTime
    duration := env.attack + env.decay + sustainTime + env.release }

end ADSR

/-- Apply an ADSR envelope to a signal.
    Returns a DSignal with the envelope's duration. -/
def applyEnvelope (env : ADSR) (sig : Signal Float) : DSignal Float :=
  { signal := fun t => sig.sample t * env.sample t
    duration := env.duration }

/-- Apply an ADSR envelope with sustain hold time. -/
def applyEnvelopeWithHold (env : ADSR) (sustainTime : Float)
    (sig : Signal Float) : DSignal Float :=
  { signal := fun t => sig.sample t * env.sampleWithHold sustainTime t
    duration := env.attack + env.decay + sustainTime + env.release }

/-- Apply a simple linear envelope (attack + decay to zero). -/
def applyAD (attack decay : Float) (sig : Signal Float) : DSignal Float :=
  let env := ADSR.create (attack := attack) (decay := decay) (sustain := 0.0) (release := 0.0)
  applyEnvelope env sig

end Fugue.Env
