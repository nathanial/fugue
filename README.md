# Fugue

A functional sound synthesis library for Lean 4. Fugue provides an algebra of sounds where signals are first-class values that compose naturally.

## Features

- **Pure functional synthesis** - Signals are functions from time to amplitude
- **Oscillators** - Sine, square, sawtooth, triangle, and noise generators
- **Envelopes** - Full ADSR (Attack-Decay-Sustain-Release) envelope shaping
- **Combinators** - Mix, scale, sequence, and transform signals
- **Real-time playback** - macOS AudioQueue integration via FFI

## Requirements

- Lean 4.26.0+
- macOS (for audio playback)
- Xcode Command Line Tools

## Building

Use the build scripts to ensure proper framework linking:

```bash
./build.sh          # Build library
./build.sh demo     # Build demo
./test.sh           # Run tests
```

## Quick Example

```lean
import Fugue

open Fugue Osc Env Combine Render FFI

def main : IO Unit := do
  -- Initialize audio
  AudioPlayer.init
  let player ← AudioPlayer.create 44100.0

  -- Create an ADSR envelope
  let env := ADSR.create (attack := 0.02) (decay := 0.1) (sustain := 0.7) (release := 0.2)

  -- Play a note: sine wave shaped by envelope
  let note := applyEnvelope env (sine 440.0)
  let samples := renderDSignalClipped cdQuality (scaleD 0.5 note)
  player.play samples
```

## Core Types

### Signal

An infinite signal is a function from time to value:

```lean
def Signal (α : Type) := Float → α
```

### DSignal

A duration-aware signal with finite length:

```lean
structure DSignal (α : Type) where
  signal : Signal α
  duration : Float
```

## API Overview

### Oscillators (`Fugue.Osc`)

| Function | Description |
|----------|-------------|
| `sine freq` | Pure sine wave at frequency (Hz) |
| `square freq` | Square wave with harmonics |
| `sawtooth freq` | Sawtooth wave |
| `triangle freq` | Triangle wave |
| `noise seed` | White noise generator |

### Envelopes (`Fugue.Env`)

| Function | Description |
|----------|-------------|
| `ADSR.create` | Create envelope with attack/decay/sustain/release |
| `ADSR.percussive` | Quick attack, no sustain |
| `ADSR.pad` | Slow attack and release |
| `ADSR.pluck` | Fast attack, medium decay |
| `applyEnvelope env sig` | Shape a signal with an envelope |
| `applyEnvelopeWithHold env holdTime sig` | Envelope with sustain hold |

### Combinators (`Fugue.Combine`)

| Function | Description |
|----------|-------------|
| `mix a b` | Add two signals |
| `mixAll sigs` | Mix list of signals (normalized) |
| `scale factor sig` | Multiply amplitude |
| `clip sig` | Hard limit to [-1, 1] |
| `append a b` | Play b after a |
| `sequence sigs` | Play signals in order |
| `delay time sig` | Add silence before signal |

### Rendering (`Fugue.Render`)

| Function | Description |
|----------|-------------|
| `renderSignal config duration sig` | Sample signal to FloatArray |
| `renderDSignal config dsig` | Render duration-aware signal |
| `renderDSignalClipped config dsig` | Render with clipping |
| `cdQuality` | 44100 Hz sample rate config |

### Audio Playback (`Fugue.FFI`)

| Function | Description |
|----------|-------------|
| `AudioPlayer.init` | Initialize audio subsystem |
| `AudioPlayer.create sampleRate` | Create player at sample rate |
| `player.play samples` | Play samples (blocking) |
| `player.playAsync samples` | Play samples (non-blocking) |
| `player.wait` | Wait for playback to finish |
| `player.stop` | Stop playback |

## Project Structure

```
fugue/
├── Fugue/
│   ├── Core/           # Signal and DSignal types
│   ├── Osc/            # Oscillators
│   ├── Env/            # ADSR envelopes
│   ├── Combine/        # Signal combinators
│   ├── Render/         # Signal to samples
│   └── FFI/            # Audio playback bindings
├── FugueTests/         # Test suite
├── Demo.lean           # Example usage
└── native/             # C FFI implementation
    ├── include/
    └── src/
```

## License

MIT License - see [LICENSE](LICENSE) for details.
