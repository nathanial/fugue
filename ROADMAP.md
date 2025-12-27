# Fugue Roadmap

## Features

### Audio Effects ✓
- [x] **Delay** - Echo/delay effect with feedback control
- [x] **Reverb** - Simple algorithmic reverb (Schroeder or Freeverb)
- [x] **Distortion** - Soft clipping, hard clipping, waveshaping
- [x] **Chorus** - Detuned copies for thickness
- [x] **Tremolo** - Amplitude modulation effect

### Filters ✓
- [x] **Low-pass filter** - Remove high frequencies (biquad implementation)
- [x] **High-pass filter** - Remove low frequencies
- [x] **Band-pass filter** - Isolate frequency range
- [x] **Resonant filter** - Filter with Q/resonance control
- [x] **Filter envelope** - Time-varying filter cutoff

### Oscillators ✓
- [x] **Pulse wave** - Square wave with variable duty cycle (squareDuty/pulse in Square.lean)
- [x] **Wavetable oscillator** - Lookup-based synthesis with morphing
- [x] **Supersaw** - Multiple detuned sawtooth waves (also: hypersaw, unison, supersquare)
- [x] **Sub-oscillator** - Octave-down accompaniment (bassPatch, reeseBass, sub808)

### Synthesis Techniques
- [ ] **FM synthesis** - Frequency modulation between oscillators
- [ ] **AM synthesis** - Amplitude modulation / ring modulation
- [ ] **Additive synthesis** - Sum of sine partials
- [ ] **Subtractive synthesis** - Oscillator → Filter → Envelope chain

### Modulation
- [ ] **LFO** - Low-frequency oscillator for parameter modulation
- [ ] **Envelope followers** - Extract amplitude envelope from signal
- [ ] **Sample and hold** - Stepped random modulation

### Music Theory Helpers ✓
- [x] **MIDI note to frequency** - Convert note numbers to Hz (midiToFreq, freqToMidi)
- [x] **Note names** - Parse "C4", "A#3", etc. (parseNote, Note.toString)
- [x] **Scales** - Major, minor, pentatonic, chromatic, modes (Scale module)
- [x] **Chords** - Chord type to frequency list, inversions, voicings (Chord module)
- [x] **Tempo/BPM utilities** - Beat duration, time signatures, synced delays (Tempo module)

### File I/O
- [ ] **WAV export** - Write rendered audio to WAV files
- [ ] **WAV import** - Load samples for playback
- [ ] **Streaming render** - Render to file without loading all samples in memory

### Stereo Support
- [ ] **Stereo signals** - `Signal (Float × Float)` for L/R channels
- [ ] **Panning** - Position mono signal in stereo field
- [ ] **Stereo width** - Expand/collapse stereo image
- [ ] **Mid/side processing** - M/S encoding and manipulation

## Enhancements

### Performance
- [ ] **Lazy signal evaluation** - Only compute samples when needed
- [ ] **Signal caching** - Memoize expensive computations
- [ ] **SIMD optimization** - Vectorized sample processing in FFI
- [ ] **Block-based rendering** - Process samples in chunks for cache efficiency

### API Improvements
- [ ] **Signal combinators** - More operators (`<+>`, `<*>`, `>>>`)
- [ ] **Frequency type** - Newtype wrapper for Hz values
- [ ] **Time type** - Newtype wrapper for seconds/samples
- [ ] **Builder pattern** - Fluent API for complex signal chains
- [ ] **Presets** - Common instrument/effect configurations

### Audio Backend
- [ ] **Linux support** - ALSA or PulseAudio backend
- [ ] **Windows support** - WASAPI backend
- [ ] **Backend abstraction** - Platform-agnostic audio interface
- [ ] **Low-latency mode** - Smaller buffers for real-time use
- [ ] **Device selection** - Choose output device

### Error Handling
- [ ] **Result types** - Replace IO exceptions with explicit errors
- [ ] **Validation** - Check parameters at construction time
- [ ] **Debug mode** - Detect NaN/infinity in signals

## Bugs to Fix

- [ ] **Immediate stop cutoff** - `AudioQueueStop(true)` may cut off final samples; consider draining with timeout instead
- [ ] **Click at note boundaries** - Add micro-fade at DSignal boundaries to prevent clicks
- [ ] **Noise seed reproducibility** - Ensure same seed produces identical output across runs

## Code Cleanup

### Documentation
- [ ] Add docstrings to all public functions
- [ ] Add module-level documentation
- [ ] Create tutorial/cookbook examples
- [ ] Document FFI interface for contributors

### Testing
- [ ] Add property-based tests for signal laws (Functor, Applicative)
- [ ] Add tests for edge cases (zero duration, negative time, etc.)
- [ ] Add audio regression tests (compare rendered output)
- [ ] Benchmark rendering performance

### Code Quality
- [ ] Extract common Float utilities (clamp, lerp, etc.) to shared module
- [ ] Consistent naming conventions across modules
- [ ] Remove duplicate `maxFloat`/`absFloat` definitions
- [ ] Add `@[specialize]` hints for performance-critical functions

### Project Structure
- [ ] Add CLAUDE.md with project-specific guidance
- [ ] Add CONTRIBUTING.md with development guidelines
- [ ] CI setup for automated testing
- [ ] Example projects in `examples/` directory

## Future Ideas

- **MIDI input** - Real-time note input from MIDI controllers
- **VST/AU plugin** - Export as audio plugin
- **Visual editor** - Node-based signal graph editor
- **Live coding** - Hot-reload signal definitions
- **Spectral processing** - FFT-based effects
- **Physical modeling** - Karplus-Strong, waveguides
- **Granular synthesis** - Grain-based sound manipulation
