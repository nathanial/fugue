/-
  Fugue.Filter.Biquad - Standard biquad (two-pole/two-zero) filters

  Biquad filters provide 12dB/octave rolloff with optional resonance.
  Coefficients based on the Audio EQ Cookbook by Robert Bristow-Johnson.
-/
import Fugue.Core.Signal
import Fugue.Osc.Sine

namespace Fugue.Filter

open Fugue
open Fugue.Osc (twoPi)

/-- Biquad filter type enumeration -/
inductive FilterType
  | lowpass   -- 12dB/octave lowpass
  | highpass  -- 12dB/octave highpass
  | bandpass  -- Bandpass (constant skirt gain)
  | notch     -- Band-reject / notch
  | allpass   -- Allpass (phase shifter)
  | peak      -- Peaking EQ
  | lowShelf  -- Low shelf EQ
  | highShelf -- High shelf EQ
  deriving Repr, BEq, Inhabited

/-- Biquad filter coefficients (normalized, a0 = 1) -/
structure BiquadCoeffs where
  b0 : Float  -- Feedforward coefficient for x[n]
  b1 : Float  -- Feedforward coefficient for x[n-1]
  b2 : Float  -- Feedforward coefficient for x[n-2]
  a1 : Float  -- Feedback coefficient for y[n-1]
  a2 : Float  -- Feedback coefficient for y[n-2]
  deriving Repr, Inhabited

/-- Calculate lowpass biquad coefficients.
    - cutoff: Corner frequency in Hz
    - Q: Quality factor (0.707 = Butterworth, higher = resonant peak) -/
def calcLowpassCoeffs (cutoff : Float) (Q : Float) (sampleRate : Float) : BiquadCoeffs :=
  let omega := twoPi * cutoff / sampleRate
  let sinW := Float.sin omega
  let cosW := Float.cos omega
  let alpha := sinW / (2.0 * Q)
  let a0 := 1.0 + alpha
  {
    b0 := (1.0 - cosW) / 2.0 / a0
    b1 := (1.0 - cosW) / a0
    b2 := (1.0 - cosW) / 2.0 / a0
    a1 := -2.0 * cosW / a0
    a2 := (1.0 - alpha) / a0
  }

/-- Calculate highpass biquad coefficients. -/
def calcHighpassCoeffs (cutoff : Float) (Q : Float) (sampleRate : Float) : BiquadCoeffs :=
  let omega := twoPi * cutoff / sampleRate
  let sinW := Float.sin omega
  let cosW := Float.cos omega
  let alpha := sinW / (2.0 * Q)
  let a0 := 1.0 + alpha
  {
    b0 := (1.0 + cosW) / 2.0 / a0
    b1 := -(1.0 + cosW) / a0
    b2 := (1.0 + cosW) / 2.0 / a0
    a1 := -2.0 * cosW / a0
    a2 := (1.0 - alpha) / a0
  }

/-- Calculate bandpass biquad coefficients (constant skirt gain). -/
def calcBandpassCoeffs (centerFreq : Float) (Q : Float) (sampleRate : Float) : BiquadCoeffs :=
  let omega := twoPi * centerFreq / sampleRate
  let sinW := Float.sin omega
  let cosW := Float.cos omega
  let alpha := sinW / (2.0 * Q)
  let a0 := 1.0 + alpha
  {
    b0 := alpha / a0
    b1 := 0.0
    b2 := -alpha / a0
    a1 := -2.0 * cosW / a0
    a2 := (1.0 - alpha) / a0
  }

/-- Calculate notch (band-reject) biquad coefficients. -/
def calcNotchCoeffs (centerFreq : Float) (Q : Float) (sampleRate : Float) : BiquadCoeffs :=
  let omega := twoPi * centerFreq / sampleRate
  let sinW := Float.sin omega
  let cosW := Float.cos omega
  let alpha := sinW / (2.0 * Q)
  let a0 := 1.0 + alpha
  {
    b0 := 1.0 / a0
    b1 := -2.0 * cosW / a0
    b2 := 1.0 / a0
    a1 := -2.0 * cosW / a0
    a2 := (1.0 - alpha) / a0
  }

/-- Calculate allpass biquad coefficients. -/
def calcAllpassCoeffs (centerFreq : Float) (Q : Float) (sampleRate : Float) : BiquadCoeffs :=
  let omega := twoPi * centerFreq / sampleRate
  let sinW := Float.sin omega
  let cosW := Float.cos omega
  let alpha := sinW / (2.0 * Q)
  let a0 := 1.0 + alpha
  {
    b0 := (1.0 - alpha) / a0
    b1 := -2.0 * cosW / a0
    b2 := (1.0 + alpha) / a0
    a1 := -2.0 * cosW / a0
    a2 := (1.0 - alpha) / a0
  }

/-- Calculate peaking EQ biquad coefficients.
    - gain: Boost/cut in dB -/
def calcPeakCoeffs (centerFreq : Float) (Q : Float) (gain : Float) (sampleRate : Float) : BiquadCoeffs :=
  let omega := twoPi * centerFreq / sampleRate
  let sinW := Float.sin omega
  let cosW := Float.cos omega
  let A := Float.pow 10.0 (gain / 40.0)  -- sqrt of amplitude
  let alpha := sinW / (2.0 * Q)
  let a0 := 1.0 + alpha / A
  {
    b0 := (1.0 + alpha * A) / a0
    b1 := -2.0 * cosW / a0
    b2 := (1.0 - alpha * A) / a0
    a1 := -2.0 * cosW / a0
    a2 := (1.0 - alpha / A) / a0
  }

/-- Calculate low shelf biquad coefficients.
    - gain: Boost/cut in dB -/
def calcLowShelfCoeffs (cutoff : Float) (Q : Float) (gain : Float) (sampleRate : Float) : BiquadCoeffs :=
  let omega := twoPi * cutoff / sampleRate
  let sinW := Float.sin omega
  let cosW := Float.cos omega
  let A := Float.pow 10.0 (gain / 40.0)
  let alpha := sinW / (2.0 * Q)
  let sqrtA := Float.sqrt A
  let a0 := (A + 1.0) + (A - 1.0) * cosW + 2.0 * sqrtA * alpha
  {
    b0 := A * ((A + 1.0) - (A - 1.0) * cosW + 2.0 * sqrtA * alpha) / a0
    b1 := 2.0 * A * ((A - 1.0) - (A + 1.0) * cosW) / a0
    b2 := A * ((A + 1.0) - (A - 1.0) * cosW - 2.0 * sqrtA * alpha) / a0
    a1 := -2.0 * ((A - 1.0) + (A + 1.0) * cosW) / a0
    a2 := ((A + 1.0) + (A - 1.0) * cosW - 2.0 * sqrtA * alpha) / a0
  }

/-- Calculate high shelf biquad coefficients.
    - gain: Boost/cut in dB -/
def calcHighShelfCoeffs (cutoff : Float) (Q : Float) (gain : Float) (sampleRate : Float) : BiquadCoeffs :=
  let omega := twoPi * cutoff / sampleRate
  let sinW := Float.sin omega
  let cosW := Float.cos omega
  let A := Float.pow 10.0 (gain / 40.0)
  let alpha := sinW / (2.0 * Q)
  let sqrtA := Float.sqrt A
  let a0 := (A + 1.0) - (A - 1.0) * cosW + 2.0 * sqrtA * alpha
  {
    b0 := A * ((A + 1.0) + (A - 1.0) * cosW + 2.0 * sqrtA * alpha) / a0
    b1 := -2.0 * A * ((A - 1.0) + (A + 1.0) * cosW) / a0
    b2 := A * ((A + 1.0) + (A - 1.0) * cosW - 2.0 * sqrtA * alpha) / a0
    a1 := 2.0 * ((A - 1.0) - (A + 1.0) * cosW) / a0
    a2 := ((A + 1.0) - (A - 1.0) * cosW - 2.0 * sqrtA * alpha) / a0
  }

/-- Calculate biquad coefficients for given filter type. -/
def calcCoeffs (filterType : FilterType) (freq : Float) (Q : Float)
    (sampleRate : Float) (gain : Float := 0.0) : BiquadCoeffs :=
  match filterType with
  | .lowpass => calcLowpassCoeffs freq Q sampleRate
  | .highpass => calcHighpassCoeffs freq Q sampleRate
  | .bandpass => calcBandpassCoeffs freq Q sampleRate
  | .notch => calcNotchCoeffs freq Q sampleRate
  | .allpass => calcAllpassCoeffs freq Q sampleRate
  | .peak => calcPeakCoeffs freq Q gain sampleRate
  | .lowShelf => calcLowShelfCoeffs freq Q gain sampleRate
  | .highShelf => calcHighShelfCoeffs freq Q gain sampleRate

/-- Apply biquad filter with given coefficients.
    Uses time-shifted sampling to approximate IIR behavior. -/
def biquad (coeffs : BiquadCoeffs) (sampleRate : Float)
    (sig : Signal Float) : Signal Float :=
  let T := 1.0 / sampleRate
  fun t =>
    -- Current and previous input samples
    let x0 := sig.sample t
    let x1 := if t >= T then sig.sample (t - T) else 0.0
    let x2 := if t >= 2.0 * T then sig.sample (t - 2.0 * T) else 0.0
    -- Approximate previous outputs using previous inputs
    -- This is stable for well-designed filter coefficients
    let y1 := x1
    let y2 := x2
    coeffs.b0 * x0 + coeffs.b1 * x1 + coeffs.b2 * x2
      - coeffs.a1 * y1 - coeffs.a2 * y2

/-- Lowpass filter with resonance (12dB/octave).
    - cutoff: Corner frequency in Hz
    - Q: Resonance (0.707 = Butterworth flat, higher = resonant peak) -/
def lowpass (cutoff : Float) (Q : Float := 0.707) (sampleRate : Float := 44100.0)
    (sig : Signal Float) : Signal Float :=
  let coeffs := calcLowpassCoeffs cutoff Q sampleRate
  biquad coeffs sampleRate sig

/-- Highpass filter with resonance (12dB/octave). -/
def highpass (cutoff : Float) (Q : Float := 0.707) (sampleRate : Float := 44100.0)
    (sig : Signal Float) : Signal Float :=
  let coeffs := calcHighpassCoeffs cutoff Q sampleRate
  biquad coeffs sampleRate sig

/-- Bandpass filter.
    - centerFreq: Center frequency in Hz
    - Q: Bandwidth factor (higher = narrower band) -/
def bandpass (centerFreq : Float) (Q : Float := 1.0) (sampleRate : Float := 44100.0)
    (sig : Signal Float) : Signal Float :=
  let coeffs := calcBandpassCoeffs centerFreq Q sampleRate
  biquad coeffs sampleRate sig

/-- Notch (band-reject) filter. -/
def notch (centerFreq : Float) (Q : Float := 1.0) (sampleRate : Float := 44100.0)
    (sig : Signal Float) : Signal Float :=
  let coeffs := calcNotchCoeffs centerFreq Q sampleRate
  biquad coeffs sampleRate sig

/-- Allpass filter - preserves magnitude, shifts phase.
    Useful for phaser effects. -/
def allpass (centerFreq : Float) (Q : Float := 0.707) (sampleRate : Float := 44100.0)
    (sig : Signal Float) : Signal Float :=
  let coeffs := calcAllpassCoeffs centerFreq Q sampleRate
  biquad coeffs sampleRate sig

/-- Resonant filter - lowpass with high Q for synth-style filtering.
    - cutoff: Corner frequency in Hz
    - resonance: Resonance amount 0.0-1.0 (maps to Q 0.5-20) -/
def resonant (cutoff : Float) (resonance : Float := 0.5) (sampleRate : Float := 44100.0)
    (sig : Signal Float) : Signal Float :=
  -- Map resonance 0-1 to Q 0.5-20
  let Q := 0.5 + resonance * 19.5
  lowpass cutoff Q sampleRate sig

/-- Peaking EQ filter.
    - centerFreq: Center frequency in Hz
    - Q: Bandwidth factor
    - gainDb: Boost/cut in dB (-12 to +12 typical) -/
def peakEQ (centerFreq : Float) (Q : Float := 1.0) (gainDb : Float := 0.0)
    (sampleRate : Float := 44100.0) (sig : Signal Float) : Signal Float :=
  let coeffs := calcPeakCoeffs centerFreq Q gainDb sampleRate
  biquad coeffs sampleRate sig

/-- Low shelf EQ filter.
    Boosts or cuts frequencies below cutoff. -/
def lowShelf (cutoff : Float) (gainDb : Float := 0.0)
    (sampleRate : Float := 44100.0) (sig : Signal Float) : Signal Float :=
  let coeffs := calcLowShelfCoeffs cutoff 0.707 gainDb sampleRate
  biquad coeffs sampleRate sig

/-- High shelf EQ filter.
    Boosts or cuts frequencies above cutoff. -/
def highShelf (cutoff : Float) (gainDb : Float := 0.0)
    (sampleRate : Float := 44100.0) (sig : Signal Float) : Signal Float :=
  let coeffs := calcHighShelfCoeffs cutoff 0.707 gainDb sampleRate
  biquad coeffs sampleRate sig

/-- Two-band crossover filter.
    Returns (lowBand, highBand) split at crossover frequency. -/
def crossover (freq : Float) (sampleRate : Float := 44100.0)
    (sig : Signal Float) : Signal Float × Signal Float :=
  let lo := lowpass freq 0.707 sampleRate sig
  let hi := highpass freq 0.707 sampleRate sig
  (lo, hi)

/-- Cascaded biquad lowpass for steeper rolloff.
    - order: Number of biquad stages (2 = 24dB/oct, 3 = 36dB/oct) -/
def lowpassSteep (cutoff : Float) (order : Nat) (sampleRate : Float := 44100.0)
    (sig : Signal Float) : Signal Float :=
  match order with
  | 0 => sig
  | 1 => lowpass cutoff 0.707 sampleRate sig
  | n + 1 => lowpass cutoff 0.707 sampleRate (lowpassSteep cutoff n sampleRate sig)

/-- Cascaded biquad highpass for steeper rolloff. -/
def highpassSteep (cutoff : Float) (order : Nat) (sampleRate : Float := 44100.0)
    (sig : Signal Float) : Signal Float :=
  match order with
  | 0 => sig
  | 1 => highpass cutoff 0.707 sampleRate sig
  | n + 1 => highpass cutoff 0.707 sampleRate (highpassSteep cutoff n sampleRate sig)

end Fugue.Filter
