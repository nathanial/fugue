/-
  Fugue.Effects.Distortion - Clipping and waveshaping effects

  Distortion effects that modify the waveform shape for harmonic richness.
-/
import Fugue.Core.Signal

namespace Fugue.Effects

open Fugue

/-- Hard clipping - limits signal to [-threshold, threshold].
    Creates harsh digital distortion with odd harmonics. -/
@[inline]
def hardClip (threshold : Float := 1.0) (sig : Signal Float) : Signal Float :=
  fun t =>
    let v := sig.sample t
    if v > threshold then threshold
    else if v < -threshold then -threshold
    else v

/-- Soft clipping using tanh - warm analog-style saturation.
    Drive controls input gain before saturation. -/
@[inline]
def softClip (drive : Float := 1.0) (sig : Signal Float) : Signal Float :=
  fun t => Float.tanh (sig.sample t * drive)

/-- Overdrive - tube amp style distortion.
    Higher drive values create more saturation. -/
@[inline]
def overdrive (drive : Float := 2.0) (sig : Signal Float) : Signal Float :=
  fun t =>
    let v := sig.sample t * drive
    Float.tanh v

/-- Asymmetric overdrive - simulates tube amp even harmonics.
    Asymmetry controls the bias (positive values boost positive half). -/
@[inline]
def tubeOverdrive (drive : Float := 2.0) (asymmetry : Float := 0.2)
    (sig : Signal Float) : Signal Float :=
  fun t =>
    let v := sig.sample t * drive
    let biased := v + asymmetry
    Float.tanh biased - Float.tanh asymmetry

/-- Fuzz distortion with wave folding.
    Creates extreme distortion with complex harmonics. -/
@[inline]
def fuzz (amount : Float := 3.0) (sig : Signal Float) : Signal Float :=
  fun t =>
    let v := sig.sample t * amount
    -- Fold wave back when it exceeds [-1, 1]
    let folded :=
      if v > 1.0 then 2.0 - v
      else if v < -1.0 then -2.0 - v
      else v
    -- Apply soft saturation to the folded signal
    Float.tanh folded

/-- Bitcrusher - reduces bit depth for lo-fi effect.
    Bits controls quantization levels (8 = 256 levels). -/
@[inline]
def bitcrush (bits : Nat := 8) (sig : Signal Float) : Signal Float :=
  fun t =>
    let levels := Float.pow 2.0 bits.toFloat
    let v := sig.sample t
    -- Quantize to discrete levels
    let scaled := (v + 1.0) * 0.5 * levels  -- Map [-1,1] to [0, levels]
    let quantized := Float.floor scaled
    let normalized := quantized / levels * 2.0 - 1.0  -- Map back to [-1,1]
    normalized

/-- Sample rate reduction - creates aliasing artifacts.
    Factor of 4 means every 4th sample is held. -/
def downsample (factor : Nat := 4) (sampleRate : Float := 44100.0)
    (sig : Signal Float) : Signal Float :=
  fun t =>
    -- Quantize time to reduced sample rate
    let reducedRate := sampleRate / factor.toFloat
    let sampleIndex := Float.floor (t * reducedRate)
    let quantizedTime := sampleIndex / reducedRate
    sig.sample quantizedTime

/-- Custom waveshaper with transfer function.
    The transfer function maps input values to output values. -/
@[inline]
def waveshape (transferFn : Float → Float) (sig : Signal Float) : Signal Float :=
  fun t => transferFn (sig.sample t)

/-- Rectifier - full wave rectification (absolute value).
    Creates octave-up effect by folding negative to positive. -/
@[inline]
def rectify (sig : Signal Float) : Signal Float :=
  fun t =>
    let v := sig.sample t
    if v < 0.0 then -v else v

/-- Half-wave rectifier - zeroes negative values.
    Creates fundamental + harmonics. -/
@[inline]
def halfRectify (sig : Signal Float) : Signal Float :=
  fun t =>
    let v := sig.sample t
    if v < 0.0 then 0.0 else v

end Fugue.Effects
