/-
  Fugue.FFI.AudioQueue - macOS AudioQueue bindings

  Low-level audio playback via Core Audio's AudioQueue API.
-/
import Fugue.FFI.Types

namespace Fugue.FFI

/-- Initialize the audio subsystem. Call once at startup. -/
@[extern "lean_fugue_audio_init"]
opaque AudioPlayer.init : IO Unit

/-- Create an audio player with given sample rate (Hz). -/
@[extern "lean_fugue_audio_player_create"]
opaque AudioPlayer.create (sampleRate : Float) : IO AudioPlayer

/-- Destroy an audio player (usually handled by GC). -/
@[extern "lean_fugue_audio_player_destroy"]
opaque AudioPlayer.destroy (player : @& AudioPlayer) : IO Unit

/-- Play samples (blocking - waits for playback to complete). -/
@[extern "lean_fugue_audio_player_play"]
opaque AudioPlayer.play (player : @& AudioPlayer) (samples : @& FloatArray) : IO Unit

/-- Play samples asynchronously (non-blocking). -/
@[extern "lean_fugue_audio_player_play_async"]
opaque AudioPlayer.playAsync (player : @& AudioPlayer) (samples : @& FloatArray) : IO Unit

/-- Wait for async playback to complete. -/
@[extern "lean_fugue_audio_player_wait"]
opaque AudioPlayer.wait (player : @& AudioPlayer) : IO Unit

/-- Stop playback immediately. -/
@[extern "lean_fugue_audio_player_stop"]
opaque AudioPlayer.stop (player : @& AudioPlayer) : IO Unit

/-- Check if player is currently playing. -/
@[extern "lean_fugue_audio_player_is_playing"]
opaque AudioPlayer.isPlaying (player : @& AudioPlayer) : IO Bool

namespace AudioPlayer

/-- Create a player with CD quality sample rate (44100 Hz). -/
def createDefault : IO AudioPlayer := create 44100.0

/-- Play samples and automatically wait for completion. -/
def playSync (player : AudioPlayer) (samples : FloatArray) : IO Unit :=
  player.play samples

end AudioPlayer
end Fugue.FFI
