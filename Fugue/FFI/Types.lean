/-
  Fugue.FFI.Types - FFI type definitions

  Opaque handle types for audio playback.
-/

namespace Fugue.FFI

/-- Opaque handle to an audio player.
    Uses the NonemptyType pattern for safe FFI. -/
opaque AudioPlayerPointed : NonemptyType

/-- Audio player handle for playback operations. -/
def AudioPlayer : Type := AudioPlayerPointed.type

instance : Nonempty AudioPlayer := AudioPlayerPointed.property

end Fugue.FFI
