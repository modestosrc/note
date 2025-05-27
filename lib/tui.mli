val terminal_setup : unit -> Unix.terminal_io
val terminal_restore : Unix.terminal_io -> unit
val checklist : Unix.terminal_io -> string option -> unit
