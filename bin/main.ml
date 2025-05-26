let () =
  let arguments = Sys.argv in
  let usage =
    "Usage: " ^ Sys.argv.(0)
    ^ " [name] \t Open named checklist\n --help \t Show this help message\n"
  in
  let name =
    if Array.length arguments > 1 then
      arguments.(1)
    else
      ""
  in
  let open Tui in
  let original = terminal_setup () in
  checklist original;
  at_exit (fun () -> terminal_restore original)
