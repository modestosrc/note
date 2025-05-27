let () =
  let arguments = Sys.argv in
  let usage =
    "Usage: " ^ Sys.argv.(0)
    ^ " [name] \t Open named checklist\n --help \t Show this help message\n"
  in
  let name = if Array.length arguments > 1 then Some arguments.(1) else None in
  if Array.length arguments > 2 then (
    print_endline usage;
    exit 1)
  else if name = Some "--help" then (
    print_endline usage;
    exit 0)
  else
    (* Initialize the terminal and start the checklist *)
    let open Tui in
    let original = terminal_setup () in
    checklist original name;
    at_exit (fun () -> terminal_restore original)
