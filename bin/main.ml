let () =
    let open Tui in
    let original = terminal_setup () in
    checklist original;
    at_exit (fun () -> terminal_restore original);
