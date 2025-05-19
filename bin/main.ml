let () =
    let open Tui in
    let items = [
        (0, false, "Item 0");
        (1, true, "Item 1");
        (2, false, "Item 2");
        (3, true, "Item 3");
    ] in
    checklist items;
