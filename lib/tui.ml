(* Set terminal to raw mode returns the original settings *)
let terminal_setup () =
  let fd = Unix.stdin in
  let original = Unix.tcgetattr fd in
  let termios = Unix.tcgetattr fd in
  let raw =
    { termios with c_icanon = false; c_echo = false; c_vmin = 1; c_vtime = 0 }
  in
  Unix.tcsetattr fd TCSANOW raw;
  original

(* Reset terminal to original settings *)
let terminal_reset original =
  let fd = Unix.stdin in
  Unix.tcsetattr fd TCSANOW original

let clear_screen () =
  print_string "\027[2J\027[H%!";
  flush stdout

let set_cursor_position (row, col) = Printf.printf "\027[%d;%dH%!" row col

let insert_mode cursor_pos original =
  set_cursor_position cursor_pos;
  terminal_reset original;
  let buffer = input_line stdin in
  terminal_setup () |> ignore;
  buffer

let set_collor colot =
  let color_code =
    match colot with
    | `Red -> "\x1b[31m"
    | `Green -> "\x1b[32m"
    | `Yellow -> "\x1b[33m"
    | `Blue -> "\x1b[34m"
    | `Magenta -> "\x1b[35m"
    | `Cyan -> "\x1b[36m"
    | `White -> "\x1b[37m"
  in
  print_string color_code

let draw_checklist items selected =
  clear_screen ();
  set_collor `White;
  set_cursor_position (0, 0);
  let rec aux items =
    match items with
    | [] -> ()
    | (id, checked, item) :: rest ->
        if selected = id then (
          let checkmark = if checked then "[x] " else "[ ] " in
          set_collor `Blue;
          print_string (checkmark ^ item);
          print_newline ();
          aux rest)
        else
          let checkmark = if checked then "[x] " else "[ ] " in
          set_collor `White;
          print_string (checkmark ^ item);
          print_newline ();
          aux rest
  in
  aux items

let draw_checklist_insert items selected =
  clear_screen ();
  set_collor `White;
  set_cursor_position (0, 0);
  let rec aux items =
    match items with
    | [] -> ()
    | (id, checked, item) :: rest ->
        if selected = id then (
          let checkmark = if checked then "[x] " else "[ ] " in
          set_collor `Blue;
          print_string (checkmark ^ item);
          print_newline ();
          print_string "[ ] ";
          print_newline ();
          aux rest)
        else
          let checkmark = if checked then "[x] " else "[ ] " in
          set_collor `White;
          print_string (checkmark ^ item);
          print_newline ();
          aux rest
  in
  aux items

let check_item items id =
  let rec aux items =
    match items with
    | [] -> []
    | (i, checked, item) :: rest ->
        if i = id then
          if checked then (i, false, item) :: rest else (i, true, item) :: rest
        else (i, checked, item) :: aux rest
  in
  aux items

let checklist () =
  Filerepository.create_dir_and_file ();
  let original = terminal_setup () in
  let rec loop selected =
    let items = Filerepository.read_file_repository () in
    draw_checklist items selected;
    if selected < 0 then loop (List.length items - 1)
    else if selected >= List.length items then loop 0
    else
      let c = input_char stdin in
      match c with
      | 'j' -> loop (selected + 1)
      | 'k' -> loop (selected - 1)
      | 'i' ->
          let new_index = selected + 1 in
          draw_checklist_insert items new_index;
          let new_item = insert_mode (new_index, 5) original in
          Filerepository.insert_item new_index new_item;
          loop (selected + 1)
      | '\n' ->
          check_item items selected |> Filerepository.write_file_repository;
          loop selected
      | 'q' ->
          terminal_reset original;
          exit 0
      | _ -> loop selected
  in
  loop 0
