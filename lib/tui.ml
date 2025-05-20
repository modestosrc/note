
(* Set terminal to raw mode *)
let terminal_setup () =
  let fd = Unix.stdin in
  let terminal_original_settings = Unix.tcgetattr fd in
  let termios = Unix.tcgetattr fd in
  let raw =
    { termios with c_icanon = false; c_echo = false; c_vmin = 1; c_vtime = 0 }
  in
  Unix.tcsetattr fd TCSANOW raw;
  terminal_original_settings

(* Reset terminal to original settings *)
let terminal_restore terminal_original_settings =
  let fd = Unix.stdin in
  Unix.tcsetattr fd TCSANOW terminal_original_settings

let clear_screen () =
  print_string "\027[2J\027[H%!";
  flush stdout

let set_cursor_position (row, col) = Printf.printf "\027[%d;%dH%!" row col

let insert_mode () =
  let buffer = Buffer.create 64 in
  let rec read_input () =
    let c = input_char stdin in
    print_string (String.make 1 c);
    flush stdout;
    if c = '\n' then Buffer.contents buffer
    else if c = '\b' || c = Char.chr 127 then (
      if Buffer.length buffer > 0 then (
        Buffer.truncate buffer (Buffer.length buffer - 1);
        print_string "\b \b";
        flush stdout);
      read_input ())
    else (
      Buffer.add_char buffer c;
      read_input ())
  in
  read_input ()

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

let draw_checklist_insert items new_id =
  clear_screen ();
  set_collor `White;
  set_cursor_position (0, 0);
  let rec aux items =
    match items with
    | [] -> ()
    | (id, checked, item) :: rest ->
        if new_id = id then (
          let checkmark = if checked then "[x] " else "[ ] " in
          set_collor `Blue;
          print_string "[ ] ";
          print_newline ();
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

let input_controller () = 
  match input_char stdin with
  | 'j' -> `Down
  | 'k' -> `Up
  | 'i' -> `Insert
  | 'd' -> `Delete
  | '\n' -> `Enter
  | 'q' -> `Quit
  | _ -> `Unknown


let checklist terminal_original_settings =
  Filerepository.create_dir_and_file ();
  let rec loop selected =
    let items = Filerepository.read_file_repository () in
    draw_checklist items selected;
    if selected < 0 then loop (List.length items - 1)
    else if selected >= List.length items then loop 0
    else
      match input_controller () with
      | `Down -> loop (selected + 1)
      | `Up -> loop (selected - 1)
      | `Insert ->
          let new_index = selected + 1 in
          draw_checklist_insert items new_index;
          set_cursor_position (new_index + 1, 5);
          let new_item = insert_mode () in
          Filerepository.insert_item new_index new_item;
          loop (selected + 1)
      | `Delete ->
          Filerepository.remove_item selected;
          loop (if selected = 0 then 0 else selected - 1)
      | `Enter ->
          check_item items selected |> Filerepository.write_file_repository;
          loop selected
      | `Quit ->
          terminal_restore terminal_original_settings;
          exit 0
      | `Unknown -> loop selected
  in
  loop 0
