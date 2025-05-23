let terminal_setup () =
  let fd = Unix.stdin in
  let terminal_original_settings = Unix.tcgetattr fd in
  let termios = Unix.tcgetattr fd in
  let raw =
    { termios with c_icanon = false; c_echo = false; c_vmin = 1; c_vtime = 0 }
  in
  Unix.tcsetattr fd TCSANOW raw;
  terminal_original_settings

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
    match c with
    | '\n' -> Buffer.contents buffer
    | '\b' | '\127' ->
        if Buffer.length buffer > 0 then (
          Buffer.truncate buffer (Buffer.length buffer - 1);
          print_string "\b \b";
          flush stdout);
        read_input ()
    | _ ->
        Buffer.add_char buffer c;
        read_input ()
  in
  read_input ()

let set_color color =
  let color_code =
    match color with
    | `Red -> "\x1b[31m"
    | `Green -> "\x1b[32m"
    | `Yellow -> "\x1b[33m"
    | `Blue -> "\x1b[34m"
    | `Magenta -> "\x1b[35m"
    | `Cyan -> "\x1b[36m"
    | `White -> "\x1b[37m"
  in
  print_string color_code

let draw_checklist_helper items selected new_id =
  clear_screen ();
  set_color `White;
  set_cursor_position (0, 0);
  List.iter
    (fun (id, checked, item) ->
      let checkmark = if checked then "[x] " else "[ ] " in
      if selected = id || new_id = id then (
        set_color `Blue;
        print_string (checkmark ^ item);
        print_newline ();
        if new_id = id then (
          print_string "[ ] ";
          print_newline ()))
      else (
        set_color `White;
        print_string (checkmark ^ item);
        print_newline ()))
    items

let draw_checklist items selected = draw_checklist_helper items selected (-1)

let draw_checklist_insert items new_id =
  draw_checklist_helper items (-1) new_id

let check_item items id =
  List.map
    (fun (i, checked, item) ->
      if i = id then (i, not checked, item) else (i, checked, item))
    items

let menu_input () =
  match input_char stdin with
  | 'j' -> `Down
  | 'k' -> `Up
  | 'i' -> `Insert
  | 'p' -> `Paste
  | 'd' -> `Delete
  | 'y' -> `Yank
  | '\n' -> `Enter
  | 'q' -> `Quit
  | _ -> `Unknown

let buffer = ref None

let wrap_index idx len =
  if idx < 0 then len - 1 else if idx >= len then 0 else idx

let handle_down selected items = wrap_index (selected + 1) (List.length items)
let handle_up selected items = wrap_index (selected - 1) (List.length items)

let handle_insert selected items =
  let new_index = selected + 1 in
  draw_checklist_insert items new_index;
  set_cursor_position (new_index + 1, 5);
  let new_item = insert_mode () in
  Filerepository.insert_item new_index new_item;
  wrap_index (selected + 1) (List.length items + 1)

let handle_delete selected =
  buffer := Filerepository.get_item selected;
  Filerepository.remove_item selected;
  if selected = 0 then 0 else selected - 1

let handle_paste selected items =
  match !buffer with
  | Some (_, _, item) ->
      let new_index = selected + 1 in
      Filerepository.insert_item new_index item;
      wrap_index (selected + 1) (List.length items + 1)
  | None -> selected

let handle_yank selected =
  buffer := Filerepository.get_item selected;
  selected

let handle_enter items selected =
  check_item items selected |> Filerepository.write_file_repository;
  selected

let handle_quit terminal_original_settings =
  terminal_restore terminal_original_settings;
  exit 0

let checklist terminal_original_settings =
  Filerepository.create_dir_and_file ();
  let rec loop selected =
    let items = Filerepository.read_file_repository () in
    draw_checklist items selected;
    match menu_input () with
    | `Down -> loop (handle_down selected items)
    | `Up -> loop (handle_up selected items)
    | `Insert -> loop (handle_insert selected items)
    | `Delete -> loop (handle_delete selected)
    | `Paste -> loop (handle_paste selected items)
    | `Yank -> loop (handle_yank selected)
    | `Enter -> loop (handle_enter items selected)
    | `Quit -> handle_quit terminal_original_settings
    | `Unknown -> loop selected
  in
  loop 0
