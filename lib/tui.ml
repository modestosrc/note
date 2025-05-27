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

(*
let get_cursor_position () =
  Printf.printf "\027[6n%!";
  let buffer = Bytes.create 32 in
  let rec read_loop i =
    if i < 32 then (
      let c = input_char stdin in
      Bytes.set buffer i c;
      if c = 'R' then Bytes.sub_string buffer 0 (i + 1) else read_loop (i + 1))
    else Bytes.sub_string buffer 0 i
  in
  let response = read_loop 0 in
  try
    if response.[0] = '\027' && response.[1] = '[' then
      let rc = String.sub response 2 (String.length response - 3) in
      match String.split_on_char ';' rc with
      | [ row; col ] -> (int_of_string row, int_of_string col)
      | _ -> (-1, -1)
    else (-1, -1)
  with _ -> (-1, -1)
*)

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
let draw_checklist_insert items new_id = draw_checklist_helper items (-1) new_id

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

let handle_insert name selected items =
  let new_index = selected + 1 in
  draw_checklist_insert items new_index;
  set_cursor_position (new_index + 1, 5);
  let new_item = insert_mode () in
  Filerepository.insert_item name new_index new_item;
  wrap_index (selected + 1) (List.length items + 1)

let handle_delete name selected =
  buffer := Filerepository.get_item name selected;
  Filerepository.remove_item name selected;
  if selected = 0 then 0 else selected - 1

let handle_paste name selected items =
  match !buffer with
  | Some (_, _, item) ->
      let new_index = selected + 1 in
      Filerepository.insert_item name new_index item;
      wrap_index (selected + 1) (List.length items + 1)
  | None -> selected

let handle_yank name selected =
  buffer := Filerepository.get_item name selected;
  selected

let handle_enter name items selected =
  check_item items selected |> Filerepository.write_file_repository name;
  selected

let handle_quit terminal_original_settings =
  terminal_restore terminal_original_settings;
  exit 0

let handle_name = function
  | Some name -> name ^ ".json"
  | None -> "notes.json"

let checklist terminal_original_settings name =
  let name = handle_name name in
  Filerepository.create_dir_and_file name;
  let rec loop selected =
    let items = Filerepository.read_file_repository name in
    draw_checklist items selected;
    match menu_input () with
    | `Down -> loop (handle_down selected items)
    | `Up -> loop (handle_up selected items)
    | `Insert -> loop (handle_insert name selected items)
    | `Delete -> loop (handle_delete name selected)
    | `Paste -> loop (handle_paste name selected items)
    | `Yank -> loop (handle_yank name selected)
    | `Enter -> loop (handle_enter name items selected)
    | `Quit -> handle_quit terminal_original_settings
    | `Unknown -> loop selected
  in
  loop 0
