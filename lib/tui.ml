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
  (* Espera resposta do tipo ESC [ linha ; coluna R *)
  try
    if response.[0] = '\027' && response.[1] = '[' then
      let rc = String.sub response 2 (String.length response - 3) in
      match String.split_on_char ';' rc with
      | [ row; col ] -> (int_of_string row, int_of_string col)
      | _ -> (-1, -1)
    else (-1, -1)
  with _ -> (-1, -1)

let set_cursor_position (row, col) = Printf.printf "\027[%d;%dH%!" row col

let insert_mode () =
  let buffer = Buffer.create 32 in
  let rec loop () =
    let c = input_char stdin in
    match c with
    | '\n' -> Buffer.contents buffer
    | c ->
        Buffer.add_char buffer c;
        loop ()
  in
  loop ()

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
  let cursor_pos = get_cursor_position () in
  let rec loop selected =
    let items = Filerepository.read_file_repository () in
    draw_checklist items selected;
    set_cursor_position cursor_pos;
    if selected < 0 then loop (List.length items - 1)
    else if selected >= List.length items then loop 0
    else
      let c = input_char stdin in
      match c with
      | 'k' -> loop (selected + 1)
      | 'j' -> loop (selected - 1)
      | 'i' ->
          set_cursor_position cursor_pos;
          print_string "Insert new item: ";
          let new_item = insert_mode () in
          let items = Filerepository.read_file_repository () in
          let new_id = List.length items in
          let new_items = (new_id, false, new_item) :: items in
          Filerepository.write_file_repository new_items;
          loop selected
      | '\n' ->
          check_item items selected |> Filerepository.write_file_repository;
          loop selected
      | 'q' ->
          terminal_reset original;
          exit 0
      | _ -> loop selected
  in
  loop 0
