let file_path =
  match Sys.getenv_opt "HOME" with
  | Some home -> home ^ "/.local/share/note/notes.json"
  | None -> failwith "HOME environment variable not set"

let create_dir_and_file () =
  let dir_path = Filename.dirname file_path in
  if not (Sys.file_exists dir_path) then Unix.mkdir dir_path 0o755;
  if not (Sys.file_exists file_path) then (
    let oc = open_out file_path in
    Yojson.Basic.to_channel oc
      (`List
         [
           `Assoc
             [
               ("id", `Int 0);
               ("checked", `Bool false);
               ("item", `String "Bem vindo");
             ];
         ]);
    close_out oc)

let read_file_repository () =
  if not (Sys.file_exists file_path) then (
    let oc = open_out file_path in
    Yojson.Basic.to_channel oc (`List []);
    close_out oc);
  let ic =
    try open_in file_path
    with Sys_error _ -> failwith "Failed to open file repository"
  in
  let json = really_input_string ic (in_channel_length ic) in
  close_in ic;
  Yojson.Basic.from_string json
  |> Yojson.Basic.Util.to_list
  |> List.map (fun x ->
         let id = Yojson.Basic.Util.(to_int (member "id" x)) in
         let checked = Yojson.Basic.Util.(to_bool (member "checked" x)) in
         let item = Yojson.Basic.Util.(to_string (member "item" x)) in
         (id, checked, item))

let write_file_repository items =
  let json =
    `List
      (List.map
         (fun (id, checked, item) ->
           `Assoc
             [
               ("id", `Int id);
               ("checked", `Bool checked);
               ("item", `String item);
             ])
         items)
  in
  let oc = open_out file_path in
  Yojson.Basic.pretty_to_channel oc json;
  close_out oc

(* Add a new item to the list maintaining the IDs correctly sorted *)
let new_item_list id content =
  let items = read_file_repository () in
  let new_item = (id, false, content) in
  let updated_items =
    List.map
      (fun (i, checked, item) ->
        if i >= id then (i + 1, checked, item) else (i, checked, item))
      items
    @ [ new_item ]
  in
  List.sort (fun (id1, _, _) (id2, _, _) -> compare id1 id2) updated_items

(* Remove an item from the list and update the IDs *)
let rm_item_list id =
  let items = read_file_repository () in
  List.filter (fun (i, _, _) -> i <> id) items
  |> List.map (fun (i, checked, item) ->
         if i > id then (i - 1, checked, item) else (i, checked, item))

let insert_item index item = new_item_list index item |> write_file_repository

let get_item index =
  let items = read_file_repository () in
  try Some (List.find (fun (i, _, _) -> i = index) items)
  with Not_found -> None

let remove_item index = rm_item_list index |> write_file_repository
