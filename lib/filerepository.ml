let file_path name =
  match Sys.getenv_opt "HOME" with
  | Some home -> home ^ "/.local/share/note/" ^ name
  | None -> failwith "HOME environment variable not set"

let create_dir_and_file name =
  let dir_path = Filename.dirname @@ file_path name in
  if not (Sys.file_exists dir_path) then Unix.mkdir dir_path 0o755;
  if not (Sys.file_exists @@ file_path name) then (
    let oc = open_out @@ file_path name in
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

let read_file_repository name =
  if not (Sys.file_exists @@ file_path name) then (
    let oc = open_out @@ file_path name in
    Yojson.Basic.to_channel oc (`List []);
    close_out oc);
  let ic =
    try open_in @@ file_path name
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

let write_file_repository name items =
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
  let oc = open_out @@ file_path name in
  Yojson.Basic.pretty_to_channel oc json;
  close_out oc

(* Add a new item to the list maintaining the IDs correctly sorted *)
let new_item_list name id content =
  let items = read_file_repository name in
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
(* 
   O codigo quebra caso a lista nao tenha pelo menos um item
   HACK: Caso tente remover o ultimo item da lista, retorna a lista original
*)
let rm_item_list name id =
  let list = read_file_repository name in
  if List.length list = 1 then list
  else
    List.filter (fun (i, _, _) -> i <> id) list
    |> List.map (fun (i, checked, item) ->
           if i > id then (i - 1, checked, item) else (i, checked, item))

let insert_item name index item =
  new_item_list name index item |> write_file_repository name

let get_item name index =
  let items = read_file_repository name in
  try Some (List.find (fun (i, _, _) -> i = index) items)
  with Not_found -> None

let remove_item name index =
  rm_item_list name index |> write_file_repository name
