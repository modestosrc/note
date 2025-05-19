let file_path = Sys.getenv "HOME" ^ "/.local/share/note/notes.json"

let create_dir_and_file () =
  let dir_path = Filename.dirname file_path in
  if not (Sys.file_exists dir_path) then (
    Unix.mkdir dir_path 0o755;
    let oc = open_out file_path in
    Yojson.Basic.to_channel oc (`List []);
    close_out oc);
  if not (Sys.file_exists file_path) then (
    let oc = open_out file_path in
    Yojson.Basic.to_channel oc (`List []);
    close_out oc)

let read_file_repository () =
  if not (Sys.file_exists file_path) then (
    let oc = open_out file_path in
    Yojson.Basic.to_channel oc (`List []);
    close_out oc);
  let ic = open_in file_path in
  let json = really_input_string ic (in_channel_length ic) in
  close_in ic;
  Yojson.Basic.from_string json
  |> Yojson.Basic.Util.to_list
  |> List.map (fun x ->
         let id = Yojson.Basic.Util.(to_int (member "id" x)) in
         let checked = Yojson.Basic.Util.(to_bool (member "checked" x)) in
         let item = Yojson.Basic.Util.(to_string (member "item" x)) in
         (id, checked, item))

  [@@@ocaml.warning "-32"]
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
  Yojson.Basic.to_channel oc json;
  close_out oc
