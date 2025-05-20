val create_dir_and_file : unit -> unit
val read_file_repository : unit -> (int * bool * string) list
val write_file_repository : (int * bool * string) list -> unit
val insert_item : int -> string -> unit
val remove_item : int -> unit
