val create_dir_and_file : string -> unit
val read_file_repository : string -> (int * bool * string) list
val write_file_repository : string -> (int * bool * string) list -> unit
val insert_item : string -> int -> string -> unit
val get_item : string -> int -> (int * bool * string) option
val remove_item : string -> int -> unit
