# Simple MySQL interface for OCaml

Some sample code:

```ocaml
open Rresult

(* Force an unwrap of a result and quit the program if there's an error *)
let die_on_error = function
  | Ok x -> x
  | Error msg -> Fmt.failwith "ERROR: %a" R.pp_msg msg

let uri = Uri.of_string "mysql://user:pass@127.0.0.1/database"
let conn = Ezmysql.connect uri |> die_on_error

let insert_sql =
  let row =
    Ezmysql.row_of_list
      [
        ("column_of_type_string", Some (Pack (Ezmysql.Field.String "value_1")));
        ("column_of_type_int", Some (Pack (Ezmysql.Field.Int 123)));
        ( "column_of_type_datetime",
          Some (Pack (Ezmysql.Field.Datetime (Ezmysql.Datetime.now ()))) );
      ]
  in
  Ezmysql.insert conn ~into:"table_name" row

let () = Ezmysql.run conn insert_sql |> die_on_error

let select_sql =
  Ezmysql.select
    [ "column_of_type_string"; "column_of_type_int" ]
    ~from:"table_name" "where stuff = %a limit 10" (Ezmysql.Pp.string conn)
    "this;.\\ string needs''''escapes"

let rows = Ezmysql.get conn select_sql |> die_on_error

let only_column_of_type_string =
  let column =
    Ezmysql.Column.make_varchar "column_of_type_string" 4_096
      Ezmysql.Column.Conv.identity
  in
  Ezmysql.to_column rows (Ezmysql.Column.of_spec column)

module My_special_table = struct
  let id =
    Ezmysql.Column.make_int "id" ~auto_increment:true
      Ezmysql.Column.Conv.identity
  let sha256 = Ezmysql.Column.make_char "sha256" 64 Ezmysql.Column.Conv.identity
  let last_seen =
    Ezmysql.Column.make_datetime "last_seen" Ezmysql.Column.Conv.identity

  (* Create a table definition and derive basic table interactions from that *)
  let table =
    Ezmysql.Table.make ~primary_key:[ Pack id ] "my_special_table"
      [ Pack id; Pack sha256; Pack last_seen ]

  type t = {
    id : int;
    sha256 : string;
    last_seen : Ezmysql.Datetime.t;
  }

  let to_row (entry : t) =
    Ezmysql.row_of_list
      [
        Ezmysql.pack_column id entry.id;
        Ezmysql.pack_column sha256 entry.sha256;
        Ezmysql.pack_column last_seen entry.last_seen;
      ]

  let of_row row =
    let get c = Ezmysql.get_column c row in
    try Ok { id = get id; sha256 = get sha256; last_seen = get last_seen } with
    | _ -> R.error_msgf "Invalid %a row" Ezmysql.Pp.table_name table

  include Ezmysql.Make (struct
    type nonrec t = t
    let table = table
    let to_row = to_row
    let of_row = of_row
  end)
end

let () =
  My_special_table.insert conn
    {
      My_special_table.id = 1;
      sha256 =
        "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855";
      last_seen = Ezmysql.Datetime.now ();
    }
  |> die_on_error

let my_special_table_rows id =
  My_special_table.select conn "where id = %a" Ezmysql.Pp.int id

let () = My_special_table.delete conn "where id = 1" |> die_on_error
```
