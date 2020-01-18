module Datetime = CalendarLib.Calendar.Precise
module Date = Datetime.Date
module Time = Datetime.Time
module String_map = struct
  include Map.Make (String)

  let keys m = to_seq m |> Seq.map fst |> List.of_seq
  let values m = to_seq m |> Seq.map snd |> List.of_seq
end

module Datetime_p = CalendarLib.Printer.Precise_Calendar
module Date_p = CalendarLib.Printer.Date
module Time_p = CalendarLib.Printer.Time

open Rresult
open Astring

let connect_exn ?(reconnect = true) uri =
  let database =
    (* The database name to use is the path without the '/' prefix *)
    match Uri.path uri with
    | "" -> None
    | p ->
      Some
        (String.trim
           ~drop:(function
             | '/' -> true
             | _ -> false)
           p)
  in
  let options =
    if reconnect then
      Some [ Mysql.OPT_RECONNECT reconnect ]
    else
      None
  in
  Mysql.quick_connect ?options ?host:(Uri.host uri) ?database
    ?port:(Uri.port uri) ?password:(Uri.password uri) ?user:(Uri.user uri) ()

let connect ?reconnect uri =
  try connect_exn ?reconnect uri |> R.ok with
  | Mysql.Error msg -> R.error_msg msg

let disconnect = Mysql.disconnect

let ping = Mysql.ping

module Pp_internal = struct
  let null = Fmt.const Fmt.string "NULL"

  let nullable f = Fmt.option ~none:null f

  let string dbd = Fmt.of_to_string (Mysql.ml2rstr dbd)

  let blob dbd = Fmt.of_to_string (Mysql.ml2rblob dbd)

  let int = Fmt.of_to_string Mysql.ml2int

  let int64 = Fmt.of_to_string Mysql.ml642int

  let float = Fmt.of_to_string Mysql.ml2float

  let datetime =
    let to_string datetime =
      Mysql.ml2datetime
        ( Datetime.year datetime,
          Datetime.month datetime |> Date.int_of_month,
          Datetime.day_of_month datetime,
          Datetime.hour datetime,
          Datetime.minute datetime,
          Datetime.second datetime )
    in
    Fmt.of_to_string to_string

  let date =
    let to_string date =
      Mysql.ml2date
        ( Date.year date,
          Date.month date |> Date.int_of_month,
          Date.day_of_month date )
    in
    Fmt.of_to_string to_string

  let time =
    let to_string time =
      Mysql.ml2time (Time.hour time, Time.minute time, Time.second time)
    in
    Fmt.of_to_string to_string

  let csv_simple pp_elt fmt = Fmt.pf fmt "(%a)" (Fmt.list ~sep:Fmt.comma pp_elt)
end

module Column = struct
  type ('ocaml, 'sql) conv = {
    serialize : 'ocaml -> 'sql;
    deserialize : 'sql -> 'ocaml;
  }

  type ('ocaml, 'sql) t =
    | String : string * ('ocaml, (string as 'sql)) conv -> ('ocaml, 'sql) t
    | Blob : string * ('ocaml, (string as 'sql)) conv -> ('ocaml, 'sql) t
    | Int : string * ('ocaml, (int as 'sql)) conv -> ('ocaml, 'sql) t
    | Int64 : string * ('ocaml, (int64 as 'sql)) conv -> ('ocaml, 'sql) t
    | Float : string * ('ocaml, (float as 'sql)) conv -> ('ocaml, 'sql) t
    | Datetime :
        string * ('ocaml, (Datetime.t as 'sql)) conv
        -> ('ocaml, 'sql) t
    | Date : string * ('ocaml, (Date.t as 'sql)) conv -> ('ocaml, 'sql) t
    | Time : string * ('ocaml, (Time.t as 'sql)) conv -> ('ocaml, 'sql) t

  type packed = Pack : _ t -> packed

  type spec_basic = {
    name : string;
    nullable : bool;
  }

  type spec_string_width = {
    name : string;
    width : int;
    nullable : bool;
  }

  type spec_int = {
    name : string;
    nullable : bool;
    auto_increment : bool;
  }

  and (_, _) spec =
    | Char :
        spec_string_width * ('ocaml, (string as 'sql)) conv
        -> ('ocaml, 'sql) spec
    | Varchar :
        spec_string_width * ('ocaml, (string as 'sql)) conv
        -> ('ocaml, 'sql) spec
    | Binary :
        spec_string_width * ('ocaml, (string as 'sql)) conv
        -> ('ocaml, 'sql) spec
    | Blob : spec_basic * ('ocaml, (string as 'sql)) conv -> ('ocaml, 'sql) spec
    | Tiny_int : spec_int * ('ocaml, (int as 'sql)) conv -> ('ocaml, 'sql) spec
    | Small_int : spec_int * ('ocaml, (int as 'sql)) conv -> ('ocaml, 'sql) spec
    | Medium_int :
        spec_int * ('ocaml, (int as 'sql)) conv
        -> ('ocaml, 'sql) spec
    | Int : spec_int * ('ocaml, (int as 'sql)) conv -> ('ocaml, 'sql) spec
    | Big_int : spec_int * ('ocaml, (int64 as 'sql)) conv -> ('ocaml, 'sql) spec
    | Float : spec_basic * ('ocaml, (float as 'sql)) conv -> ('ocaml, 'sql) spec
    | Datetime :
        spec_basic * ('ocaml, (Datetime.t as 'sql)) conv
        -> ('ocaml, 'sql) spec
    | Date : spec_basic * ('ocaml, (Date.t as 'sql)) conv -> ('ocaml, 'sql) spec
    | Time : spec_basic * ('ocaml, (Time.t as 'sql)) conv -> ('ocaml, 'sql) spec

  type packed_spec = Pack : _ spec -> packed_spec

  let equal_packed_spec (a : packed_spec) (b : packed_spec) =
    match (a, b) with
    | (Pack (Char (a_spec, _)), Pack (Char (b_spec, _))) -> a_spec = b_spec
    | (Pack (Varchar (a_spec, _)), Pack (Varchar (b_spec, _))) ->
      a_spec = b_spec
    | (Pack (Binary (a_spec, _)), Pack (Binary (b_spec, _))) -> a_spec = b_spec
    | (Pack (Blob (a_spec, _)), Pack (Blob (b_spec, _))) -> a_spec = b_spec
    | (Pack (Tiny_int (a_spec, _)), Pack (Tiny_int (b_spec, _))) ->
      a_spec = b_spec
    | (Pack (Small_int (a_spec, _)), Pack (Small_int (b_spec, _))) ->
      a_spec = b_spec
    | (Pack (Medium_int (a_spec, _)), Pack (Medium_int (b_spec, _))) ->
      a_spec = b_spec
    | (Pack (Int (a_spec, _)), Pack (Int (b_spec, _))) -> a_spec = b_spec
    | (Pack (Big_int (a_spec, _)), Pack (Big_int (b_spec, _))) ->
      a_spec = b_spec
    | (Pack (Float (a_spec, _)), Pack (Float (b_spec, _))) -> a_spec = b_spec
    | (Pack (Datetime (a_spec, _)), Pack (Datetime (b_spec, _))) ->
      a_spec = b_spec
    | (Pack (Date (a_spec, _)), Pack (Date (b_spec, _))) -> a_spec = b_spec
    | (Pack (Time (a_spec, _)), Pack (Time (b_spec, _))) -> a_spec = b_spec
    | (Pack (Char _), _) -> false
    | (Pack (Varchar _), _) -> false
    | (Pack (Binary _), _) -> false
    | (Pack (Blob _), _) -> false
    | (Pack (Tiny_int _), _) -> false
    | (Pack (Small_int _), _) -> false
    | (Pack (Medium_int _), _) -> false
    | (Pack (Int _), _) -> false
    | (Pack (Big_int _), _) -> false
    | (Pack (Float _), _) -> false
    | (Pack (Datetime _), _) -> false
    | (Pack (Date _), _) -> false
    | (Pack (Time _), _) -> false

  module Conv = struct
    type ('ocaml, 'sql) t = ('ocaml, 'sql) conv

    let make ~serialize ~deserialize = { serialize; deserialize }

    let identity = { serialize = (fun x -> x); deserialize = (fun x -> x) }

    let bool =
      make
        ~serialize:(fun x ->
          if x then
            1
          else
            0)
        ~deserialize:(fun x -> x <> 0)
  end

  let of_spec (type o s) (spec : (o, s) spec) : (o, s) t =
    match spec with
    | Char ({ name; _ }, conv) -> String (name, conv)
    | Varchar ({ name; _ }, conv) -> String (name, conv)
    | Binary ({ name; _ }, conv) -> String (name, conv)
    | Blob ({ name; _ }, conv) -> Blob (name, conv)
    | Tiny_int ({ name; _ }, conv) -> Int (name, conv)
    | Small_int ({ name; _ }, conv) -> Int (name, conv)
    | Medium_int ({ name; _ }, conv) -> Int (name, conv)
    | Int ({ name; _ }, conv) -> Int (name, conv)
    | Big_int ({ name; _ }, conv) -> Int64 (name, conv)
    | Float ({ name; _ }, conv) -> Float (name, conv)
    | Datetime ({ name; _ }, conv) -> Datetime (name, conv)
    | Date ({ name; _ }, conv) -> Date (name, conv)
    | Time ({ name; _ }, conv) -> Time (name, conv)

  let spec_to_sql_type (type o s) (spec : (o, s) spec) =
    match spec with
    | Char ({ width; _ }, _) -> Fmt.strf "char(%d)" width
    | Varchar ({ width; _ }, _) -> Fmt.strf "varchar(%d)" width
    | Binary ({ width; _ }, _) -> Fmt.strf "binary(%d)" width
    | Blob _ -> "longblob"
    | Tiny_int _ -> "tinyint"
    | Small_int _ -> "smallint"
    | Medium_int _ -> "mediumint"
    | Int _ -> "int"
    | Big_int _ -> "bigint"
    | Float _ -> "float"
    | Datetime _ -> "datetime"
    | Date _ -> "date"
    | Time _ -> "time"

  let make_spec_string ~nullable ~auto_increment =
    let null =
      if nullable then
        ""
      else
        " not null"
    in
    let ai =
      if auto_increment then
        " auto_increment"
      else
        ""
    in
    String.concat ~sep:"" [ null; ai ]

  let spec_to_spec_string (type o s) (spec : (o, s) spec) =
    match spec with
    | Char ({ nullable; _ }, _) ->
      make_spec_string ~nullable ~auto_increment:false
    | Varchar ({ nullable; _ }, _) ->
      make_spec_string ~nullable ~auto_increment:false
    | Binary ({ nullable; _ }, _) ->
      make_spec_string ~nullable ~auto_increment:false
    | Blob ({ nullable; _ }, _) ->
      make_spec_string ~nullable ~auto_increment:false
    | Tiny_int ({ nullable; auto_increment; _ }, _) ->
      make_spec_string ~nullable ~auto_increment
    | Small_int ({ nullable; auto_increment; _ }, _) ->
      make_spec_string ~nullable ~auto_increment
    | Medium_int ({ nullable; auto_increment; _ }, _) ->
      make_spec_string ~nullable ~auto_increment
    | Int ({ nullable; auto_increment; _ }, _) ->
      make_spec_string ~nullable ~auto_increment
    | Big_int ({ nullable; auto_increment; _ }, _) ->
      make_spec_string ~nullable ~auto_increment
    | Float ({ nullable; _ }, _) ->
      make_spec_string ~nullable ~auto_increment:false
    | Datetime ({ nullable; _ }, _) ->
      make_spec_string ~nullable ~auto_increment:false
    | Date ({ nullable; _ }, _) ->
      make_spec_string ~nullable ~auto_increment:false
    | Time ({ nullable; _ }, _) ->
      make_spec_string ~nullable ~auto_increment:false

  let make_char name width ?(nullable = false) conv =
    Char ({ name; width; nullable }, conv)

  let make_varchar name width ?(nullable = false) conv =
    Varchar ({ name; width; nullable }, conv)

  let make_binary name width ?(nullable = false) conv =
    Binary ({ name; width; nullable }, conv)

  let make_blob name ?(nullable = false) conv = Blob ({ name; nullable }, conv)

  let make_tiny_int name ?(nullable = false) ?(auto_increment = false) conv =
    Tiny_int ({ name; nullable; auto_increment }, conv)

  let make_small_int name ?(nullable = false) ?(auto_increment = false) conv =
    Small_int ({ name; nullable; auto_increment }, conv)

  let make_medium_int name ?(nullable = false) ?(auto_increment = false) conv =
    Medium_int ({ name; nullable; auto_increment }, conv)

  let make_int name ?(nullable = false) ?(auto_increment = false) conv =
    Int ({ name; nullable; auto_increment }, conv)

  let make_big_int name ?(nullable = false) ?(auto_increment = false) conv =
    Big_int ({ name; nullable; auto_increment }, conv)

  let make_float name ?(nullable = false) conv = Float ({ name; nullable }, conv)

  let make_datetime name ?(nullable = false) conv =
    Datetime ({ name; nullable }, conv)

  let make_date name ?(nullable = false) conv = Date ({ name; nullable }, conv)

  let make_time name ?(nullable = false) conv = Time ({ name; nullable }, conv)

  let string name conv : _ t = String (name, conv)

  let blob name conv : _ t = Blob (name, conv)

  let int name conv : _ t = Int (name, conv)

  let int64 name conv : _ t = Int64 (name, conv)

  let float name conv : _ t = Float (name, conv)

  let datetime name conv : _ t = Datetime (name, conv)

  let date name conv : _ t = Date (name, conv)

  let time name conv : _ t = Time (name, conv)

  let name (type o s) (c : (o, s) t) =
    match c with
    | String (name, _) -> name
    | Blob (name, _) -> name
    | Int (name, _) -> name
    | Int64 (name, _) -> name
    | Float (name, _) -> name
    | Datetime (name, _) -> name
    | Date (name, _) -> name
    | Time (name, _) -> name

  let pp (type o s) dbd (c : (o, s) t) fmt (x : o) =
    match c with
    | String (_, conv) -> Pp_internal.string dbd fmt (conv.serialize x)
    | Blob (_, conv) -> Pp_internal.blob dbd fmt (conv.serialize x)
    | Int (_, conv) -> Pp_internal.int fmt (conv.serialize x)
    | Int64 (_, conv) -> Pp_internal.int64 fmt (conv.serialize x)
    | Float (_, conv) -> Pp_internal.float fmt (conv.serialize x)
    | Datetime (_, conv) -> Pp_internal.datetime fmt (conv.serialize x)
    | Date (_, conv) -> Pp_internal.date fmt (conv.serialize x)
    | Time (_, conv) -> Pp_internal.time fmt (conv.serialize x)

  let pp_spec dbd spec fmt x = pp dbd (of_spec spec) fmt x

  let pp_name fmt c = Fmt.of_to_string name fmt c

  let pp_spec_name fmt c = pp_name fmt (of_spec c)

  let of_packed_spec (Pack spec : packed_spec) : packed = Pack (of_spec spec)

  let name_of_spec spec = name (of_spec spec)

  let name_of_packed_spec (Pack spec) = name (of_spec spec)

  let with_name (type o s) name (spec : (o, s) spec) : (o, s) spec =
    let strip_auto_increment s = { s with auto_increment = false } in
    match spec with
    | Char (s, conv) -> Char ({ s with name }, conv)
    | Varchar (s, conv) -> Varchar ({ s with name }, conv)
    | Binary (s, conv) -> Binary ({ s with name }, conv)
    | Blob (s, conv) -> Blob ({ s with name }, conv)
    | Tiny_int (s, conv) ->
      Tiny_int ({ (strip_auto_increment s) with name }, conv)
    | Small_int (s, conv) ->
      Small_int ({ (strip_auto_increment s) with name }, conv)
    | Medium_int (s, conv) ->
      Medium_int ({ (strip_auto_increment s) with name }, conv)
    | Int (s, conv) -> Int ({ (strip_auto_increment s) with name }, conv)
    | Big_int (s, conv) -> Big_int ({ (strip_auto_increment s) with name }, conv)
    | Float (s, conv) -> Float ({ s with name }, conv)
    | Datetime (s, conv) -> Datetime ({ s with name }, conv)
    | Date (s, conv) -> Date ({ s with name }, conv)
    | Time (s, conv) -> Time ({ s with name }, conv)
end

module Field = struct
  type _ t =
    | String : string -> string t
    | Blob : string -> string t
    | Int : int -> int t
    | Int64 : int64 -> int64 t
    | Float : float -> float t
    | Datetime : Datetime.t -> Datetime.t t
    | Date : Date.t -> Date.t t
    | Time : Time.t -> Time.t t

  type packed = Pack : _ t -> packed

  type error = Unhandled_type of Mysql.dbty

  let pp (type v) dbd fmt (field : v t) =
    match field with
    | String s -> Pp_internal.string dbd fmt s
    | Blob b -> Pp_internal.blob dbd fmt b
    | Int i -> Pp_internal.int fmt i
    | Int64 i -> Pp_internal.int64 fmt i
    | Float f -> Pp_internal.float fmt f
    | Datetime c -> Pp_internal.datetime fmt c
    | Date d -> Pp_internal.date fmt d
    | Time t -> Pp_internal.time fmt t

  let pp_opt dbd fmt = function
    | Some fld -> pp dbd fmt fld
    | None -> Pp_internal.null fmt ()

  let pp_packed dbd fmt field =
    match field with
    | Pack fld -> pp dbd fmt fld

  let pp_packed_opt dbd fmt field =
    match field with
    | Some packed -> pp_packed dbd fmt packed
    | None -> Pp_internal.null fmt ()

  let to_string_unquoted = function
    | Pack (String s) -> s
    | Pack (Blob s) -> s
    | Pack (Int i) -> Mysql.ml2int i
    | Pack (Int64 i) -> Mysql.ml642int i
    | Pack (Float f) -> Mysql.ml2float f
    | Pack (Datetime c) -> Datetime_p.to_string c
    | Pack (Date d) -> Date_p.to_string d
    | Pack (Time t) -> Time_p.to_string t

  let opt_to_string_unquoted = function
    | None -> "NULL"
    | Some x -> to_string_unquoted x

  let string_of_dbty = function
    | Mysql.IntTy -> "IntTy"
    | Mysql.FloatTy -> "FloatTy"
    | Mysql.StringTy -> "StringTy"
    | Mysql.SetTy -> "SetTy"
    | Mysql.EnumTy -> "EnumTy"
    | Mysql.DateTimeTy -> "DateTimeTy"
    | Mysql.DateTy -> "DateTy"
    | Mysql.TimeTy -> "TimeTy"
    | Mysql.YearTy -> "YearTy"
    | Mysql.TimeStampTy -> "TimeStampeTy"
    | Mysql.UnknownTy -> "UnknownTy"
    | Mysql.Int64Ty -> "Int64Ty"
    | Mysql.BlobTy -> "BlobTy"
    | Mysql.DecimalTy -> "DecimalTy"

  let error_to_string = function
    | Unhandled_type typ -> Fmt.strf "Unhandled_type %s" (string_of_dbty typ)

  let datetime_of_tuple (y, m, d, hh, mm, ss) = Datetime.make y m d hh mm ss

  let date_of_tuple (y, m, d) = Date.make y m d

  let time_of_tuple (hh, mm, ss) = Time.make hh mm ss

  let of_mysql_type typ s =
    match typ with
    | Mysql.IntTy -> R.ok @@ Pack (Int (Mysql.int2ml s))
    | Mysql.Int64Ty -> R.ok @@ Pack (Int64 (Mysql.int642ml s))
    | Mysql.FloatTy -> R.ok @@ Pack (Float (Mysql.float2ml s))
    | Mysql.StringTy -> R.ok @@ Pack (String (Mysql.str2ml s))
    | Mysql.BlobTy -> R.ok @@ Pack (Blob (Mysql.blob2ml s))
    | Mysql.DateTimeTy ->
      R.ok @@ Pack (Datetime (Mysql.datetime2ml s |> datetime_of_tuple))
    | Mysql.DateTy -> R.ok @@ Pack (Date (Mysql.date2ml s |> date_of_tuple))
    | Mysql.TimeTy -> R.ok @@ Pack (Time (Mysql.time2ml s |> time_of_tuple))
    | ( Mysql.SetTy | Mysql.EnumTy | Mysql.YearTy | Mysql.TimeStampTy
      | Mysql.UnknownTy | Mysql.DecimalTy ) as typ ->
      R.error (`Mysql_field (Unhandled_type typ))

  let of_column_spec (type o s) (spec : (o, s) Column.spec) (v : o) : s t =
    match Column.of_spec spec with
    | Column.String (_, conv) -> String (conv.serialize v)
    | Column.Blob (_, conv) -> Blob (conv.serialize v)
    | Column.Int (_, conv) -> Int (conv.serialize v)
    | Column.Int64 (_, conv) -> Int64 (conv.serialize v)
    | Column.Float (_, conv) -> Float (conv.serialize v)
    | Column.Datetime (_, conv) -> Datetime (conv.serialize v)
    | Column.Date (_, conv) -> Date (conv.serialize v)
    | Column.Time (_, conv) -> Time (conv.serialize v)

  let unpack (type o s) (column : (o, s) Column.t) packed : o option =
    match packed with
    | Pack field ->
      ( match (column, field) with
      | (Column.String (_, conv), String v) -> Some (conv.deserialize v)
      | (Column.Blob (_, conv), Blob v) -> Some (conv.deserialize v)
      | (Column.Int (_, conv), Int v) -> Some (conv.deserialize v)
      | (Column.Int64 (_, conv), Int64 v) -> Some (conv.deserialize v)
      | (Column.Float (_, conv), Float v) -> Some (conv.deserialize v)
      | (Column.Datetime (_, conv), Datetime v) -> Some (conv.deserialize v)
      | (Column.Date (_, conv), Date v) -> Some (conv.deserialize v)
      | (Column.Time (_, conv), Time v) -> Some (conv.deserialize v)
      | _ -> None
      )
end

type 'kind sql = string constraint 'kind = [< `Run | `Get ]

type row = Field.packed option String_map.t

let row_of_list l = List.to_seq l |> String_map.of_seq

let pack_column_opt spec vo =
  let column = Column.of_spec spec in
  let name = Column.name column in
  match vo with
  | None -> (name, None)
  | Some v -> (name, Some (Field.Pack (Field.of_column_spec spec v)))

let pack_column spec v = pack_column_opt spec (Some v)

let find_column spec (row : row) =
  let column = Column.of_spec spec in
  let name = Column.name column in
  match String_map.find_opt name row with
  | None -> R.error_msgf "no column %s in row" name
  | Some None -> R.ok None
  | Some (Some packed) ->
    ( match Field.unpack column packed with
    | None -> R.error_msgf "field type mismatch for %s in row" name
    | Some v -> R.ok (Some v)
    )

let get_column spec row =
  match find_column spec row with
  | Error (`Msg msg) -> invalid_arg msg
  | Ok None -> raise Not_found
  | Ok (Some v) -> v

let make_run fmt = Fmt.kstrf (fun x -> x) fmt

let make_get fmt = Fmt.kstrf (fun x -> x) fmt

let insert' dbd ~into:table fields fmt =
  let columns = String_map.keys fields in
  let values = String_map.values fields in
  Fmt.kstrf
    (fun s ->
      make_run "insert into %s %a values %a %s" table
        (Pp_internal.csv_simple Fmt.string)
        columns
        (Pp_internal.csv_simple (Field.pp_packed_opt dbd))
        values s)
    fmt

let pp_update fmt column = Fmt.pf fmt "%s = values(%s)" column column

let insert ?on_duplicate_key_update dbd ~into row =
  match on_duplicate_key_update with
  | None -> insert' dbd ~into row ""
  | Some update ->
    let (id_column, columns) =
      match update with
      | `All -> (None, String_map.keys row)
      | `Columns columns -> (None, columns)
      | `Except columns ->
        ( None,
          List.filter (fun name -> List.mem name columns) (String_map.keys row)
        )
      | `With_id (id_column, columns) -> (Some id_column, columns)
    in
    let id_column_sql =
      match id_column with
      | None -> ""
      | Some column ->
        let pp_sep =
          match columns with
          | [] -> Fmt.nop
          | _ -> Fmt.comma
        in
        (* If a column is specified, make sure last_insert_id identifies that
           value once/if this insert completes successfully. *)
        Fmt.strf "%a%s = last_insert_id(%s)" pp_sep () column column
    in
    insert' dbd ~into row "on duplicate key update %a%s"
      (Fmt.list ~sep:Fmt.comma pp_update)
      columns id_column_sql

let replace = `Use_insert_on_duplicate_key_update

let update table fmt = Fmt.kstrf (fun s -> Fmt.strf "update %s %s" table s) fmt

let delete ~from:table fmt =
  Fmt.kstrf (fun s -> Fmt.strf "delete from %s %s" table s) fmt

let select columns ~from:table fmt =
  Fmt.kstrf
    (fun s ->
      Fmt.strf "select %a from %s %s"
        Fmt.(list ~sep:comma string)
        columns table s)
    fmt

let field_of_mysql_type_exn typ s =
  match Field.of_mysql_type typ s with
  | Ok f -> f
  | Error (`Mysql_field e) -> invalid_arg (Field.error_to_string e)

let parse_row columns row =
  let num_columns = Array.length columns in
  if num_columns <> Array.length row then
    invalid_arg "mysql: metadata column count mismatch";
  Array.mapi
    (fun i col ->
      ( Mysql.(col.name),
        Option.map (field_of_mysql_type_exn Mysql.(col.ty)) row.(i) ))
    columns
  |> Array.to_seq
  |> String_map.of_seq

let to_rows columns result =
  let rec loop rows =
    match Mysql.fetch result with
    | None -> List.rev rows
    | Some row -> loop (parse_row columns row :: rows)
  in
  loop []

let exec dbd sql =
  let result = Mysql.exec dbd sql in
  let columns = Mysql.fetch_fields result in
  match columns with
  | None -> None
  | Some columns -> Some (to_rows columns result)

let run_exn dbd sql =
  match exec dbd sql with
  | None -> ()
  | Some _rows -> Fmt.failwith "Ezmysql.run: unexpected results from %s" sql

let run dbd sql =
  match run_exn dbd sql with
  | () -> Ok ()
  | exception Failure msg -> R.error_msg msg
  | exception Mysql.Error msg ->
    R.error_msgf "Ezmysql.run: %a from %s" Fmt.(brackets string) msg sql

let get_exn dbd sql =
  match exec dbd sql with
  | None -> Fmt.failwith "Ezmysql.get: empty result from %s" sql
  | Some rows -> rows

let get dbd sql =
  match get_exn dbd sql with
  | rows -> Ok rows
  | exception Failure msg -> R.error_msg msg
  | exception Mysql.Error msg ->
    R.error_msgf "Ezmysql.get: %a from %s" Fmt.(brackets string) msg sql

let get_v (type o) (column : (o, _) Column.t) (row : row) : o option =
  let name = Column.name column in
  match String_map.find_opt name row with
  | None -> Fmt.invalid_arg "Ezmysql.get_v: No column %s in row" name
  | Some v ->
    ( match v with
    | None -> None
    | Some packed_field ->
      ( match Field.unpack column packed_field with
      | Some _ as s -> s
      | None ->
        Fmt.invalid_arg
          "Ezmysql.get_v: column %s's type does not match what was expected"
          name
      )
    )

let list_map f l = List.rev_map f l |> List.rev

let to_column rows column = list_map (fun row -> get_v column row) rows

let to_column2 rows (column1, column2) =
  list_map (fun row -> (get_v column1 row, get_v column2 row)) rows

let to_column3 rows (column1, column2, column3) =
  list_map
    (fun row -> (get_v column1 row, get_v column2 row, get_v column3 row))
    rows

let to_column4 rows (column1, column2, column3, column4) =
  list_map
    (fun row ->
      ( get_v column1 row,
        get_v column2 row,
        get_v column3 row,
        get_v column4 row ))
    rows

let to_column5 rows (column1, column2, column3, column4, column5) =
  list_map
    (fun row ->
      ( get_v column1 row,
        get_v column2 row,
        get_v column3 row,
        get_v column4 row,
        get_v column5 row ))
    rows

let start_transaction_sql = "start transaction"

let start_transaction dbd = run dbd start_transaction_sql

let start_transaction_exn dbd = run_exn dbd start_transaction_sql

let commit_sql = "commit"

let commit dbd = run dbd commit_sql

let commit_exn dbd = run_exn dbd commit_sql

let rollback_sql = "rollback"

let rollback dbd = run dbd rollback_sql

let rollback_exn dbd = run_exn dbd rollback_sql

let with_transaction dbd f =
  let ( >>= ) = R.( >>= ) in
  start_transaction dbd >>= fun () ->
  match f () with
  | Ok _ as o -> commit dbd >>= fun () -> o
  | Error _ as e -> rollback dbd >>= fun () -> e
  | exception exn -> rollback dbd >>= fun () -> raise exn

let with_transaction_exn dbd f =
  start_transaction_exn dbd;
  match f () with
  | v ->
    commit_exn dbd;
    v
  | exception exn ->
    rollback_exn dbd;
    raise exn

module Prepared = struct
  type 'a t = {
    dbd : Mysql.dbd;
    sql : string;
    mutable statement : Mysql.Prepared.stmt;
  }
    constraint 'a = [< `Run | `Get ]

  let make dbd sql = { dbd; sql; statement = Mysql.Prepared.create dbd sql }

  let make_run dbd sql =
    try Ok (make dbd sql) with
    | Mysql.Error msg -> R.error_msg msg

  let make_get dbd sql =
    try Ok (make dbd sql) with
    | Mysql.Error msg -> R.error_msg msg

  let prepare_parameters fields =
    Array.map Field.opt_to_string_unquoted (Array.of_list fields)

  let to_rows columns result =
    let rec loop rows =
      match Mysql.Prepared.fetch result with
      | None -> Ok (List.rev rows)
      | Some row -> loop (parse_row columns row :: rows)
      | exception Mysql.Error msg -> R.error_msg msg
    in
    loop []

  let exec ps fields =
    let ( >>= ) = R.( >>= ) in
    let params = prepare_parameters fields in
    ( match Mysql.Prepared.execute ps.statement params with
    | x -> Ok x
    | exception Mysql.Error msg ->
      R.error_msgf "While executing a prepared statement: %s" msg
    )
    >>= fun result ->
    let columns =
      Mysql.Prepared.result_metadata ps.statement |> Mysql.fetch_fields
    in
    match columns with
    | None -> Ok None
    | Some columns -> Ok (Some (to_rows columns result))

  let run ps fields =
    match exec ps fields with
    | Error _ as e -> e
    | Ok None -> Ok ()
    | Ok (Some _rows) ->
      R.error_msgf "Ezmysql.Prepared.run: unexpected results from %s" ps.sql
    | exception Mysql.Error msg ->
      R.error_msgf "Ezmysql.Prepared.run: %a from %s"
        Fmt.(brackets string)
        msg ps.sql

  let get ps fields =
    match exec ps fields with
    | Error _ as e -> e
    | Ok None ->
      R.error_msgf "Ezmysql.Prepared.get: empty result from %s" ps.sql
    | Ok (Some rows) -> rows
    | exception Mysql.Error msg ->
      R.error_msgf "Ezmysql.Prepared.get: %a from %s"
        Fmt.(brackets string)
        msg ps.sql

  let remake ps =
    (* Assume things were broken and we need to remake the statement *)
    try
      Mysql.ping ps.dbd;
      ps.statement <- Mysql.Prepared.create ps.dbd ps.sql;
      Ok ()
    with
    | Mysql.Error msg -> R.error_msg msg

  let close ps = Mysql.Prepared.close ps.statement
end

module Table = struct
  type t = {
    name : string;
    columns : Column.packed_spec list;
    primary_key : Column.packed_spec list;
    indices : (string * index_field list) list;
    unique_keys : (string * Column.packed_spec list) list;
    foreign_keys : foreign_key list;
    deps : t list;
  }

  and foreign_key = {
    key_name : string option;
    keys : foreign_key_mapping;
    on_update : foreign_key_action;
    on_delete : foreign_key_action;
  }

  and foreign_key_mapping = {
    foreign_table : t;
    key_mapping : key_mapping list;
  }

  and foreign_key_action =
    | Restrict
    | Cascade
    | Set_null
    | No_action

  and key_mapping =
    | Key : {
        local : ('ocaml, 'sql) Column.spec;
        remote : ('ocaml, 'sql) Column.spec;
      }
        -> key_mapping

  and index_field =
    | Column : (_, _) Column.spec -> index_field
    | Prefix : {
        column : (_, _) Column.spec;
        width : int;
      }
        -> index_field

  let column_of_index_field : index_field -> Column.packed_spec = function
    | Column spec -> Pack spec
    | Prefix { column = spec; _ } -> Pack spec

  let name table = table.name

  let mem_columns ~truth ~test =
    List.fold_left
      (fun ok test_c ->
        ok
        && List.exists
             (fun truth_c -> Column.equal_packed_spec test_c truth_c)
             truth)
      true test

  let mem_columns_multiple ~truth ~tests =
    List.fold_left (fun ok test -> ok && mem_columns ~truth ~test) true tests

  let mem_columns_fk ~truth ~tests =
    List.fold_left
      (fun ok fk ->
        let local_columns =
          List.map
            (fun (Key { local; _ }) -> (Pack local : Column.packed_spec))
            fk.keys.key_mapping
        in
        ok && mem_columns ~truth ~test:local_columns)
      true tests

  let make_foreign_key ?key_name foreign_table key_mapping ~on_update ~on_delete
      =
    match key_mapping with
    | [] ->
      invalid_arg
        "Ezmysql.Table.make_foreign_key requires a non-empty list of fields"
    | _ ->
      let everything_ok =
        let remote_columns =
          List.map (fun (Key { remote; _ }) -> Column.Pack remote) key_mapping
        in
        mem_columns ~truth:foreign_table.columns ~test:remote_columns
      in
      if not everything_ok then
        invalid_arg
        @@ Fmt.strf
             "Ezmysql.Table.make_foreign_key refers to columns absent from %s"
             foreign_table.name;
      { key_name; keys = { foreign_table; key_mapping }; on_update; on_delete }

  let make
      ?(primary_key = [])
      ?(indices = [])
      ?(unique_keys = [])
      ?(foreign_keys = [])
      name
      columns =
    if String.is_empty name then
      invalid_arg "Ezmysql.Table.make requires a non-empty table name";
    ( match columns with
    | [] -> invalid_arg "Ezmysql.Table.make requires a non-empty column list"
    | _ -> ()
    );
    let deps = List.map (fun fk -> fk.keys.foreign_table) foreign_keys in
    let everything_ok =
      let indices_columns =
        List.map
          (fun (_name, index) ->
            List.map
              (fun index_field -> column_of_index_field index_field)
              index)
          indices
      in
      let unique_keys_columns =
        List.map (fun (_name, columns) -> columns) unique_keys
      in
      mem_columns ~truth:columns ~test:primary_key
      && mem_columns_multiple ~truth:columns ~tests:indices_columns
      && mem_columns_multiple ~truth:columns ~tests:unique_keys_columns
      && mem_columns_fk ~truth:columns ~tests:foreign_keys
    in
    if not everything_ok then
      invalid_arg "Ezmysql.Table.make: key or index refers to absent column";
    { name; columns; primary_key; indices; unique_keys; foreign_keys; deps }

  let pp_column_type fmt (Column.Pack c) =
    Fmt.of_to_string Column.spec_to_sql_type fmt c

  let pp_column_spec fmt (Column.Pack c) =
    Fmt.of_to_string Column.spec_to_spec_string fmt c

  let pp_column_name fmt (Column.Pack c) = Column.pp_spec_name fmt c

  let pp_column fmt column =
    Fmt.pf fmt "@[%a %a%a@]" pp_column_name column pp_column_type column
      pp_column_spec column

  let pp_primary_key fmt pk =
    match pk with
    | [] -> ()
    | _ ->
      Fmt.comma fmt ();
      Fmt.pf fmt "@[primary@ key@ %a@]"
        (Pp_internal.csv_simple pp_column_name)
        pk

  let pp_index_field fmt index_field =
    match index_field with
    | Column spec -> Fmt.pf fmt "%a" pp_column_name (Pack spec)
    | Prefix { column; width } ->
      Fmt.pf fmt "%a(%d)" pp_column_name (Pack column) width

  let pp_index fmt (name, index) =
    match index with
    | [] -> ()
    | _ ->
      Fmt.pf fmt "@[index@ %s@ %a@]" name
        (Pp_internal.csv_simple pp_index_field)
        index

  let pp_indices fmt indices =
    match indices with
    | [] -> ()
    | _ ->
      Fmt.comma fmt ();
      Fmt.list ~sep:Fmt.comma pp_index fmt indices

  let pp_unique_key fmt (name, uk) =
    match uk with
    | [] -> ()
    | _ ->
      Fmt.pf fmt "@[unique@ key@ %s@ %a@]" name
        (Pp_internal.csv_simple pp_column_name)
        uk

  let pp_unique_keys fmt unique_keys =
    match unique_keys with
    | [] -> ()
    | _ ->
      Fmt.comma fmt ();
      Fmt.list ~sep:Fmt.comma pp_unique_key fmt unique_keys

  let foreign_key_action_to_string = function
    | Restrict -> "restrict"
    | Cascade -> "cascade"
    | Set_null -> "set null"
    | No_action -> "no action"

  let pp_foreign_key_action = Fmt.of_to_string foreign_key_action_to_string

  let pp_foreign_key fmt fk =
    match fk.keys.key_mapping with
    | [] -> ()
    | _ ->
      let (local, foreign) =
        List.map
          (fun (Key { local; remote }) ->
            (Column.Pack local, Column.Pack remote))
          fk.keys.key_mapping
        |> List.split
      in
      Fmt.pf fmt "@[foreign@ key@ ";
      ( match fk.key_name with
      | None -> ()
      | Some name -> Fmt.pf fmt "%s@ " name
      );
      Fmt.pf fmt "%a@;" (Pp_internal.csv_simple pp_column_name) local;
      Fmt.pf fmt "references@ %s%a@;" fk.keys.foreign_table.name
        (Pp_internal.csv_simple pp_column_name)
        foreign;
      Fmt.pf fmt "on@ update@ %a@;on@ delete@ %a@]" pp_foreign_key_action
        fk.on_update pp_foreign_key_action fk.on_delete

  let pp_foreign_keys fmt fks =
    match fks with
    | [] -> ()
    | _ ->
      Fmt.comma fmt ();
      Fmt.list ~sep:Fmt.comma pp_foreign_key fmt fks

  let pp_sql fmt ~ok_if_exists table =
    Fmt.pf fmt "@[";
    Fmt.pf fmt "@[create@ table@]@ ";
    if ok_if_exists then Fmt.pf fmt "@[if@ not@ exists@]@ ";
    Fmt.pf fmt "%s" table.name;
    Fmt.pf fmt "@;@[<1>(%a" (Fmt.list ~sep:Fmt.comma pp_column) table.columns;
    Fmt.pf fmt "%a" pp_primary_key table.primary_key;
    Fmt.pf fmt "%a" pp_indices table.indices;
    Fmt.pf fmt "%a" pp_unique_keys table.unique_keys;
    Fmt.pf fmt "%a" pp_foreign_keys table.foreign_keys;
    Fmt.pf fmt ")@]"

  let pp_name fmt table = Fmt.string fmt (name table)

  let create_exn dbd table ~ok_if_exists =
    let sql = make_run "%a" (pp_sql ~ok_if_exists) table in
    run_exn dbd sql

  let create dbd table ~ok_if_exists =
    let sql = make_run "%a" (pp_sql ~ok_if_exists) table in
    run dbd sql

  let deps table = table.deps

  module Topo_sort = struct
    module V : Graph.Sig.COMPARABLE with type t = t = struct
      (* Graph vertices are tables, unique by name *)
      type nonrec t = t

      let compare (a : t) (b : t) = compare a.name b.name

      let equal (a : t) (b : t) = String.equal a.name b.name

      let hash (a : t) = Hashtbl.hash a.name
    end

    (* Persistent (immutable) graphs and matching topological sort modules *)
    module G = Graph.Persistent.Digraph.Concrete (V)
    module Topo = Graph.Topological.Make_stable (G)

    (* Add a table to an existing graph. [table] must already exist in [graph]. *)
    let rec add_table graph table =
      if G.mem_vertex graph table then
        graph
      else (
        (* Make sure we add the table to our graph! *)
        let graph = G.add_vertex graph table in
        (* Now recursively add all of the table's dependencies *)
        List.fold_left
          (fun graph_accumulator table_dep ->
            (* Add this table and record the dependency. *)
            let graph_accumulator =
              let graph = G.add_vertex graph_accumulator table_dep in
              G.add_edge graph table table_dep
            in
            (* Now recurse in and add any dependencies for table_dep *)
            add_table graph_accumulator table_dep)
          graph table.deps
      )

    let init tables = List.fold_left add_table G.empty tables

    let sorted_deps graph =
      Topo.fold (fun table all_deps -> table :: all_deps) graph []
  end

  let transitive_sorted_deps tables =
    Topo_sort.init tables |> Topo_sort.sorted_deps
end

module type S = sig
  type t

  val table : Table.t

  val to_row : t -> row

  val of_row : row -> (t, [> `Msg of string ]) result
end

module type Db = sig
  type t

  val table : Table.t

  val init :
    Mysql.dbd -> ok_if_exists:bool -> (unit, [> `Msg of string ]) result

  val init_exn : Mysql.dbd -> ok_if_exists:bool -> unit

  val insert' :
    Mysql.dbd ->
    t ->
    ('a, Format.formatter, unit, (unit, [> R.msg ]) result) format4 ->
    'a

  val insert_exn' :
    Mysql.dbd -> t -> ('a, Format.formatter, unit, unit) format4 -> 'a

  val insert_sql :
    ?on_duplicate_key_update:
      [ `All
      | `Columns of Column.packed_spec list
      | `Except of Column.packed_spec list
      | `With_id of (_, _) Column.spec * Column.packed_spec list
      ] ->
    Mysql.dbd ->
    t ->
    [ `Run ] sql

  val insert :
    ?on_duplicate_key_update:
      [ `All
      | `Columns of Column.packed_spec list
      | `Except of Column.packed_spec list
      | `With_id of (_, _) Column.spec * Column.packed_spec list
      ] ->
    Mysql.dbd ->
    t ->
    (unit, [> `Msg of string ]) result

  val insert_exn :
    ?on_duplicate_key_update:
      [ `All
      | `Columns of Column.packed_spec list
      | `Except of Column.packed_spec list
      | `With_id of (_, _) Column.spec * Column.packed_spec list
      ] ->
    Mysql.dbd ->
    t ->
    unit

  val replace : [ `Use_insert_on_duplicate_key_update ]
    [@@ocaml.deprecated "Use 'insert ~on_duplicate_key_update' instead"]

  val update_sql : ('a, Format.formatter, unit, [ `Run ] sql) format4 -> 'a

  val update :
    Mysql.dbd ->
    ('a, Format.formatter, unit, (unit, [> `Msg of string ]) result) format4 ->
    'a

  val update_exn : Mysql.dbd -> ('a, Format.formatter, unit, unit) format4 -> 'a

  val select_sql : ('a, Format.formatter, unit, [ `Get ] sql) format4 -> 'a

  val select :
    Mysql.dbd ->
    ('a, Format.formatter, unit, (t list, [> `Msg of string ]) result) format4 ->
    'a

  val select_exn :
    Mysql.dbd -> ('a, Format.formatter, unit, t list) format4 -> 'a

  val delete_sql : ('a, Format.formatter, unit, [ `Run ] sql) format4 -> 'a

  val delete :
    Mysql.dbd ->
    ('a, Format.formatter, unit, (unit, [> `Msg of string ]) result) format4 ->
    'a

  val delete_exn : Mysql.dbd -> ('a, Format.formatter, unit, unit) format4 -> 'a
end

module Make (M : S) : Db with type t := M.t = struct
  let table = M.table

  let init dbd ~ok_if_exists = Table.create dbd M.table ~ok_if_exists

  let init_exn dbd ~ok_if_exists = Table.create_exn dbd M.table ~ok_if_exists

  let insert'_sql runner dbd t fmt =
    let row = M.to_row t in
    Fmt.kstrf
      (fun s -> insert' dbd ~into:(Table.name M.table) row "%s" s |> runner dbd)
      fmt

  let insert' dbd t fmt = insert'_sql run dbd t fmt

  let insert_exn' dbd t fmt = insert'_sql run_exn dbd t fmt

  let on_duplicate_key_update_to_strings = function
    | `All -> `All
    | `Columns specs -> `Columns (List.map Column.name_of_packed_spec specs)
    | `Except specs -> `Except (List.map Column.name_of_packed_spec specs)
    | `With_id (id_spec, specs) ->
      `With_id
        (Column.name_of_spec id_spec, List.map Column.name_of_packed_spec specs)

  let insert_sql ?on_duplicate_key_update dbd t =
    let row = M.to_row t in
    let on_duplicate_key_update =
      match on_duplicate_key_update with
      | None -> None
      | Some x -> Some (on_duplicate_key_update_to_strings x)
    in
    insert ?on_duplicate_key_update dbd ~into:M.table.Table.name row

  let insert ?on_duplicate_key_update dbd t =
    run dbd (insert_sql ?on_duplicate_key_update dbd t)

  let insert_exn ?on_duplicate_key_update dbd t =
    run_exn dbd (insert_sql ?on_duplicate_key_update dbd t)

  let replace = `Use_insert_on_duplicate_key_update

  let update_sql clauses =
    Fmt.kstrf (fun s -> update M.table.name "%s" s) clauses

  let update_exn dbd clauses =
    Fmt.kstrf (fun s -> update M.table.name "%s" s |> run_exn dbd) clauses

  let update dbd clauses =
    Fmt.kstrf (fun s -> update M.table.name "%s" s |> run dbd) clauses

  exception Error of string

  let of_row_exn row =
    match M.of_row row with
    | Ok x -> x
    | Error (`Msg msg) -> raise (Error msg)

  let select_sql clauses =
    Fmt.kstrf (fun s -> select [ "*" ] ~from:M.table.Table.name "%s" s) clauses

  let select_exn dbd clauses =
    Fmt.kstrf
      (fun s ->
        let rows =
          select [ "*" ] ~from:M.table.Table.name "%s" s |> get_exn dbd
        in
        try List.rev_map of_row_exn rows |> List.rev with
        | Error msg -> failwith msg)
      clauses

  let select dbd clauses =
    let ( >>= ) = R.( >>= ) in
    Fmt.kstrf
      (fun s ->
        select [ "*" ] ~from:M.table.Table.name "%s" s |> get dbd
        >>= fun rows ->
        try List.rev_map of_row_exn rows |> List.rev |> R.ok with
        | Error msg -> R.error_msg msg)
      clauses

  let delete_sql clauses =
    Fmt.kstrf (fun s -> delete ~from:M.table.Table.name "%s" s) clauses

  let delete_exn dbd clauses =
    Fmt.kstrf
      (fun s -> delete ~from:M.table.Table.name "%s" s |> run_exn dbd)
      clauses

  let delete dbd clauses =
    Fmt.kstrf
      (fun s -> delete ~from:M.table.Table.name "%s" s |> run dbd)
      clauses
end

module Clause = struct
  type comparison =
    | Eq
    | Ne
    | Lt
    | Gt
    | Lte
    | Gte

  type 'ocaml column =
    | Column : ('ocaml, 'sql) Column.t -> 'ocaml column
    | Spec : ('ocaml, 'sql) Column.spec -> 'ocaml column

  type t =
    | And : t * t -> t
    | Or : t * t -> t
    | Compare : comparison * 'ocaml column * 'ocaml -> t

  let make_compare comparison column v = Compare (comparison, column, v)

  let ( = ) col v = make_compare Eq col v

  let ( <> ) col v = make_compare Ne col v

  let ( < ) col v = make_compare Lt col v

  let ( > ) col v = make_compare Gt col v

  let ( <= ) col v = make_compare Lte col v

  let ( >= ) col v = make_compare Gte col v

  let ( && ) a b = And (a, b)

  let ( || ) a b = Or (a, b)

  let string_of_comparison comp =
    match comp with
    | Eq -> "="
    | Ne -> "<>"
    | Lt -> "<"
    | Gt -> ">"
    | Lte -> "<="
    | Gte -> ">="

  let pp_column_name fmt column =
    match column with
    | Column c -> Column.pp_name fmt c
    | Spec s -> Column.pp_spec_name fmt s

  let pp_comparison = Fmt.of_to_string string_of_comparison

  let pp_value dbd column =
    match column with
    | Column c -> Column.pp dbd c
    | Spec s -> Column.pp_spec dbd s

  let rec pp dbd fmt clause =
    match clause with
    | And (left, right) ->
      Fmt.pf fmt "(%a@ and@ %a)" (pp dbd) left (pp dbd) right
    | Or (left, right) ->
      Fmt.pf fmt "(%a@ or @ %a)" (pp dbd) left (pp dbd) right
    | Compare (comparison, column, v) ->
      Fmt.pf fmt "%a@ %a@ %a" pp_column_name column pp_comparison comparison
        (pp_value dbd column) v
end

module Pp = struct
  include Pp_internal

  let column = Column.pp

  let column_name = Column.pp_name

  let spec = Column.pp_spec

  let spec_name = Column.pp_spec_name

  let field = Field.pp

  let field_opt = Field.pp_opt

  let table_name = Table.pp_name

  let clause = Clause.pp
end
