module Datetime = CalendarLib.Calendar.Precise
module Date = Datetime.Date
module Time = Datetime.Time
module String_map :
  Map.S with type key = string with type 'a t = 'a Map.Make(String).t

val connect :
  ?reconnect:bool -> Uri.t -> (Mysql.dbd, [> `Msg of string ]) result
(** [connect uri] connects to [uri].

    @param reconnect specifies whether a {!ping} should automatically reconnect
    if [dbd] has dropped. Defaults to [true]. *)

val connect_exn : ?reconnect:bool -> Uri.t -> Mysql.dbd
(** Like {!connect} but raises {!Mysql.Error} if the connection cannot be made. *)

val disconnect : Mysql.dbd -> unit
(** [disconnect dbd] disconnects from the server *)

val ping : Mysql.dbd -> unit
(** [ping dbd] pings the remote connection for liveness, re-establishing a
    connection is the connection is not live and automatic reconnection is
    enabled for [dbd]. *)

module Column : sig
  (** {1 A database column} *)

  module Conv : sig
    (** {1 Conversion between OCaml and SQL values} *)

    type ('ocaml, 'sql) t
    (** [('ocaml, 'sql) t] defines a two-way mapping between [ocaml] and [sql]
        representations of data. The ['ocaml] type parameter indicates the type
        representation in OCaml code and the ['sql] type parameter indicates the
        type representation in SQL. *)

    val make :
      serialize:('ocaml -> 'sql) ->
      deserialize:('sql -> 'ocaml) ->
      ('ocaml, 'sql) t
    (** [make ~serialize ~deserialize] will use [serialize] to convert from
        OCaml to SQL representation and [deserialize] to convert from SQL to
        OCaml. *)

    val identity : ('a, 'a) t
    (** Identity conversion - the same representation is used in OCaml and SQL *)

    val bool : (bool, int) t
    (** Boolean conversion with SQL representing [false] as [0] and [true] as
        any non-zero value *)
  end

  (** {2 Basic column identification}

      The following functions define {!t} values with a column name and a
      {!Conv.t} defining how to translate values between OCaml and SQL. *)

  type ('ocaml, 'sql) t
  (** A named column and the element type associated with it *)

  (** Used for packing multiple columns into a single data structure *)
  type packed = Pack : _ t -> packed

  val string : string -> ('ocaml, string) Conv.t -> ('ocaml, string) t

  val blob : string -> ('ocaml, string) Conv.t -> ('ocaml, string) t

  val int : string -> ('ocaml, int) Conv.t -> ('ocaml, int) t

  val int64 : string -> ('ocaml, int64) Conv.t -> ('ocaml, int64) t

  val float : string -> ('ocaml, float) Conv.t -> ('ocaml, float) t

  val datetime : string -> ('ocaml, Datetime.t) Conv.t -> ('ocaml, Datetime.t) t

  val date : string -> ('ocaml, Date.t) Conv.t -> ('ocaml, Date.t) t

  val time : string -> ('ocaml, Time.t) Conv.t -> ('ocaml, Time.t) t

  val name : _ t -> string
  (** [name column] returns the name of the given column *)

  (** {2 Column specifications}

      Column specifications to use when defining tables. The [make_*] functions
      below each take a column name, a {!Conv.t} defining how to translate
      values between OCaml and SQL and any other parameters which may be
      relevant to that column's basic definition. *)

  type ('ocaml, 'sql) spec
  (** Column specification (ex. name, width, nullable, auto_increment) *)

  (** [Pack spec] can be used to pack differing column {!spec} values into a
      single data structure. It is necessary when defining a table with the
      {!Table} module, for example. *)
  type packed_spec = Pack : _ spec -> packed_spec

  val make_char :
    string ->
    int ->
    ?nullable:bool ->
    ('ocaml, string) Conv.t ->
    ('ocaml, string) spec

  val make_varchar :
    string ->
    int ->
    ?nullable:bool ->
    ('ocaml, string) Conv.t ->
    ('ocaml, string) spec

  val make_binary :
    string ->
    int ->
    ?nullable:bool ->
    ('ocaml, string) Conv.t ->
    ('ocaml, string) spec

  val make_blob :
    string -> ?nullable:bool -> ('ocaml, string) Conv.t -> ('ocaml, string) spec

  val make_tiny_int :
    string ->
    ?nullable:bool ->
    ?auto_increment:bool ->
    ('ocaml, int) Conv.t ->
    ('ocaml, int) spec

  val make_small_int :
    string ->
    ?nullable:bool ->
    ?auto_increment:bool ->
    ('ocaml, int) Conv.t ->
    ('ocaml, int) spec

  val make_medium_int :
    string ->
    ?nullable:bool ->
    ?auto_increment:bool ->
    ('ocaml, int) Conv.t ->
    ('ocaml, int) spec

  val make_int :
    string ->
    ?nullable:bool ->
    ?auto_increment:bool ->
    ('ocaml, int) Conv.t ->
    ('ocaml, int) spec

  val make_big_int :
    string ->
    ?nullable:bool ->
    ?auto_increment:bool ->
    ('ocaml, int64) Conv.t ->
    ('ocaml, int64) spec

  val make_float :
    string -> ?nullable:bool -> ('ocaml, float) Conv.t -> ('ocaml, float) spec

  val make_datetime :
    string ->
    ?nullable:bool ->
    ('ocaml, Datetime.t) Conv.t ->
    ('ocaml, Datetime.t) spec

  val make_date :
    string -> ?nullable:bool -> ('ocaml, Date.t) Conv.t -> ('ocaml, Date.t) spec

  val make_time :
    string -> ?nullable:bool -> ('ocaml, Time.t) Conv.t -> ('ocaml, Time.t) spec

  val of_spec : ('ocaml, 'sql) spec -> ('ocaml, 'sql) t

  val of_packed_spec : packed_spec -> packed

  val with_name : string -> ('ocaml, 'sql) spec -> ('ocaml, 'sql) spec
  (** [with_name name spec] returns a new column {!spec} matching [spec] except
      its name is [name]. This can be useful when creating a column intended to
      reference another as a foreign key constraint. For example, if there is a
      table module [Base_table] with an column [Base_table.id] named ["id"] then
      a column matching the type and other specs of [Base_table.id] but with the
      name ["my_id"] can be created using [with_name "my_id" My_id.id].

      {b NOTE:} [with_name] will always produce a field with [auto_increment]
      set to [false]. *)
end

module Field : sig
  (** Fields (elements of a row) *)
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
end

module Table : sig
  type t
  (** A table definition *)

  type foreign_key
  (** A foreign key definition *)

  (** Action to take when a foreign key constraint take effect *)
  type foreign_key_action =
    | Restrict
    | Cascade
    | Set_null
    | No_action

  (** Fields of an index *)
  type index_field =
    | Column : (_, _) Column.spec -> index_field  (** Index for this column *)
    | Prefix : {
        column : (_, _) Column.spec;
        width : int;
      }
        -> index_field  (** Prefix index with the specified [width] *)

  (** A column mapping for foreign key constraints *)
  type key_mapping =
    | Key : {
        local : ('ocaml, 'sql) Column.spec;
        remote : ('ocaml, 'sql) Column.spec;
      }
        -> key_mapping

  val make_foreign_key :
    ?key_name:string ->
    t ->
    key_mapping list ->
    on_update:foreign_key_action ->
    on_delete:foreign_key_action ->
    foreign_key
  (** [make_foreign_key ?name foreign_table column_pairs ~on_update ~on_delete]
      creates a foreign key with [on_update] and [on_delete] actions associated.

      @raise Invalid_argument if any of the remote columns listed in
      [column_pairs] do no exist in [foreign_table] or if [column_pairs] is
      empty.
      @param foreign_table is the external table referenced by the key.
      @param pairs is a list of [(local, foreign)] column mappings defining the
      constraint. *)

  val make :
    ?primary_key:Column.packed_spec list ->
    ?indices:(string * index_field list) list ->
    ?unique_keys:(string * Column.packed_spec list) list ->
    ?foreign_keys:foreign_key list ->
    string ->
    Column.packed_spec list ->
    t
  (** [make ?primary_key ?indices ?unique_keys ?foreign_keys name columns] makes
      a table definition for a table named [name] with the given [columns].

      @raise Invalid_argument if [columns] isn't a superset of all other listed
      columns.
      @param primary_key defaults to no primary key
      @param indices defaults to no index creation
      @param unique_keys defaults to no unique keys
      @param foreign_keys defaults to no foreign keys *)

  val create :
    Mysql.dbd -> t -> ok_if_exists:bool -> (unit, [> `Msg of string ]) result
  (** [create dbd table ~ok_if_exists] creates a table matching the definition
      in [table] in the context of the connection [dbd].

      @param ok_if_exists will add ["if not exists"] to the generated SQL *)

  val create_exn : Mysql.dbd -> t -> ok_if_exists:bool -> unit
  (** Like {!create} except it raises in case of failure.

      @raise Mysql.Error if there was a problem during table creation. *)

  val name : t -> string
  (** [name t] return the name of the table defined by [t]. *)

  val deps : t -> t list
  (** [deps t] returns the list of tables [t] depends on via foreign keys. *)

  val pp_sql : Format.formatter -> ok_if_exists:bool -> t -> unit
  (** SQL pretty-printer for table definitions

      @param ok_if_exists will add ["if not exists"] to the generated SQL *)

  (** {2 Topological ordering of table dependencies}

      Calculate the {b transitive dependencies} of one or more tables according
      to their foreign key definitions. *)

  val transitive_sorted_deps : t list -> t list
  (** [transitive_sorted_deps tables] returns a topological ordering of the
      tables in [tables] along with their transitive dependencies according to
      those tables' foreign key definitions. If table [A] depends on table [B]
      and [B] depends on table [C] then the sorted deps for [A] would be
      [C; B; A] since [C] must be created first, then [B], then finally [A] in
      order for key constraints to apply properly. *)
end

type 'kind sql = private string constraint 'kind = [< `Run | `Get ]
(** SQL you can [`Run] for side-effects or [`Get] values from. *)

type row = Field.packed option String_map.t
(** A row is a map from strings (column names) to fields, where [None] signifies
    a [NULL] field *)

val row_of_list : (string * Field.packed option) list -> row
(** [row_of_list values] creates a row from the [(column_name, field)] elements
    in [values]. *)

val pack_column :
  ('ocaml, 'sql) Column.spec -> 'ocaml -> string * Field.packed option
(** [pack_column spec elt] returns [(column_name, field)] match the column
    [spec] and the value in [elt] ready for use in {!row_of_list}. *)

val pack_column_opt :
  ('ocaml, 'sql) Column.spec -> 'ocaml option -> string * Field.packed option
(** [pack_column_opt spec elt] returns [(column_name, field)] match the column
    [spec] and the value in [elt] ready for use in {!row_of_list}. If [elt] is
    [None] then the column will be assigned a [NULL] value. *)

val find_column :
  ('ocaml, 'sql) Column.spec ->
  row ->
  ('ocaml option, [> `Msg of string ]) result
(** [find_column spec row] will find the value matching [spec] in [row] if it
    exists. *)

val get_column : ('ocaml, 'sql) Column.spec -> row -> 'ocaml
(** [get_column spec row] will find the value matching [spec] in [row] if it
    exists.

    @raise Not_found if the value associated with [spec] is [NULL].
    @raise Invalid_argument if there is no column matching [spec] in [row]. *)

val make_run : ('a, Format.formatter, unit, [ `Run ] sql) format4 -> 'a
(** [make_run fmt ...] returns a {!sql} value for running for side-effects.

    {b NOTE}: The function is like {!Fmt.strf} in the quotations it takes. You
    can use the {!Pp} module to properly quote arguments.

    {b NOTE}: No checks are made to ensure the SQL does not return rows until
    the query is run. *)

val make_get : ('a, Format.formatter, unit, [ `Get ] sql) format4 -> 'a
(** [make_run fmt ...] returns a {!sql} value for running for results.

    {b NOTE}: The function is like {!Fmt.strf} in the quotations it takes. You
    can use the {!Pp} module to properly quote arguments.

    {b NOTE}: No checks are made to ensure the SQL does returns rows until the
    query is run. *)

val insert' :
  Mysql.dbd ->
  into:string ->
  row ->
  ('a, Format.formatter, unit, [ `Run ] sql) format4 ->
  'a
(** [insert' dbd ~into:table row extra_clauses] creates an insert statement
    inserting [row] into [table]. If a key in [row] has [None] as its associated
    value then a [NULL] value will be inserted for that column.

    @param extra_clauses can be used to provide suffix clauses for the
    statement. It is intended for cases which would not be covered by
    {!insert}'s more basic ["on duplicate key update"] support, for example. *)

val insert :
  ?on_duplicate_key_update:
    [ `All
    | `Columns of string list
    | `Except of string list
    | `With_id of string * string list
    ] ->
  Mysql.dbd ->
  into:string ->
  row ->
  [ `Run ] sql
(** [insert ?on_duplicate_key_update dbd ~into:table row] creates an insert
    statement inserting the values in [row] into [table]. If a key in [row] has
    [None] as its associated value then a [NULL] value will be inserted for that
    column.

    @param on_duplicate_key_update will have no effect if it is not specified or
    explicitly passed in as [?on_duplicate_key_update:None]. [`All] will replace
    all columns in the existing table's row with the values in [row] if there is
    a duplicate key found on insert. [`Columns column_names] will only replace
    the columns listed in [column_names] in the case when a duplicate key is
    found during insert. [`Except column_names] will replace all columns in the
    row {b except} those listed in [column_names].
    [`With_id (insert_id_column_name, other_column_names)] will only replace
    [other_column_names] in the row when a duplicate key is found during insert.
    The difference between [`With_id] and [`Columns] is that the
    [insert_id_column_name] value will be reported by {!Mysql.insert_id} or a
    SQL query against [last_insert_id()]. *)

val replace : [ `Use_insert_on_duplicate_key_update ]
  [@@ocaml.deprecated "Use 'insert ~on_duplicate_key_update' instead"]

val update : string -> ('a, Format.formatter, unit, [ `Run ] sql) format4 -> 'a
(** [update table fmt] creates an update statement against [table].

    {b NOTE}: The function is like {!Fmt.strf} in the quotations it takes. You
    can use the {!Pp} module to properly quote arguments. *)

val delete :
  from:string -> ('a, Format.formatter, unit, [ `Run ] sql) format4 -> 'a
(** [delete ~from:table fmt] creates an delete statement against [table].

    {b NOTE}: The function is like {!Fmt.strf} in the quotations it takes. You
    can use the {!Pp} module to properly quote arguments. *)

val select :
  string list ->
  from:string ->
  ('a, Format.formatter, unit, [ `Get ] sql) format4 ->
  'a
(** [select columns ~from:table fmt] creates an select statement querying for
    [columns] against [table].

    {b NOTE}: The function is like {!Fmt.strf} in the quotations it takes. You
    can use the {!Pp} module to properly quote arguments. *)

val run : Mysql.dbd -> [ `Run ] sql -> (unit, [> `Msg of string ]) result
(** [run dbd sql] runs [sql] against the connection [dbd] strictly for
    side-effects.

    @return Error if there is a problem with [dbd] or if any rows are returned. *)

val run_exn : Mysql.dbd -> [ `Run ] sql -> unit
(** Like {!run} except it raises an exception in the case of failure.

    @raise Failure if the server returns rows.
    @raise Mysql.Error if there was problem during execution of [sql]. *)

val get : Mysql.dbd -> [ `Get ] sql -> (row list, [> `Msg of string ]) result
(** [run dbd sql] runs [sql] against the connection [dbd] in order to retrieve
    rows from the database. The results is an iterator over the returned rows,
    compatible with the combinators available in the {!Gen} module.

    @return Error if there is a problem with [dbd] or if no rows are returned.
    An empty set of rows is OK. *)

val get_exn : Mysql.dbd -> [ `Get ] sql -> row list
(** List {!get} except it raises an exception in the case of failure.

    @raise Failure if no rows are returned. An empty set of rows is OK.
    @raise Mysql.Error if there was a problem during the execution of [sql]. *)

val to_column : row list -> ('a, _) Column.t -> 'a option list

val to_column2 :
  row list ->
  ('a, _) Column.t * ('b, _) Column.t ->
  ('a option * 'b option) list

val to_column3 :
  row list ->
  ('a, _) Column.t * ('b, _) Column.t * ('c, _) Column.t ->
  ('a option * 'b option * 'c option) list

val to_column4 :
  row list ->
  ('a, _) Column.t * ('b, _) Column.t * ('c, _) Column.t * ('d, _) Column.t ->
  ('a option * 'b option * 'c option * 'd option) list

val to_column5 :
  row list ->
  ('a, _) Column.t
  * ('b, _) Column.t
  * ('c, _) Column.t
  * ('d, _) Column.t
  * ('e, _) Column.t ->
  ('a option * 'b option * 'c option * 'd option * 'e option) list
(** [to_column rows col] and its [2,3,4,5] variants extract specific columns
    from [rows], returning new iterators containing only those columns. *)

(** {1 Transactions} *)

val start_transaction : Mysql.dbd -> (unit, [> `Msg of string ]) result
(** Start a new transaction. *)

val start_transaction_exn : Mysql.dbd -> unit
(** Like {!start_transaction} but raises in case of failure.

    @raise Mysql.Error if the process fails. *)

val commit : Mysql.dbd -> (unit, [> `Msg of string ]) result
(** Commit the current transaction. *)

val commit_exn : Mysql.dbd -> unit
(** Like {!commit} but raises in case of failure.

    @raise Mysql.Error if the process fails. *)

val rollback : Mysql.dbd -> (unit, [> `Msg of string ]) result
(** Roll back the current transaction. *)

val rollback_exn : Mysql.dbd -> unit
(** Like {!rollback} but raises in case of failure.

    @raise Mysql.Error if the process fails. *)

val with_transaction :
  Mysql.dbd ->
  (unit -> ('a, ([> `Msg of string ] as 'b)) result) ->
  ('a, 'b) result
(** [with_transaction dbd f] will run [f ()] in the context of a new
    transaction. If [f ()] returns [Ok _] the transaction will be committed. If
    [f ()] returns [Error _] the transaction will be rolled back. If [f ()]
    raises an exception the transaction will be rolled back then the exception
    will be re-raised. *)

val with_transaction_exn : Mysql.dbd -> (unit -> 'a) -> 'a
(** [with_transaction_exn dbd f] will run [f ()] in the context of a new
    transaction. If [f ()] completes without raising an exception the
    transaction will be committed. If [f ()] raises an exception the transaction
    will be rolled back then the exception will be re-raised.

    @raise Mysql.Error if there is a database error at any point during the
    transaction. *)

module Prepared : sig
  type 'a t = {
    dbd : Mysql.dbd;
    sql : string;
    mutable statement : Mysql.Prepared.stmt;
  }
    constraint 'a = [< `Run | `Get ]
  (** A prepared statement, its associated database connection and the original
      text used to create it. *)

  val make_run : Mysql.dbd -> string -> ([ `Run ] t, [> `Msg of string ]) result
  (** [make_run dbd sql] creates a new {!t} using the given database connection
      and SQL ([sql]), intended to be run for its side-effects only.

      {b NOTE}: No checks are made to ensure the SQL does not return rows until
      the query is run. *)

  val make_get : Mysql.dbd -> string -> ([ `Get ] t, [> `Msg of string ]) result
  (** [make_run dbd sql] creates a new {!t} using the given database connection
      and SQL ([sql]), intended to be run for its returned rows.

      {b NOTE}: No checks are made to ensure the SQL does returns rows until the
      query is run. *)

  val run :
    [ `Run ] t -> Field.packed option list -> (unit, [> `Msg of string ]) result
  (** [run stmt params] runs [stmt] against the connection [dbd] using [params]
      to fill in the query parameters for [stmt].

      @return Error if there is a problem with [stmt] or if any rows are
      returned. *)

  val get :
    [ `Get ] t ->
    Field.packed option list ->
    (row list, [> `Msg of string ]) result
  (** [get stmt params] runs [stmt] against the connection [dbd] using [params]
      to fill in the query parameters for [stmt].

      @return Error if there is a problem with [stmt] or if no rows are
      returned. *)

  val remake : 'a t -> (unit, [> `Msg of string ]) result
  (** [remake stmt] will ping the database connection associated with [stmt] and
      recreate the prepared statement.

      @return Error if the database connection can not be re-established *)

  val close : 'a t -> unit
  (** [close stmt] closes the prepared statement associated with [stmt] but {b
      NOT} the underlying database connection. *)
end

module type S = sig
  type t
  (** A type which can be inserted into or selected from a single row in a table *)

  val table : Table.t
  (** A table definition which can hold serialized values of type {!t}. *)

  val to_row : t -> row
  (** [to_row t] serialized [t] into a row suitable for {!table} *)

  val of_row : row -> (t, [> `Msg of string ]) result
  (** [of_row row] returns a value deserialized from a row selected from
      {!table}. *)
end

module type Db = sig
  type t
  (** A type which can be inserted into or selected from a single row in a table *)

  val table : Table.t
  (** The table definition used in this module *)

  val init :
    Mysql.dbd -> ok_if_exists:bool -> (unit, [> `Msg of string ]) result
  (** [init dbd ~ok_if_exists] creates the table associated with this module.
      See {!Table.create} for the meaning of the [ok_if_exists] argument. *)

  val init_exn : Mysql.dbd -> ok_if_exists:bool -> unit
  (** Like {!init} except it raises in case of failure.

      @raise Mysql.Error if the operation fails. *)

  val insert' :
    Mysql.dbd ->
    t ->
    ('a, Format.formatter, unit, (unit, [> `Msg of string ]) result) format4 ->
    'a
  (** [insert' dbd x fmt] inserts [x] into the table associated with this
      module. See {!insert'} for an explanation of the [fmt] parameter. *)

  val insert_exn' :
    Mysql.dbd -> t -> ('a, Format.formatter, unit, unit) format4 -> 'a
  (** Like {!insert'} except it raises in case of failure.

      @raise Mysql.Error if the operation fails. *)

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
  (** Like {!insert} but only generates and returns the SQL for the query
      without running it.

      The database handle argument is used to properly escape the given {!t}
      value. {b NOTE:} This escaping may not be valid when connecting to
      different databases or potentially even across different connections to
      the same database! Use with care. *)

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
  (** [insert ?on_duplicate_key_update dbd x] inserts [x] into the table
      associated with this module. See {!insert} for an explanation of the
      [on_duplicate_key_update] parameter. *)

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
  (** Like {!insert} except it raises in case of failure.

      @raise Mysql.Error if the operation fails. *)

  val replace : [ `Use_insert_on_duplicate_key_update ]
    [@@ocaml.deprecated "Use 'insert ~on_duplicate_key_update' instead"]

  val update_sql : ('a, Format.formatter, unit, [ `Run ] sql) format4 -> 'a
  (** Like {!update} but only generates and returns the SQL for the query
      without running it. *)

  val update :
    Mysql.dbd ->
    ('a, Format.formatter, unit, (unit, [> `Msg of string ]) result) format4 ->
    'a
  (** [update dbd fmt] updates rows in the current table.

      {b NOTE}: The function is like {!Fmt.strf} in the quotations it takes. You
      can use the {!Pp} module to properly quote arguments. *)

  val update_exn : Mysql.dbd -> ('a, Format.formatter, unit, unit) format4 -> 'a
  (** Like {!update} except it raises in case of failure.

      @raise Mysql.Error if the operation fails. *)

  val select_sql : ('a, Format.formatter, unit, [ `Get ] sql) format4 -> 'a
  (** Like {!select} but only generates and returns the SQL for the query
      without running it. *)

  val select :
    Mysql.dbd ->
    ('a, Format.formatter, unit, (t list, [> `Msg of string ]) result) format4 ->
    'a
  (** [select dbd fmt_string fmt_args] selects values from the table associated
      with this module which match the given clauses in [fmt_string]. *)

  val select_exn :
    Mysql.dbd -> ('a, Format.formatter, unit, t list) format4 -> 'a
  (** Like {!select} except it raises in case of failure.

      @raise Failure if a row does not match the expected format.
      @raise Mysql.Error if the operation fails. *)

  val delete_sql : ('a, Format.formatter, unit, [ `Run ] sql) format4 -> 'a
  (** Like {!delete} but only generates and returns the SQL for the query
      without running it. *)

  val delete :
    Mysql.dbd ->
    ('a, Format.formatter, unit, (unit, [> `Msg of string ]) result) format4 ->
    'a
  (** [delete dbd fmt_string fmt_args] deletes rows from the table associated
      with this module which match the given clauses in [fmt_string]. *)

  val delete_exn : Mysql.dbd -> ('a, Format.formatter, unit, unit) format4 -> 'a
  (** Like {!delete} except it raises in case of failure.

      @raise Mysql.Error if the operation fails. *)
end

module Clause : sig
  (** {1 DSL for condition clauses in SQL} *)

  type 'ocaml column =
    | Column : ('ocaml, 'sql) Column.t -> 'ocaml column
    | Spec : ('ocaml, 'sql) Column.spec -> 'ocaml column

  type t

  val ( = ) : 'ocaml column -> 'ocaml -> t

  val ( <> ) : 'ocaml column -> 'ocaml -> t

  val ( > ) : 'ocaml column -> 'ocaml -> t

  val ( < ) : 'ocaml column -> 'ocaml -> t

  val ( >= ) : 'ocaml column -> 'ocaml -> t

  val ( <= ) : 'ocaml column -> 'ocaml -> t

  val ( && ) : t -> t -> t

  val ( || ) : t -> t -> t
end

module Pp : sig
  (** {1 Pretty-printers for use in SQL queries} *)

  (** {2 Simple value printers} *)

  val null : unit Fmt.t
  (** [NULL] *)

  val nullable : 'a Fmt.t -> 'a option Fmt.t
  (** [nullable pp] can be used to wrap another printer to properly handle
      optional values. For example, [nullable string] will render like [string]
      if the value to be printed is [Some _]. If the value is [None] then it
      will render as {!null}. *)

  val string : Mysql.dbd -> string Fmt.t

  val blob : Mysql.dbd -> string Fmt.t

  val int : int Fmt.t

  val int64 : int64 Fmt.t

  val float : float Fmt.t

  val datetime : Datetime.t Fmt.t

  val date : Date.field Date.date Fmt.t

  val time : Time.t Fmt.t
  (** Pretty-printers for MySQL values with proper quoting *)

  val csv_simple : 'a Fmt.t -> 'a list Fmt.t
  (** CSV-printing of values, wrapped in [()] *)

  (** {2 Column printers} *)

  val column : Mysql.dbd -> ('ocaml, 'sql) Column.t -> 'ocaml Fmt.t
  (** [column dbd c] creates a pretty-printer for values appropriate for the
      column [c]. *)

  val column_name : _ Column.t Fmt.t
  (** Printer for column names *)

  val spec : Mysql.dbd -> ('ocaml, 'sql) Column.spec -> 'ocaml Fmt.t
  (** [spec dbd s] creates a pretty-printer for values appropriate for the
      column [s]. *)

  val spec_name : _ Column.spec Fmt.t
  (** Printer for column names *)

  (** {2 Formatting clauses}

      These printers are slightly more complex and meant to be used with the DSL
      in in {!Clause} module. *)

  val clause : Mysql.dbd -> Clause.t Fmt.t
  (** Printer for clauses defined with the {!Clause} module's functions. *)

  (** {2 Field printers} *)

  val field : Mysql.dbd -> _ Field.t Fmt.t
  (** Pretty-printer for field values *)

  val field_opt : Mysql.dbd -> _ Field.t option Fmt.t
  (** Pretty-printer for field values. Formats [None] as a [NULL] value. *)

  (** {2 Table printers} *)

  val table_name : Table.t Fmt.t
  (** Pretty-printer for table names *)
end

(** Given a module of type {!S}, generate a module with the functions specified
    by {!Db} *)
module Make (M : S) : Db with type t := M.t
