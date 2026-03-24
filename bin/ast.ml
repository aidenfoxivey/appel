type bop =
  | Add
  | Sub
  | Mult
  | Div
  | Gt
  | Lt
  | Ge
  | Le
  | Eq
  | Neq
  [@@deriving show]

type ty =
  | TyId of string
  (* For example, `{x: int, y: string}` *)
  | TyRecord of tyfield list
  (* E.G. `array of int` *)
  | TyArray of ty

  and tyfield = {
    field_name : string;
    field_ty: ty;
  }
  [@@deriving show]

type expr =
  | String of string
  | Int of int
  | Nil
  | LValue of lvalue
  | UnaryMinus of expr
  | BinOp of expr * bop * expr
  | Assign of lvalue * expr
  | Call of string * expr list
  | Seq of expr list
  | ArrayCreate of string * expr * expr
  | IfThen of expr * expr
  | IfThenElse of expr * expr * expr
  | While of expr * expr
  | For of string * expr * expr * expr
  | Break
  | RecordCreate of string * (string * expr) list
  | Let of dec list * expr list

  and lvalue =
  | LId of string
  (* E.G. `v3.x` - selects field x from record v3 *)
  | LField of lvalue * string
  (* E.G. `v3[2]` - selects element at index 2 from array v3 *)
  | LIndex of lvalue * expr

  and dec =
    (* type type-id = ty *)
    | TyDec of string * ty
    | VarDec of vardec
    (* function func-id (param1: ty1, param2: ty2, ...) : ty = body *)
    | FunDec of string * tyfield list * ty option * expr

  and vardec = {
    var_name : string;
    var_ty : ty option;
    var_exp: expr;
  }
  [@@deriving show]
