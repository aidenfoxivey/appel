open Core
open Appel.Ast

let () =
  let lexed = In_channel.read_all "sample_programs/merge.tig" |> Lexing.from_string in
  try
  Appel.Parser.program Appel.Lexer.read lexed |> show_expr |> printf "%s\n"
  with
  | e ->
    let pos = lexed.Lexing.lex_curr_p in
    printf "Error at line %d, column %d: %s\n"
      pos.Lexing.pos_lnum
      (pos.Lexing.pos_cnum - pos.Lexing.pos_bol)
      (Exn.to_string e)
;;
