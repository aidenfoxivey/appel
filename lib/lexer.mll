{
open Lexing
open Parser

exception SyntaxError of string

(* borrowed, understand it later and rewrite *)
let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }
}

let digit = [ '0'-'9' ]
let int = digit digit*
let whitespace = [ ' ' '\t' ]+
let newline = '\n' | '\r' | "\r\n"
let id = [ 'a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

(* This is listed as follows from page 32. *)
rule read = parse
| "while" { WHILE }
| "for" { FOR }
| "to" { TO }
| "break" { BREAK }
| "let" { LET }
| "in" { IN }
| "end" { END }
| "function" { FUNCTION }
| "var" { VAR }
| "type" { TYPE }
| "array" { ARRAY }
| "if" { IF }
| "then" { THEN }
| "else" { ELSE }
| "do" { DO }
| "of" { OF }
| "nil" { NIL }
| "," { COMMA }
| ":" { COLON }
| ";" { SEMICOLON }
| "(" { LPAREN }
| ")" { RPAREN }
| "[" { LBRACKET }
| "]" { RBRACKET }
| "{" { LBRACE }
| "}" { RBRACE }
| "." { DOT }
| "+" { PLUS }
| "-" { MINUS }
| "*" { ASTERISK }
| "/" { SLASH }
| "=" { EQUAL }
| "<>" { NEQUAL }
| "<" { LESS }
| "<=" { LESSEQUAL }
| ">" { GREATER }
| ">=" { GREATEREQUAL }
| "&" { AND }
| "|" { OR }
| ":=" { ASSIGN }
(* Only seen 1 begin comment statement so far *)
| "/*" { eat_comment 1 lexbuf }
(* Tentatively assume strings are of size 16. *)
| "\"" { read_string (Buffer.create 16) lexbuf }
| whitespace { read lexbuf }
| newline { next_line lexbuf; read lexbuf }
| int { INT (int_of_string (Lexing.lexeme lexbuf))}
| id { ID (Lexing.lexeme lexbuf) }
| _ { 
    let c = Lexing.lexeme_char lexbuf 0 in
    raise (SyntaxError (Printf.sprintf "Unexpected character: %c" c))
  }
| eof { EOF }

and eat_comment depth = parse
| "/*" { eat_comment (depth + 1) lexbuf }
| "*/" { if depth = 1 then read lexbuf else eat_comment (depth - 1) lexbuf }
| eof { raise (SyntaxError "Unterminated comment")}
| _ { eat_comment depth lexbuf }

and read_string buf = parse
| '"' { STRING (Buffer.contents buf) }
| '\\' 'n' { Buffer.add_char buf '\n'; read_string buf lexbuf }
| '\\' 't' { Buffer.add_char buf '\t'; read_string buf lexbuf }
| '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
| '\\' '"' { Buffer.add_char buf '"'; read_string buf lexbuf }
| eof { raise (SyntaxError "Unterminated string literal") }
| _ { Buffer.add_char buf (Lexing.lexeme_char lexbuf 0); read_string buf lexbuf}