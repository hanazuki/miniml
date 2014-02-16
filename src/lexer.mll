{
  let reservedWords = [
    "and", Parser.AND;
    "as", Parser.AS;
    "assert", Parser.ASSERT;
    "bool", Parser.BOOL;
    "else", Parser.ELSE;
    "false", Parser.FALSE;
    "fun", Parser.FUN;
    "if", Parser.IF;
    "in", Parser.IN;
    "int", Parser.INT;
    "let", Parser.LET;
    "match", Parser.MATCH;
    "rec", Parser.REC;
    "then", Parser.THEN;
    "true", Parser.TRUE;
    "unit", Parser.UNIT;
    "when", Parser.WHEN;
    "with", Parser.WITH;
  ]
}

rule main = parse
  (* ignore spacing and newline characters *)
  | [' ' '\009' '\012' '\n']+
      { main lexbuf }

  | "-"? ['0'-'9']+
      { Parser.INTV (int_of_string (Lexing.lexeme lexbuf)) }

  | "[" { Parser.LBRACKET }
  | "]" { Parser.RBRACKET }
  | "(" { Parser.LPAREN }
  | ")" { Parser.RPAREN }
  | ":" { Parser.COLON }
  | "::" { Parser.CONS }
  | ";;" { Parser.SEMISEMI }
  | "," { Parser.COMMA }

  | "&&" { Parser.AMPAMP }
  | "||" { Parser.BARBAR }
  | "+" { Parser.PLUS }
  | "*" { Parser.MULT }
  | "<" { Parser.LESS }
  | "=" { Parser.EQUALS }
  | "->" { Parser.RARROW }
  | ";" { Parser.SEMI }
  | "|" { Parser.BAR }
  | "_" { Parser.WILD }

  | "'" ['a'-'z'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']*
      {
        let lexeme = (Lexing.lexeme lexbuf) in
        Parser.TVID (String.sub lexeme 1 (String.length lexeme - 1))
      }
  | ['a'-'z'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']*
      {
        let id = Lexing.lexeme lexbuf in
        try 
          List.assoc id reservedWords
        with
          | Not_found -> Parser.ID id
      }
  | eof { Parser.EOF }

  | "(*" { comments 0 lexbuf }
and comments level = shortest
  | (_)* "(*" { comments (level + 1) lexbuf }
  | (_)* "*)" { if level = 0 then main lexbuf else comments (level - 1) lexbuf }
