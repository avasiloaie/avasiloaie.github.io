grammar cpd;

compileUnit: stmt+ EOF ;

stmt
  : for_stmt
  | do_stmt
  | while_stmt
  | if_stmt
  | no_op_expr '=' expr ';'
  ;

for_stmt: 'for' '(' op_expr ';' op_expr ';' op_expr ')' '{' stmt+ '}' ;

do_stmt: 'do' '(' Identifier '=' expr ',' expr ')' '{' stmt+ '}' ;

while_stmt: 'while' '(' full_expr ')' '{' stmt+ '}' ;

if_stmt: if_part else_if_part* else_part? ;

if_part: 'if' '(' full_expr ')' '{' stmt+ '}' ;

else_if_part: 'else' if_part ;

else_part: 'else' '{' stmt+ '}' ;




full_expr
  : no_op_expr
  | op_expr
  ;

op_expr
  : Identifier '++'
  | '++' Identifier
  | Identifier '--'
  | '--' Identifier
  | expr
  | min_expr
  ;

no_op_expr
  : Identifier
  | IntLiteral
  | arrayaccess
  ;

arrayaccess: Identifier ( '[' full_expr ']' )+ ;

min_expr
  : '-' no_op_expr
  | no_op_expr
  ;



expr: or_expr ;
or_expr: and_expr ( '||' and_expr )* ;
and_expr: ineq_expr ( '&&' ineq_expr )* ;
ineq_expr: addsub_expr ( ( '<=' | '<' | '>=' | '>' | '!=' | '==' ) addsub_expr )* ;
addsub_expr: multdiv_expr ( ( '+' | '-' ) multdiv_expr )* ;
multdiv_expr: final_expr ( ( '*' | '/' | '%' ) no_min_final_expr )* ;

final_expr
  : '(' expr ')'
  | no_op_expr
  ;

no_min_final_expr
  : '(' expr ')'
  | min_expr
  ;



  Do : 'do';
  Else : 'else';
  For : 'for';
  If : 'if';
  While : 'while';

  LeftParen : '(';
  RightParen : ')';
  LeftBracket : '[';
  RightBracket : ']';
  LeftBrace : '{';
  RightBrace : '}';

  Less : '<';
  LessEqual : '<=';
  Greater : '>';
  GreaterEqual : '>=';

  Plus : '+';
  PlusPlus : '++';
  Minus : '-';
  MinusMinus : '--';
  Star : '*';
  Div : '/';
  Mod : '%';

  And : '&';
  Or : '|';
  AndAnd : '&&';
  OrOr : '||';

  Colon : ':';
  Semi : ';';
  Comma : ',';

  Assign : '=';
  StarAssign : '*=';
  DivAssign : '/=';
  ModAssign : '%=';
  PlusAssign : '+=';
  MinusAssign : '-=';

  Equal : '==';
  NotEqual : '!=';

  Identifier
      :   IdentifierNondigit
          (   IdentifierNondigit
          |   Digit
          )*
      ;

  fragment
  IdentifierNondigit
      :   Nondigit
      ;

  fragment
  Nondigit
      :   [a-zA-Z_]
      ;

  IntLiteral
      :   NonzeroDigit Digit*
      ;

  fragment
  Digit
      :   [0-9]
      ;

  fragment
  NonzeroDigit
      :   [1-9]
      ;

  Whitespace
      :   [ \t]+
          -> skip
      ;

  Newline
      :   (   '\r' '\n'?
          |   '\n'
          )
          -> skip
      ;

  BlockComment
      :   '/*' .*? '*/'
          -> skip
      ;

  LineComment
      :   '//' ~[\r\n]*
          -> skip
      ;
