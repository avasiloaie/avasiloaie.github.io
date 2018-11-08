grammar cpd;

compileUnit: stmt+ EOF ;

stmt
  : do_stmt EOL
  | if_stmt EOL
  | assign_stmt EOL
  ;


// DO

do_stmt
  : DO VARNAME (ASSIGN eq_expr COMMA eq_expr (COMMA eq_expr)?)? EOL stmt+ (ENDDO | (END DO))
  ;


// IF+ELSE

if_stmt
  : IF LPAR expr RPAR (block_if_stmt | assign_stmt)
  ;

block_if_stmt
  : first_if_block (else_if_stmt)* (else_stmt)? (ENDIF | (END IF))
  ;

first_if_block
  : THEN EOL stmt+
  ;

else_if_stmt
  : (ELSEIF | (ELSE IF)) LPAR expr RPAR THEN EOL stmt+
  ;

else_stmt
  : ELSE EOL stmt+
  ;


// ASSIGN

assign_stmt
  : array ASSIGN expr
  | VARNAME ASSIGN expr
  ;


// EXPRESSION

expr
  : or_expr (OR or_expr)*
  ;

or_expr
  : and_expr (AND and_expr)*
  ;

and_expr
  : eq_expr ((LT | LE | EQ | NE | GT | GE) eq_expr)?
  ;

eq_expr
  : add_expr ((PLUS | MINUS) add_expr)*
  ;

add_expr
  : mult_expr ((STAR | DIV) mult_expr)*
  ;

mult_expr
  : (PLUS | MINUS)* final_expr
  ;

final_expr
  : array
  | VARNAME
  | INT
  | LPAR eq_expr RPAR
  ;

array
  : VARNAME LPAR expr (COMMA expr)* RPAR
  ;



// LEXER

END
   : 'END' | 'end'
   ;

IF
   : 'IF' | 'if'
   ;


THEN
   : 'THEN' | 'then'
   ;


ELSE
   : 'ELSE' | 'else'
   ;


ENDIF
   : 'ENDIF' | 'endif'
   ;


ELSEIF
   : 'ELSEIF' | 'elseif'
   ;


DO
   : 'DO' | 'do'
   ;


ENDDO
   : 'ENDDO' | 'enddo'
   ;


COMMA
   : ','
   ;


LPAR
   : '('
   ;


RPAR
   : ')'
   ;


COLON
   : ':'
   ;


ASSIGN
   : '='
   ;


MINUS
   : '-'
   | '–'
   ;


PLUS
   : '+'
   ;


DIV
   : '/'
   ;


STAR
   : '*'
   ;


NOT
   : '.not.' | '.NOT.'
   ;


AND
   : '.and.' | '.AND.'
   ;


OR
   : '.or.' | '.OR.'
   ;


LT
   : '.lt.' | '.LT.'
   ;


LE
   : '.le.' | '.LE.'
   ;


GT
   : '.gt.' | '.GT.'
   ;


GE
   : '.ge.' | '.GE.'
   ;


NE
   : '.ne.' | '.NE.'
   ;


EQ
   : '.eq.' | '.EQ.'
   ;


TO
   : 'TO'
   ;


fragment ALNUM
   : (ALPHA | NUM0)
   ;

fragment ALPHA
   : ('a' .. 'z') | ('A' .. 'Z')
   ;

fragment NUM0
   : ('0' .. '9')
   ;

fragment NUM
   : ('1' .. '9')
   ;


VARNAME
   : ALPHA ALNUM*
   ;

INT
   : '0'
   | NUM NUM0*
   ;

EOL
   : [\r\n] +
   ;

WS
   : [\t ] + -> skip
   ;
