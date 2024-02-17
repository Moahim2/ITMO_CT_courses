grammar PrefixLanguage;

@header {
    package levitskiy.hw;
}

/*
* Lexer Rules
*/

fragment DIGIT : [0-9]       ;
fragment LETTER : [a-zA-Z]   ;

NUMBER         : DIGIT+      ;

ASSIGNMENT     : '='         ;
PRINT          : 'print'     ;

IF             : 'if'        ;
FALSE          : 'FALSE'     ;
TRUE           : 'TRUE'      ;
LBR            : '<'         ;
RBR            : '>'         ;
EQ             : '=='        ;
NOT            : '!'         ;

PLUS           : '+'         ;
MINUS          : '-'         ;
MUL            : '*'         ;
DIV            : '/'         ;

WHILE          : 'while'     ;
DO             : 'do'        ;
LB             : '('         ;
RB             : ')'         ;

VAR_NAME       : LETTER(LETTER|DIGIT)*                 ;

WS             : (' '|'\n'|'\r'|'\t'|'\f') -> skip     ;


///////////////////

/*
 * Parser Rules
 */

program           : (operation)* EOF                                                           ;

operation         : valueExprToken | logicBranch | print | assignment | whileCycle             ;


unaryAr
    : MINUS
    ;
binaryAr
    : PLUS
    | MINUS
    | MUL
    | DIV
    ;
arithmetic
    : binaryAr arithmetic arithmetic
    | unaryAr arithmetic
    | NUMBER
    | VAR_NAME
    ;


specialLogKeyWord : FALSE | TRUE                                                               ;
logToken          : arithmetic | specialLogKeyWord                                             ;
unaryLog
    : NOT
    ;
binaryLog
    : LBR
    | RBR
    | EQ
    ;
logic
    : binaryLog logToken logToken
    | unaryLog logic
    | specialLogKeyWord
    | VAR_NAME
    ;
logicBranch       : IF logic operation                                                         ;

valueExprToken    : arithmetic | logic                                                         ;


print             : PRINT valueExprToken                                                       ;
assignment        : ASSIGNMENT VAR_NAME valueExprToken                                         ;
whileCycle        : WHILE logic LB DO (operation)* RB                                          ;
