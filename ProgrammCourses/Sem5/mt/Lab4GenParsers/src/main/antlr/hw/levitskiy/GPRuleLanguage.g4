grammar GPRuleLanguage;

@header {
    package hw.levitskiy;
}

fragment DIGIT : [0-9]                   ;
fragment SMALL_LETTER : [a-z]            ;
fragment BIG_LETTER: [A-Z]               ;
fragment LETTER: SMALL_LETTER|BIG_LETTER ;


WS           : (' '|'\n'|'\r'|'\t'|'\f') -> skip      ;

TOKEN_NAME   : BIG_LETTER(BIG_LETTER|DIGIT|'\'')*     ;

DOWN         : '_'                                    ;
MARROW       : '->'                                   ;
DEF_RULE     : '!'                                    ;
DEF_RETURN   : '?'                                    ;
COL          : ':'                                    ;
BR_ATTR_RULE : '("'.+?'")'                            ;
EPS          : '#'                                    ;
TOKEN        : '"'.+?'"'                              ;
DEF_PACKAGE  : '_package'                             ;


sub_package     : DEF_PACKAGE MARROW TOKEN                                           ;
skip            : DOWN MARROW TOKEN                                                  ;
token_bijection : TOKEN_NAME MARROW TOKEN                                            ;
def_tr          : (TOKEN_NAME BR_ATTR_RULE?)                                         ;
rule_bijection  : DEF_RULE TOKEN_NAME MARROW (EPS|def_tr*) (DEF_RETURN TOKEN)?       ;
start_terminal  : DEF_RULE DEF_RULE TOKEN_NAME                                       ;
return_val      : DEF_RETURN TOKEN_NAME BR_ATTR_RULE? COL TOKEN                      ;

gr : ((sub_package|skip|token_bijection|rule_bijection|start_terminal|return_val))+  ;

