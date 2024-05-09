: POSSIBLY  ( "name" -- )  BL WORD FIND  ?dup AND IF  EXECUTE  THEN ;
: anew  ( "name" -- )( Run: -- )  >in @ possibly  >in ! marker ;

\ Forth 2012 compatible {: and :} using Gforth 0.7.3's { }, minus
\ Forth 2012 |, which Gforth 0.7.3 doesn't understand.
\ For {: a b | c -- :} use 0 {: a b c -- :} instead.
' { alias {: immediate
locals-types definitions
' } alias :}
forth definitions

variable stack-depth

: (* depth stack-depth ! ;
: *) depth stack-depth @ - ;
