: POSSIBLY  ( "name" -- )  BL WORD FIND  ?dup AND IF  EXECUTE  THEN ;
: anew  ( "name" -- )( Run: -- )  >in @ possibly  >in ! marker ;