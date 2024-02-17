_package -> "lab2"


VAR  -> "^var$"
ARR  -> "^Array$"
COM  -> ","
COL  -> ":"
NAME -> "[a-zA-Z]+"
LBR  -> "<"
RBR  -> ">"
SCOL -> ";"


_    -> "[ \n\r\t\f]"

!! S

! S  -> VAR S'

! S' -> NAME COL ARR LBR E RBR SCOL

! S' -> ARR COL ARR LBR E RBR SCOL

! E  -> ARR LBR E RBR

! E  -> NAME E'

! E' -> #
! E' -> LBR T RBR
! T  -> E T'
! T' -> #
! T' -> COM E T'



