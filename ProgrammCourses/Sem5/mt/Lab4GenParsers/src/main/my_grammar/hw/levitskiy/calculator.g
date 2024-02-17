_package -> "calculator"

NUM   -> "^\\d+$"
ADD   -> "\\+"
SUB   -> "\\-"
MUL   -> "\\*"
DIV   -> "\\/"
LB    -> "\\("
RB    -> "\\)"
FTR   -> "\\!"


_    -> "[ \n\r\t\f]"

!! E

? E               : "Int"
? E'("acc : Int") : "Int"
? T               : "Int"
? T'("acc : Int") : "Int"
? F               : "Int"
? F'("acc : Int") : "Int"

! E  -> T E'("`T`")            ? "`E'`"
! E' -> ADD T E'("acc + `T`")  ? "`E'`"
! E' -> SUB T E'("acc - `T`")  ? "`E'`"
! E' -> #                      ? "acc"
! T  -> F T'("`F`")            ? "`T'`"
! T' -> MUL F T'("acc * `F`")  ? "`T'`"
! T' -> DIV F T'("acc / `F`")  ? "`T'`"
! T' -> #                      ? "acc"
! F  -> LB E RB F'("`E`")      ? "`F'`"
! F  -> NUM F'("`NUM`.toInt()") ? "`F'`"
! F' -> FTR F'("{ var result = 1; for (i in 2..acc) result *= i; result}.invoke()") ? "`F'`"
! F' -> #                      ? "acc"


