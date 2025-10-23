entero
0 + ['1'..'9']['0'..'9']* + (-['1'..'9']['0'..'9'])*
identificador
[['a'..'z'] ++ ['A'..'Z']]([['a'..'z'] ++ ['A'..'Z']]*)['0'..'9']*
operador
(\\+)+(-)+(\\*)
opbool
= + <= + not + and
asignacion
:=
delimitador
; + (\\() + (\\))
palabraReservada
true + false + skip + if + then + else + while + do