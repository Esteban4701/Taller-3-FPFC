import ManiobrasTrenes._
//Pruebas función aplicarMovimiento
val e1 = (List(1,2,3,4),Nil,Nil)
val e2 = aplicarMovimiento(e1,Uno(1))
val e3 = aplicarMovimiento(e2,Dos(1))
val e4 = aplicarMovimiento(e3,Uno(-3))
val e5 = aplicarMovimiento(e4,Dos(5))
val e6 = aplicarMovimiento(e5, Dos(-5))
val e7 = aplicarMovimiento(e6,Uno(0))

//Pruebas función aplicarMovimientos
aplicarMovimientos(e1, List(Uno(1),Dos(1),Uno(-3),Dos(5),Dos(-5),Uno(0)))
val e = (List(2,1),List(4),List(3))
aplicarMovimientos(e,List(Uno(1),Dos(1),Uno(-1),Dos(-2),Uno(-1)))

//Pruebas función definirManiobra y su comprobación con la función aplicarMovimientos
definirManiobra(List(1,2,3,4),List(3,2,4,1)) //Espero que al pasar t1 y los movs a aplicarMovimientos el último estado sea (List(3,2,4,1),Nil,Nil)
aplicarMovimientos((List(1,2,3,4),Nil,Nil),List(Uno(4), Uno(-2), Dos(2), Uno(-4), Dos(-2), Uno(3), Uno(-2), Dos(2), Uno(-3), Dos(-2)))

definirManiobra(List(2,3,5,6),List(6,2,3,5)) //Espero que al pasar t1 y los movs a aplicarMovimientos el último estado sea(List(6,2,3,5),Nil,Nil)
aplicarMovimientos((List(2,3,5,6),Nil,Nil),List(Uno(4), Uno(-3), Dos(3), Uno(-4), Dos(-3)))

definirManiobra(List(1,3,4,5,2),List(1,2,3,4,5))//Espero que al pasar t1 y los movs a aplicarMovimientos el último estado sea (List(1,2,3,4,5),Nil,Nil)
aplicarMovimientos((List(1,3,4,5,2),Nil,Nil),List(Uno(4), Uno(-3), Dos(3), Uno(-4), Dos(-3)))

definirManiobra(List(1,2),List(2,1)) //Espero que al pasar t1 y los movs a aplicarMovimientos el último estado sea (List(2,1),Nil,Nil)
aplicarMovimientos((List(1,2),Nil,Nil), List(Uno(2), Uno(-1), Dos(1), Uno(-2), Dos(-1)))

definirManiobra(List(1,"a",3),List("a",3,1)) //Espero que al pasar t1 y los movs a aplicarMovimientos el último estado sea (List("a",3,1),Nil,Nil)
aplicarMovimientos((List(1,"a",3),Nil,Nil),List(Uno(3), Uno(-1), Dos(1), Uno(-3), Dos(-1)))

//Casos IA
// ---------- aplicarMovimiento ----------
// 1. Pedir más vagones de los que hay en "principal": se trasladan todos
aplicarMovimiento((List('a','b','c'), Nil, Nil), Uno(10)) ==
  (Nil, List('a','b','c'), Nil)

// 2. Traer más vagones de los que hay en "uno": regresan todos
aplicarMovimiento((List('a'), List('b','c'), Nil), Uno(-10)) ==
  (List('a','b','c'), Nil, Nil)

// 3. Estado vacío con movimiento negativo grande: no pasa nada
aplicarMovimiento((Nil, Nil, Nil), Dos(-3)) == (Nil, Nil, Nil)

// ---------- aplicarMovimientos ----------
val e0 = (List('a','b'), Nil, Nil)

// 1. Sin movimientos: solo el estado inicial
aplicarMovimientos(e0, Nil) == List(e0)

// 2. Ida y vuelta exacta: regresa al estado inicial
aplicarMovimientos(e0, List(Uno(2), Uno(-2))) ==
  List(e0, (Nil, List('a','b'), Nil), e0)

// 3. Movimientos que se pasan del límite y un movimiento sobre un trayecto vacío
aplicarMovimientos(e0, List(Uno(5), Dos(-5), Uno(-5))) ==
  List(e0,
    (Nil, List('a','b'), Nil),
    (Nil, List('a','b'), Nil),
    (List('a','b'), Nil, Nil))

// ---------- definirManiobra ----------
def ok(t1: Tren, t2: Tren): Boolean =
  aplicarMovimientos((t1, Nil, Nil), definirManiobra(t1, t2)).last == (t2, Nil, Nil)

// 1. Trenes vacíos
definirManiobra(Nil, Nil) == Nil

// 2. Trenes iguales: no hay que mover nada
definirManiobra(List('a','b','c'), List('a','b','c')) == Nil

// 3. Tren largo invertido por completo
ok((1 to 20).toList, (1 to 20).toList.reverse)