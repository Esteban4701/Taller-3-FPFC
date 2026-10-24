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
aplicarMovimientos((List(1,2,3,4),Nil,Nil),List(Uno(4), Uno(-2), Dos(2), Uno(-1), Dos(-2), Uno(-4), Uno(3), Uno(-1), Dos(1), Uno(-1), Dos(-1), Uno(-3), Uno(2), Uno(-1), Dos(1), Uno(-1), Dos(-1), Uno(-2)))

definirManiobra(List(2,3,5,6),List(6,2,3,5)) //Espero que al pasar t1 y los movs a aplicarMovimientos el último estado sea(List(6,2,3,5),Nil,Nil)
aplicarMovimientos((List(2,3,5,6),Nil,Nil),List(Uno(4), Uno(-3), Dos(3), Uno(-1), Dos(-3), Uno(-4)))

definirManiobra(List(1,3,4,5,2),List(1,2,3,4,5))//Espero que al pasar t1 y los movs a aplicarMovimientos el último estado sea (List(1,2,3,4,5),Nil,Nil)
aplicarMovimientos((List(1,3,4,5,2),Nil,Nil),List(Uno(4), Uno(-3), Dos(3), Uno(-1), Dos(-3), Uno(-4)))

definirManiobra(List(1,2),List(2,1)) //Espero que al pasar t1 y los movs a aplicarMovimientos el último estado sea (List(2,1),Nil,Nil)
aplicarMovimientos((List(1,2),Nil,Nil),List(Uno(2), Uno(-1), Dos(1), Uno(-1), Dos(-1), Uno(-2)))

definirManiobra(List(1,"a",3),List("a",3,1)) //Espero que al pasar t1 y los movs a aplicarMovimientos el último estado sea (List("a",3,1),Nil,Nil)
aplicarMovimientos((List(1,"a",3),Nil,Nil),List(Uno(3), Uno(-1), Dos(1), Uno(-1), Dos(-1), Uno(-3), Uno(2), Uno(-1), Dos(1), Uno(-1), Dos(-1), Uno(-2)))
