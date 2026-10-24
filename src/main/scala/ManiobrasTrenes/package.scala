package object ManiobrasTrenes {
  type Vagon = Any
  type Tren = List[Vagon]
  type Estado = (Tren, Tren, Tren)

  trait Movimiento
  case class Uno (n: Int) extends Movimiento
  case class Dos (n: Int) extends Movimiento

  type Maniobra = List[Movimiento]

  def aplicarMovimiento(e:Estado, m:Movimiento): Estado = {
    m match {
      case Uno(0) => e //Caso base Uno(0) devuelve el estado sin cambios

      case Dos(0) => e //Caso base Dos(0) devuelve el estado sin cambios

      case Uno(n) =>   //Caso Uno(n) dependiendo del "n" decide si los vagones van de e._1 a e._2(Positivo), o de e._2 a e._1(Negativo) y devuelve el estado(Tupla)
        if (n>0)       //Condición n positivo
          if(e._1.length >= n) //Si la cantidad de vagones en e._1 es mayor o igual a n, de e._1 toma los que quedaron, en e._2 se concatena a la izquierda lo que se le quitó a e._1 y e._3 se mantiene igual
            (e._1.take(e._1.length - n), e._1.drop(e._1.length - n) ++ e._2, e._3)
          else //Si la cantidad de vagones en e._1 es menor a n, en e._1 no quedan vagones, en e._2, e._1 se concatena con e._2 y e._3 se mantiene igual
            (Nil,e._1 ++ e._2, e._3)

        else           //Condición n negativo
          if(e._2.length >= -n) //Si la cantidad de vagones en e._2 es mayor o igual que -n, en e._1 se concatena e._1 con los "-n" vagones que pasan de e._2, en e._2 se queda con los que sobran y e._3 se mantiene igual
            (e._1 ++ e._2.take(-n), e._2.drop(-n), e._3)
          else         //Si la cantidad de vagones en e._2 es menor que -n, en e._1 se concatena e._1 con e._2, en e._2 no quedan vagones y e._3 se mantiene igual
            (e._1 ++ e._2, Nil, e._3)

      case Dos(n) =>   //Caso Dos(n) dependiendo del "n" decide si los vagones van de e._1 a e._3(Positivo), o de e._3 a e._1(Negativo) y devuelve el estado(Tupla)
        if (n>0)       //Condición n positivo
          if(e._1.length >= n)  //Si la cantidad de vagones en e._1 es mayor o igual a n, de e._1 toma los que quedaron, en e._2 se mantiene igual y e._3 se concatena a la izquierda lo que se le quitó a e._1
            (e._1.take(e._1.length - n),e._2, e._1.drop(e._1.length - n) ++ e._3)
          else         //Si la cantidad de vagones en e._1 es menor a n, en e._1 no quedan vagones, en e._2 se mantiene igual y en e._3, e._1 se concatena con e._3
            (Nil,e._2,e._1 ++ e._3)

        else           //Condición n negativo
          if(e._3.length >= -n) //Si la cantidad de vagones en e._3 es mayor o igual que -n, en e._1 se concatena e._1 con los "-n" vagones que pasan de e._3, en e._2 se mantiene igual y e._3 se queda con los que sobran
            (e._1 ++ e._3.take(-n), e._2, e._3.drop(-n))
          else         //Si la cantidad de vagones en e._3 es menor que -n, en e._1 se concatena e._1 con e._3, en e._2 se mantiene igual y e._3 no quedan vagones
            (e._1 ++ e._3, e._2, Nil)
    }
  }
  /*Recibe un estado inicial y una lista de movimientos, devuelve una lista de cada estado después de aplicar un movimiento*/
  def aplicarMovimientos(e:Estado, movs:Maniobra): List[Estado]= {
    movs match {
      case Nil => List(e)
      case m::ms => e::aplicarMovimientos(aplicarMovimiento(e,m),ms)
    }
  }
  /*Recibe un estado inicial y una lista de movimientos, devuelve el tren en e._1(Principal), es una función de apoyo*/
  def aplicarMovimientosTren(e:Estado, movs:Maniobra): Tren = {
    movs match {
      case Nil => e._1
      case m :: ms => aplicarMovimientosTren(aplicarMovimiento(e, m), ms)
    }
  }
  //t1 = tren original, t2 = tren deseado 
  /*Algoritmo para organizar el tren:
  1-Uno(t1.length):         Manda el tren completo de e._1(Principal) a e._2(Uno) 
  2-Uno(-(t1 indexOf y)):   El indexOf me da la posición donde este "y" y pasa esa cantidad de e._2(Uno) a e._1(Principal) 
  3-Dos(t1 indexOf y):      Mando la misma cantidad que traje de e._2(Uno), pero ahora haciendo e._1(Principal) a e._3(Dos) 
  4-Uno(-1):                Traigo el "y" que era el que necesitaba de e._2(Uno) hacía e._1 (Principal)
  5-Dos(-(t1 indexOf y)):   Traigo los que había llevado de e._1(Principal) a e._3(Dos), ahora trayéndolos de vuelta de e._3(Dos) a e._1(Principal) 
  6-Uno(-t1.length):        Traigo los que se me quedaron en e._2(Uno) 
  Como tal ahora me quedo con (t1, Nil, Nil) con x==y, el nuevo punto de partida para aplicar de nuevo el algoritmo*/
  def definirManiobra(t1:Tren, t2:Tren): Maniobra = {
    (t1, t2) match {
      case (x :: xs, y :: ys) =>
        if (x == y) {
          definirManiobra(xs, ys)
        }
        else {
          val movs = List(Uno(t1.length), Uno(-(t1 indexOf y)), Dos(t1 indexOf y), Uno(-1), Dos(-(t1 indexOf y)), Uno(-t1.length))
          movs ++ definirManiobra(aplicarMovimientosTren((t1, Nil, Nil), movs).tail, ys)
        }
      case (Nil, _) => Nil
      case (_, Nil) => Nil
    }
  }
}
