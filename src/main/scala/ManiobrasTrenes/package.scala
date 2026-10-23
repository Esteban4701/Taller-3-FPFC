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
  /*Es una recursion lineal la condición para que entre al if es que movs no este vacio, 
  si no lo está, hace una lista donde concatena el estado con la llamada recursiva, en esta se le pasara
  como primer parametro un nuevo estado obtenido de aplicar la función aplicarMovimiento que tiene como parámetros
   el estado actual y el primer movimiento de movs, el segundo parametro será movs si este primer elemento,
   seguirá asi hasta que movs se quede sin elementos en su lista a lo cual pasara al else donde devolverá 
   la lista con el último estado*/
  def aplicarMovimientos(e:Estado, movs:Maniobra): List[Estado]= {
    if(movs.nonEmpty) e::aplicarMovimientos(aplicarMovimiento(e,movs.head),movs.drop(1)) // 
    else List(e)
  }
}
