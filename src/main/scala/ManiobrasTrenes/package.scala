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
      case Uno(0) => e

      case Dos(0) => e

      case Uno(n) =>
        if (n>0)
          if(e._1.length >= n)
            (e._1.take(e._1.length - n), e._1.drop(e._1.length - n) ++ e._2, e._3)
          else
            (Nil,e._1 ++ e._2, e._3)

        else
          if(e._2.length >= -n)
            (e._1 ++ e._2.take(-n), e._2.drop(-n), e._3)
          else
            (e._1 ++ e._2, Nil, e._3)

      case Dos(n) =>
        if (n>0)
          if(e._1.length >= n)
            (e._1.take(e._1.length - n),e._2, e._1.drop(e._1.length - n) ++ e._3)
          else
            (Nil,e._2,e._1 ++ e._3)

        else
          if(e._3.length >= -n)
            (e._1 ++ e._3.take(-n), e._2, e._3.drop(-n))
          else
            (e._1 ++ e._3, e._2, Nil)
    }
  }

  def aplicarMovimientos(e:Estado, movs:Maniobra): List[Estado]= {
    movs match {
      case Nil => List(e)
      case x::xs => e::aplicarMovimientos(aplicarMovimiento(e,x),xs)
    }
  }

  def aplicarMovimientosTren(e:Estado, movs:Maniobra): Tren = {
    movs match {
      case Nil => e._1
      case x :: xs => aplicarMovimientosTren(aplicarMovimiento(e, x), xs)
    }
  }

  def definirManiobra(t1:Tren, t2:Tren): Maniobra = {
    (t1, t2) match {
      case (x :: xs, y :: ys) =>
        if (x == y) {
          definirManiobra(xs, ys)
        }
        else {
          val movs = List(Uno(t1.length), Uno(-(t1 indexOf y)), Dos(t1 indexOf y), Uno(-t1.length), Dos(-(t1 indexOf y)))
          movs ++ definirManiobra(aplicarMovimientosTren((t1, Nil, Nil), movs).tail, ys)
        }
      case (Nil, _) => Nil
      case (_, Nil) => Nil
    }
  }
}
