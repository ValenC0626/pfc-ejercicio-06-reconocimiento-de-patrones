package taller

class Ejercicio() {

  // Punto 1. La suma de las áreas de todas las figuras de la lista.
  def areaTotal(figuras: List[Figura]): Double = {
    figuras match {
      case Nil => 0.0
      case cabeza :: cola => cabeza.area + areaTotal(cola)
    }
  }

  // Punto 2. Los elementos de l sin repetidos y en orden ascendente.
  // La lista puede llegar desordenada.
  def sinRepetidos(l: List[Int]): List[Int] = {
    l.distinct.sorted
  }

  // Punto 3. Los elementos que están en las dos listas, sin repetidos
  // y en orden ascendente.
  def comunes(l1: List[Int], l2: List[Int]): List[Int] = {
    l1.intersect(l2).distinct.sorted
  }

  // Punto 4. El valor entero de la expresión.
  def evaluar(e: Expr): Int = {
    e match {
      case Numero(valor) => valor
      case Suma(e1, e2) => evaluar(e1) + evaluar(e2)
      case Resta(e1, e2) => evaluar(e1) - evaluar(e2)
      case Prod(e1, e2) => evaluar(e1) * evaluar(e2)
    }
  }

  // La expresión en una línea, con paréntesis solo donde hacen falta.
  def mostrar(e: Expr): String = {
    e match {
      case Numero(valor) =>
        valor.toString

      case Suma(e1, e2) =>
        mostrar(e1) + " + " + mostrar(e2)

      case Resta(e1, e2) =>
        mostrar(e1) + " - " + mostrar(e2)

      case Prod(e1, e2) =>
        mostrar(e1) + " * " + mostrar(e2)
    }
  }

  // La expresión sin sumas de cero, productos por uno, productos por cero
  // ni restas de una expresión consigo misma. No hace aritmética.
  def simplificar(e: Expr): Expr = {
    e match {
      case Numero(valor) =>
        Numero(valor)

      case Suma(e1, e2) =>
        val s1 = simplificar(e1)
        val s2 = simplificar(e2)

        (s1, s2) match {
          case (Numero(0), _) => s2
          case (_, Numero(0)) => s1
          case _ => Suma(s1, s2)
        }

      case Resta(e1, e2) =>
        val s1 = simplificar(e1)
        val s2 = simplificar(e2)

        if (s1 == s2) Numero(0)
        else Resta(s1, s2)

      case Prod(e1, e2) =>
        val s1 = simplificar(e1)
        val s2 = simplificar(e2)

        (s1, s2) match {
          case (Numero(0), _) => Numero(0)
          case (_, Numero(0)) => Numero(0)
          case (Numero(1), _) => s2
          case (_, Numero(1)) => s1
          case _ => Prod(s1, s2)
        }
    }
  }
}
