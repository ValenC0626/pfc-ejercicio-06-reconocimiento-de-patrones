package taller

// Una figura tiene nombre, área y perímetro.
abstract class Figura {
  def nombre: String
  def area: Double
  def perimetro: Double

  // Responde si esta figura tiene más área que la otra.
  def esMayorQue(otra: Figura): Boolean = {
    area > otra.area
  }
}

// Lo que se puede agrandar o encoger multiplicando sus medidas por k.
trait Escalable {
  def escalar(k: Double): Figura
}

// Círculo de radio dado.
class Circulo(val radio: Double) extends Figura with Escalable {
  def nombre: String = "círculo"

  def area: Double = {
    Math.PI * radio * radio
  }

  def perimetro: Double = {
    2 * Math.PI * radio
  }

  def escalar(k: Double): Figura = {
    new Circulo(radio * k)
  }
}

class Rectangulo(val base: Double, val altura: Double)
    extends Figura with Escalable {

  def nombre: String = "rectángulo"

  def area: Double = {
    base * altura
  }

  def perimetro: Double = {
    2 * (base + altura)
  }

  def escalar(k: Double): Figura = {
    new Rectangulo(base * k, altura * k)
  }
}

// Un cuadrado es un rectángulo con los dos lados iguales.
class Cuadrado(val lado: Double) extends Rectangulo(lado, lado) {

  override def nombre: String = "cuadrado"

  override def escalar(k: Double): Figura = {
    new Cuadrado(lado * k)
  }
}

// Triángulo rectángulo.
class Triangulo(val base: Double, val altura: Double)
    extends Figura with Escalable {

  def nombre: String = "triángulo"

  def area: Double = {
    (base * altura) / 2
  }

  def perimetro: Double = {
    val hipotenusa = Math.sqrt(base * base + altura * altura)
    base + altura + hipotenusa
  }

  def escalar(k: Double): Figura = {
    new Triangulo(base * k, altura * k)
  }
}