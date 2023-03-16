package u02Bucchieri

import scala.annotation.tailrec

object Task extends App:

  // Task 3a
  val positiveVal: Int => String = _ match
    case n if n < 0 => "negative"
    case _ => "positive"

  def positiveDef(n: Int): String = n match
    case n if n < 0 => "negative"
    case _ => "positive"

  //Task 3b
  val negVal: (String => Boolean) => (String => Boolean) = f => b => !f(b)
  def negDef(f: String => Boolean): (String => Boolean) = !f(_)

  //Task 3c
  def negGen[A](f: A => Boolean): (A => Boolean) = !f(_)

  //Task 4
  val p1: Int => Int => Int => Boolean = x => y => z => x <= y && y == z
  val p2: (Int, Int, Int) => Boolean = (x, y, z) => x <= y && y == z
  def p3(x: Int)(y: Int)(z: Int): Boolean = x <= y && y == z
  def p4(x: Int, y: Int, z: Int): Boolean = x <= y && y == z

  //Task 5
  def compose(f: Int => Int, g: Int => Int): Int => Int = i => f(g(i))
  def composeGen[X, Y, Z](f: X => Y, g: Z => X): Z => Y = i => f(g(i))

  //Task 6
  @tailrec
  def gcd(a: Int, b: Int): Int = b == 0 match
    case true => a
    case _ => gcd(b, a % b)

  // Task 7
  enum Shape:
    case Rectangle (b: Double, h: Double, o: (Double, Double))
    case Circle (r: Double, o: (Double, Double))
    case Square (l: Double, o: (Double, Double))

  object Shape:
    def perimeter(shape: Shape): Double = shape match
      case Rectangle(b, h, _) => 2 * (b + h)
      case Circle(r , _) => 2 * r * 3.14
      case Square(l , _) => 4 * l
    def contains(shape: Shape, point: (Double, Double)): Boolean = shape match
      case Rectangle(b, h, o) => (o, point) match
        case ((x0, y0), (x1, y1)) if x0 == x1 & y0 == y1 => true
        case ((x0, y0), (x1, y1)) => (x1 <= x0 + b / 2 & x1 >= x0 - b / 2) &
          (y1 <= y0 + h / 2 & y1 >= y0 - h / 2)
      case Circle(r, o)  => (o, point) match
        case ((x0, y0), (x1, y1)) if x0 == x1 & y0 == y1 => true
        case ((x0, y0), (x1, y1)) => ((x1 - x0) * (x1 - x0) + (y1 - y0) * (y1 - y0)) <= r * r
      case Square(l, o) => (o, point) match
        case ((x0, y0), (x1, y1)) if x0 == x1 & y0 == y1 => true
        case ((x0, y0), (x1, y1)) => (x1 <= x0 + l / 2 & x1 >= x0 - l / 2) &
          (y1 <= y0 + l / 2 & y1 >= y0 - l / 2)

  import Shape.*
  println(contains(Rectangle(2, 2, (1.0, 1.0)), (2.0, 2.0)))

  // Task 8
  enum Option[A]:
    case Some(a: A)
    case None() // here parens are needed because of genericity

  object Option:
    def isEmpty[A](opt: Option[A]): Boolean = opt match
      case None() => true
      case _ => false
    def orElse[A, B >: A](opt: Option[A], orElse: B): B = opt match
      case Some(a) => a
      case _ => orElse
    def flatMap[A, B](opt: Option[A])(f: A => Option[B]): Option[B] = opt match
      case Some(a) => f(a)
      case _ => None()
    def filter[A](option: Option[A])(f: A => Boolean): Option[A] = option match
      case Some(a) if f(a) => option
      case _ => None()
    def map[A](option: Option[A])(f: A => Boolean): Option[Boolean] = option match
      case Some(a) => Some(f(a))
      case _ => None()
    def fold[A](option: Option[A])(d: A)(f: A => A): A = option match
      case Some(a) => f(a)
      case _ => d

end Task
