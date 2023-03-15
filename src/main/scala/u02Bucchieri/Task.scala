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

  //Task 4
  @tailrec
  def gcd(a: Int, b: Int): Int = a > b match
    case true => a % b match
      case 0 => b
      case _ => gcd(b, a % b)
    case false => b % a match
      case 0 => a
      case _ => gcd(a, b % a)


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
      case Rectangle(b, h, o) =>
        (point._1 <= o._1 + b / 2 & point._1 >= o._1 - b / 2) &
          (point._2 <= o._2 + h / 2 & point._2 >= o._2 - h / 2)
      case Circle(r, o) =>
        ((point._1 - o._1) * (point._1 - o._1) + (point._2 - o._2) * (point._2 - o._2)) <= r * r
      case Square(l, o) =>
        (point._1 <= o._1 + l / 2 & point._1 >= o._1 - l / 2) &
          (point._2 <= o._2 + l / 2 & point._2 >= o._2 - l / 2)

  import Shape.*


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
