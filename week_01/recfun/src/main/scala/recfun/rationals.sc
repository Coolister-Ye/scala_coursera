class Rationals (x: Int, y: Int) {
  require(y > 0, "denominator must be positive")
  def this(x: Int) = this(x, 1)
  def gcd(x: Int, y: Int): Int = if (y == 0) x else gcd(y, y % x)
  val numer = x / gcd(x, y)
  val demon = y / gcd(x, y)

  def add(that: Rationals) =
    new Rationals(
      numer * that.demon + that.numer * demon,
      demon * that.demon
    )

  def neg = new Rationals(-numer, demon)
  def sub(that: Rationals) = add(that.neg)
  def less(that: Rationals) = numer * that.demon < that.numer * demon
  def max(that: Rationals) = if(less(that)) that else this
  override def toString: String =
    numer + "/" + demon
}

val x = new Rationals(1, 2)
val y = new Rationals(3, 2)
val z = new Rationals(7, 10)
x.add(y)
x.sub(y).sub(z)