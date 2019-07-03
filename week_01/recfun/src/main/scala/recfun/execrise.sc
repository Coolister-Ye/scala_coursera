import math.abs

def fixedPoint(f: Double => Double)(firstGuess: Double) = {
  def isGoodEnough(x: Double, y: Double) = abs((y - x)/x) < 0.0001
  def iterate(guess: Double): Double = {
    println(guess)
    val next = f(guess)
    if (isGoodEnough(guess, next)) next
    else iterate(next)
  }
  iterate(firstGuess)
}

fixedPoint(x => 1 + x/2)(1)

def averageDamp(f: Double => Double)(x: Double) = (f(x) + x)/2
def sqrt(x: Double) = fixedPoint(averageDamp(y=> x/y))(1)
sqrt(2)