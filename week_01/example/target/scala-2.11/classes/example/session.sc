1 + 2

def abs(x: Double) = if (x < 0) -x else x

def isGoodEnough(guess: Double, x: Double) =
  abs(guess * guess -x) / x < 0.01

def improve(guess: Double, x: Double) =
  (guess + x / guess) / 2

def sqrtIter(guess: Double, x: Double):Double =
  if (isGoodEnough(guess, x)) guess
  else sqrtIter(improve(guess, x), x)

def sqrt(x: Double) = sqrtIter(1.0, x)

sqrt(10)
sqrt(8e-10)

