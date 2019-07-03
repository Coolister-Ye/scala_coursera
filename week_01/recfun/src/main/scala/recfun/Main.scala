//package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = {
      if (r == 0 && c == 0) 1
      else if (c < 0 || c > r) 0
      else pascal(c - 1, r - 1) + pascal(c, r - 1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {

      def checkParents(leftCount: Int, char: Char): Int = {
        if (char == Char("(")) leftCount + 1
        else if (char == Char(")")) leftCount - 1
        else 0
      }

      def checkRightParents(rightCount: Int, char: Char): Int = {
        if (char == Char(")")) rightCount + 1
        else if (char == )
      }

      def loop(chars: List[Char], leftCount: Int, rightCount: Int): Boolean = {
        if (chars.isEmpty && leftCount == 0 && rightCount == 0) true
        else loop(chars.tail, checkLeftParents(leftCount, chars.head), checkRightParents(rightCount, chars.head))
      }
    }
  
  /**
   * Exercise 3
   */
//    def countChange(money: Int, coins: List[Int]): Int = ???
  }
