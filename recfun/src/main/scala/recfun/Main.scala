package recfun

import languageFeature.higherKinds
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
    if (c < 0 || r < 0) 0
    else if (c == 0 && r == 0) 1
    else pascal(c, r - 1) + pascal(c - 1, r - 1)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def helper(accu: Int, chars: List[Char]): Boolean = {
      (accu >= 0) && (
          if (chars.isEmpty) accu == 0
          else {
            val tail = chars.tail
            chars.head match {
              case '(' => helper(accu + 1, tail)
              case ')' => helper(accu - 1, tail)
              case _   => helper(accu, tail)
            }
          })
    }

    helper(0, chars)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    // money = 4
    // coins = [1, 2]  unique values

    // helper(4, [2]) + helper(4-1, [1, 2])
    if (money == 0 && coins.nonEmpty) 1
    else if (money < 0 || coins.isEmpty) 0
    else countChange(money, coins.tail) + countChange(money - coins.head, coins)
  }
}
