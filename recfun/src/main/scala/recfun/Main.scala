package recfun

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
    if (c == 0 || r == c) {
      1
    } else {
      pascal(c - 1, r - 1) + pascal(c, r - 1)
    }
  }
  
  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    recursiveBalance(chars)
  }

  def recursiveBalance(chars: List[Char]): Boolean = {
    val string = chars.filter(c => c == '(' || c == ')').mkString
    val replaced = string.replace("()", "")
    if (string == replaced) {
      string.isEmpty
    } else {
      recursiveBalance(replaced.toList)
    }
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    val sorted = coins.sorted(Ordering[Int].reverse)
    sorted.map(coin => {
      if (money > coin) {
        countChange(money - coin, sorted.dropWhile(c => c > coin))
      } else if (money == coin) {
        1
      } else {
        0
      }
    }).sum
  }
}
