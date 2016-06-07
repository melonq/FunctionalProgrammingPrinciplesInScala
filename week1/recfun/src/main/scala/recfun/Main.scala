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

    //if (c < 0 || r < 0) throw new NoSuchElementException
    //if (c == 0 || r == 0 || r == c) return 1
    //
    //pascal(c - 1, r - 1) + pascal(c, r - 1)

    r match {
      case 0 | `c` => 1
      case a: Int if c == 0 => 1
      case _ => pascal(c - 1, r - 1) + pascal(c, r - 1)
    }

  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {

    def count(l: List[Char], c: Int = 0): Int = {
      l match {
        case Nil => c
        case head :: tail =>
          head match {
            case '(' => count(tail, c + 1)
            case ')' => if (c > 0) count(tail, c - 1) else -1
            case _ => count(tail, c)
          }
      }
    }

    count(chars) == 0
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {

    def count(m: Int, cs: List[Int]): Int = {
      m match {
        case 0 => 1
        case negativeNum if m < 0 => 0
        case _ =>
          cs match {
            case Nil => 0
            case head :: tail => count(m - head, cs) + count(m, tail)
          }
      }
    }

    coins match {
      case Nil => 0
      case _ => count(money, coins.sorted.reverse)
    }
  }
}
