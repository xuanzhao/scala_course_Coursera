package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }

    println("Parentheses balancing")
    print(balance("())(".toList))
    println()

    println("Money change")
    print(countChange(4, List(1,2)))
    println()
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = {
      if (c==r) 1
      else if (c==0) 1
      else pascal(c-1, r-1) + pascal(c, r-1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def iter(rest_chars: List[Char], num: Int): Int = {

        if (num < 0 || rest_chars.isEmpty) num

        else if(rest_chars.head == '(') iter(rest_chars.tail, num +1)
        else if (rest_chars.head == ')') iter(rest_chars.tail, num -1)
        else iter(rest_chars.tail, num)
      }
      iter(chars, 0) == 0
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      def cc(money: Int, coins: List[Int]): Int = {
        if (money == 0) 1
        else if ((money < 0) || (coins.isEmpty)) 0
        else {
          cc(money, coins.tail) + cc(money-coins.head, coins)
        }
      }
      cc(money, coins)
    }
  }
