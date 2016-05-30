object P01 {
  //Find the last element of a list.
  assert(last(List(1, 1, 2, 3, 5, 8)) == 8)
      
  def last[A](l: List[A]): A = l match {
    case x :: Nil  => x
    case x :: xs => last(xs)
    case _ => throw new NoSuchElementException
  }
}

object P02 {
  //Find the last but one element of a list.
  assert(penultimate(List(1, 1, 2, 3, 5, 8)) == 5)
  
  def penultimate[A](l: List[A]): A = l match {
    case x :: y :: Nil => x
    case x :: xs => penultimate(xs)
    case _ => throw new NoSuchElementException
  }
}

object P03 {
  //Find the Kth element of a list.
  assert(nth(2, List(1, 1, 2, 3, 5, 8)) == 2)
  
  def nth[A](n: Int, l: List[A]): A = n match {
    case 0 => l.head
    case _ => nth(n - 1, l.tail)
  }
}

object P04 {
  //Find the number of elements of a list.
  assert(length(List(1, 1, 2, 3, 5, 8)) == 6)
  
  def length[A](l: List[A]): Int = {
    def length[A](l: List[A], res: Int): Int = l match {
      case Nil =>  res
      case x :: xs => length(xs, res + 1)
    }
    length(l, 0)
  }
}

object P05 {
  //Reverse a list.
  assert(reverse(List(1, 1, 2, 3, 5, 8)) == List(8, 5, 3, 2, 1, 1))
  
  def reverse[A](l: List[A]): List[A] = {
    def reverse(l: List[A], res: List[A]): List[A] = l match {
      case Nil => res
      case x :: xs => reverse(xs, x :: res)
    }
    reverse(l, Nil)
  }
}

object P06 {
  //Find out whether a list is a palindrome.
  assert(isPalindrome(List(1, 2, 3, 2, 1)) == true)
  
  def isPalindrome[A](l : List[A]): Boolean = l match {
    case Nil => true
    case x :: Nil => true
    case _ => if (l.head == l.last) isPalindrome(l.tail.init) else false
  }
}

object P07 {
  //Flatten a nested list structure.
  assert(flatten(List(List(1, 1), 2, List(3, List(5, 8)))) == List(1, 1, 2, 3, 5, 8))
  
  def flatten(l: List[Any]): List[Any] = l flatMap {
    case xs: List[Any] => flatten(xs)
    case x => List(x)
  }
}

object P08 {
  //Eliminate consecutive duplicates of list elements.
  assert(compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) == List('a, 'b, 'c, 'a, 'd, 'e))
  
  def compress[A](l: List[A]): List[A] = {
    def compress(l: List[A], k: A, res: List[A]): List[A] = l match {
      case Nil => res
      case x :: xs => if (x == k) compress(l.tail, k, res)
                      else compress(l.tail, x, res ::: List(x))
    }
    compress(l, l.head, List(l.head))
  }
}
