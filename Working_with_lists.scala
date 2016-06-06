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

object P09 {
  //Pack consecutive duplicates of list elements into sublists.
  assert(pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) ==
    List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e)))
  
  def pack[A](l: List[A]): List[List[A]] = {
    def pack(l: List[A], k: A, cell: List[A], res: List[List[A]]): List[List[A]] = l match {
      case Nil => res ::: List(cell)
      case x :: xs => if (x == k) pack(l.tail, k, cell ::: List(x), res)
                      else pack(l.tail, x, x :: Nil, res ::: List(cell))
    }
    pack(l.tail, l.head, l.head :: Nil, Nil)
  }
}

object P10 {
  //Run-length encoding of a list.
  assert(encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) == 
    List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e)))
  
  def encode[A](l: List[A]): List[(Int, A)] = {
    def encode(l: List[A], k: A, i: Int, res: List[(Int, A)]): List[(Int, A)] = l match {
      case Nil => res ::: List((i, k))
      case x :: xs => if (x == k) encode(l.tail, k, i + 1, res)
                      else encode(l.tail, x, 1, res ::: List((i, k)))
    }
    encode(l.tail, l.head, 1, Nil)
  }
}

object P11 {
  //Modified run-length encoding.
  assert(encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) ==
    List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e)))
  
  def encodeModified[A](l: List[A]): List[Any] = {
    def encodeModified(l: List[A], k: A, i: Int, res: List[Any]): List[Any] = l match {
      case Nil => if (i == 1) res ::: List(k) else res ::: List((i, k))
      case x :: xs => if (x == k) encodeModified(l.tail, k, i + 1, res)
                      else if (i == 1) encodeModified(l.tail, x, 1, res ::: List(k))
                      else encodeModified(l.tail, x, 1, res ::: List((i, k)))
    }
    encodeModified(l.tail, l.head, 1, Nil)
  }
}

object P12 {
  //Decode a run-length encoded list.
  assert(decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))) ==
    List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
    
  def decode[A](l: List[(Int, A)]): List[A] = {
    def decodeCell(c: (Int, A), res: List[A]): List[A] = c._1 match {
      case 0 => res
      case _ => decodeCell((c._1 - 1, c._2), res ::: List(c._2))
    }
  def decode(l: List[(Int, A)], res: List[A]): List[A] = l match {
      case Nil => res
      case x :: xs => decode(xs, res ::: decodeCell(x, Nil))
    }
  decode(l, Nil)
  }
}

object P14 {
  //Duplicate the elements of a list.
  assert(duplicate(List('a, 'b, 'c, 'c, 'd)) ==
    List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd))
    
  def duplicate[A](l: List[A]): List[A] = l flatMap {
    case x => List(x, x)
  }
}

object P15 {
  //Duplicate the elements of a list a given number of times.
  assert(duplicateN(3, List('a, 'b, 'c, 'c, 'd)) == 
    List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd))
    
  def duplicateN[A](n: Int, l: List[A]): List[A] = l flatMap {
    case x => List.fill(n)(x)
  }
}

object P16 {
  //Drop every Nth element from a list.
  assert(drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) ==
    List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k))
    
  def drop[A](n: Int, l: List[A]): List[A] = {
    def drop(l: List[A], acc: Int, res: List[A]): List[A] = l match {
      case Nil => res
      case x :: xs => if (acc == n) drop(l.tail, 1, res)
                      else drop(l.tail, acc + 1, res ::: List(l.head))
    }
    drop(l, 1, Nil)
  }
}

object P17 {
  //Split a list into two parts.
  assert(split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) ==
    (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
    
  def split[A](i: Int, l: List[A]): (List[A], List[A]) = {
    def split[A](acc: Int, l: List[A], rsf: List[A]): (List[A], List[A]) = acc match {
      case 0 => (rsf, l)
      case _ => split(acc - 1, l.tail, rsf ::: List(l.head))
    }
    split(i, l, Nil)
  }
}

object P18 {
  // Extract a slice from a list.
  assert(slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) ==
    List('d, 'e, 'f, 'g))
    
  def slice[A](a: Int, b: Int, l: List[A]): List[A] = {
    def slice(l: List[A], acc: Int, res: List[A]): List[A] = acc match {
      case x if (acc == b) => res
      case x if (acc >= a) => slice(l.tail, acc + 1, res ::: List(l.head))
      case _ => slice(l.tail, acc + 1, res)
    }
    slice(l, 0, Nil)
  }
}

object P19 {
  //Rotate a list N places to the left.
  assert(rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) ==
    List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c))
  assert(rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) ==
    List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i))
    
  def rotate[A](n: Int, l: List[A]): List[A] = n match {
    case 0 => l
    case x if x > 0 => rotate(n - 1, l.tail ::: List(l.head))
    case _ => rotate(n + 1, l.last :: l.init)
  }
}

object P20 {
  // Remove the Kth element from a list.
  assert(removeAt(1, List('a, 'b, 'c, 'd)) == (List('a, 'c, 'd),'b))
  
  def removeAt[A](n: Int, l: List[A]): (List[A], A) = {
    def removeAt(acc: Int, l: List[A], res: List[A]): (List[A], A) = acc match {
      case 0 => (res ::: l.tail, l.head)
      case _ => removeAt(acc - 1, l.tail, res ::: List(l.head))
    }
    removeAt(n, l, Nil)
  }
}

object P21 {
  // Insert an element at a given position into a list.
  assert(insertAt('new, 1, List('a, 'b, 'c, 'd)) == List('a, 'new, 'b, 'c, 'd))
  
  def insertAt[A](i: A, n: Int, l: List[A]): List[A] = {
    def insertAt(acc: Int, l: List[A], res: List[A]): List[A] = acc match {
      case 0 => res ::: List(i) ::: l
      case _ => insertAt(acc - 1, l.tail, res ::: List(l.head))
    }
    insertAt(n, l, Nil)
  }
}

object P22 {
  // Create a list containing all integers within a given range.
  assert(range(4, 9) == List(4, 5, 6, 7, 8, 9))
  
  def range(a: Int, b: Int): List[Int] = {
    def range(acc: Int, res: List[Int]): List[Int] = acc match {
      case x if (x == b) => res ::: List(b)
      case _ => range(acc + 1, res ::: List(acc))
    }
    range(a, Nil)
  }
}

object P23 {
  // Extract a given number of randomly selected elements from a list.
  
  def randomSelect[A](n: Int, l: List[A]): List[A] = {
    def randomSelect(acc: Int, l: List[A], res: List[A]): List[A] = acc match {
      case 0 => res
      case _ => val a = P20.removeAt(scala.util.Random.nextInt(l.length), l)
      randomSelect(acc - 1, a._1, res ::: List(a._2))
    }
    randomSelect(n, l, Nil)
  }
}

object P24 {
  // Lotto: Draw N different random numbers from the set 1..M.
  
  def lotto(n: Int, a: Int): List[Int] = {
    def lotto(acc: Int, res: List[Int]): List[Int] = acc match {
      case 0 => res
      case _ => lotto(acc - 1, res ::: List(scala.util.Random.nextInt(a)))
    }
    lotto(n, Nil)
  }
}

object P25 {
  // Generate a random permutation of the elements of a list.
  def randomPermute[A](l: List[A]): List[A] = P23.randomSelect(l.length, l)
}
