package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._
import Math._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {
  
  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }
  
  property("empty is empty, unless not empty is empty") = forAll { (a: Int) =>
    isEmpty(empty)
  }
  
  property("not empty is not empty, unless insert is bogus") = forAll { (a: Int) =>
    !isEmpty(insert(a, empty))
  }
    
  property("insert actually inserts") = forAll { a: Int =>
    a == findMin(insert(a, empty))
  }
  
  def isSorted(h: H): Boolean = {
    if (isEmpty(h)) true
    else {
      val m = findMin(h)
      val h2 = deleteMin(h)
      isEmpty(h2) || (m <= findMin(h2) && isSorted(h2))
    }
  }

  property("insert works correctly") = forAll { h: H =>
    isSorted(h)
  }
    
  property("findMin is correct") = forAll { (a: Int, b: Int) =>
    findMin(insert(b, insert(a, empty))) == min(a, b)
  }
  
  property("meld is correct") = forAll { (h1: H, h2: H) =>
    isSorted(meld(h1, h2))
  }

  property("meld") = forAll { (h1: H, h2: H) =>
    def heapEqual(h1: H, h2: H): Boolean =
      if (isEmpty(h1) && isEmpty(h2)) true
      else {
        val m1 = findMin(h1)
        val m2 = findMin(h2)
        m1 == m2 && heapEqual(deleteMin(h1), deleteMin(h2))
      }
    heapEqual(meld(h1, h2),
              meld(deleteMin(h1), insert(findMin(h1), h2)))
  }
 
  lazy val genHeap: Gen[H] = for {
    x <- arbitrary[A]
    h <- oneOf(const(empty), genHeap)
  } yield insert(x, h)
  
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)
}
