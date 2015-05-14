package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  lazy val genHeap: Gen[H] = for {
    n <- arbitrary[Int]
    h <- oneOf(genHeap, const(empty))
  } yield insert(n, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("finding minimum after two inserts") = forAll { t: (Int, Int) =>
    t match {
      case (x, y) => {
        val min = Math.min(x, y)
        val h = empty
        findMin(insert(x, insert(y, h))) == min
      }
    }
  }

  property("insert min twice") = forAll { n: (Int) =>
    if (n < 0) {
      true
    } else {
      val h = empty
      val twoMins = insert(n, (insert(n-1, h)))
      val min1 = findMin(twoMins)
      val min2 = findMin(deleteMin(twoMins))
      min1 == n-1 && min2 == n
    }
  }

  property("delete ordered1") = forAll { n: (Int) =>
    if (n < 0) {
      true
    } else {
      val h = empty
      val min1 = n-2
      val min2 = n-1
      val min3 = n
      findMin(deleteMin(insert(min3, insert(min2, insert(min1, h))))) == min2
    }
  }
  property("delete ordered2") = forAll { n: (Int) =>
    if (n < 0) {
      true
    } else {
      val h = empty
      val min1 = n-2
      val min2 = n-1
      val min3 = n
      findMin(deleteMin(insert(min1, insert(min2, insert(min3, h))))) == min2
    }
  }

  property("delete ordered3") = forAll { n: (Int) =>
    if (n < 0) {
      true
    } else {
      val h = empty
      val min1 = n-2
      val min2 = n-1
      val min3 = n
      findMin(deleteMin(insert(min3, insert(min2, insert(min1, h))))) == min2
    }
  }

  property("deleting til empty") = forAll { n: (Int) =>
    val h = empty
    isEmpty(deleteMin(insert(n, h)))
  }

  property("ordered set out of a heap") = forAll { (h: H) =>
    isSortedHeap(h)
  }

  property("meld should keep ordering") = forAll { (hs: (H, H)) =>
    hs match {
      case (h1, h2) => isSortedHeap(meld(h1, h2))
    }
  }

  property("meld should keep minimum") = forAll { (hs: (H, H)) =>
    hs match {
      case (h1, h2) => {
        if (!isEmpty(h1) && !isEmpty(h2)) {
          val m1 = findMin(h1)
          val m2 = findMin(h2)
          val min = Math.min(m1, m2)
          findMin(meld(h1, h2)) == min
        }
        true
      }
    }
  }


  property("foo") = forAll { (xs: List[Int]) =>
    val h = xs.foldRight(empty)((x, h) => insert(x + 1, h))
    val hm = insert(Int.MinValue, h)
    findMin(hm) == Int.MinValue
  }

  property("bar") = forAll { (h: H) =>
    if (isEmpty(h) || isEmpty(deleteMin(h)))
      true
    else
      isSortedHeap(deleteMin(deleteMin(h)))
  }

  def isSortedHeap(h: H): Boolean = {
    if (isEmpty(h)) {
      return true
    }
    if (isEmpty(deleteMin(h)))
      return true
    val min1 = findMin(h)
    val min2 = findMin(deleteMin(h))
    if (min1 > min2)
      return false
    else
      isSortedHeap(deleteMin(h))
  }

  def printHeap(h: H): Unit = {
    if (isEmpty(h)) {
      return
    }
    val min1 = findMin(h)
    printHeap(deleteMin(h))
  }
}
