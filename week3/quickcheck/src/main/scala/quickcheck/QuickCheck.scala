package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    i <- arbitrary[Int]
    h <- oneOf(const(empty), genHeap)
  } yield insert(i, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("ins2min_Less") = forAll { (a: Int, b: Int) =>
    val h = insert(b, insert(a, empty))
    findMin(h) == Math.min(a, b)
  }

  property("insEmptydelmin_Empty") = forAll { (a: Int) =>
    val h = deleteMin(insert(a, empty))
    isEmpty(h)
  }

  property("remRec_ordSeq") = forAll { (h: H) =>

    def getHeapAsOrderedList(h: H) : List[Int] = {
      if (isEmpty(h)) List()
      else findMin(h) :: getHeapAsOrderedList(deleteMin(h))
    }

    val list = getHeapAsOrderedList(h)
    list == list.sorted
  }

  property("meld2heaps_min") = forAll { (h1: H, h2: H) =>
    val m = meld(h1, h2)
    val m_min = findMin(m)
    m_min == findMin(h1) || m_min == findMin(h2)
  }
}
