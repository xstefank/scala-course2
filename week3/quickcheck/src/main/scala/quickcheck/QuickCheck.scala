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

  //if you insert an element into an empty heap,
  //then find the minimum of the resulting heap,
  //you get the element back
  property("minEmpty") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  //if insert number lesser than any in the heap,
  //this number should be the minimum
  property("minNonEmpty") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  //If you insert any two elements into an empty heap,
  //finding the minimum of the resulting heap should
  //get the smallest of the two elements back
  property("ins2min_Less") = forAll { (a: Int, b: Int) =>
    val h = insert(b, insert(a, empty))
    findMin(h) == Math.min(a, b)
  }

  //If you insert an element into an empty heap,
  //then delete the minimum, the resulting heap should be empty
  property("insEmptydelmin_Empty") = forAll { (a: Int) =>
    val h = deleteMin(insert(a, empty))
    isEmpty(h)
  }

  //Given any heap, you should get a sorted sequence of
  //elements when continually finding and deleting minima
  property("remRec_ordSeq") = forAll { (h: H) =>

    def getHeapAsOrderedList(h: H) : List[Int] = {
      if (isEmpty(h)) List()
      else findMin(h) :: getHeapAsOrderedList(deleteMin(h))
    }

    val list = getHeapAsOrderedList(h)
    list == list.sorted
  }

  //Finding a minimum of the melding of any two heaps should
  //return a minimum of one or the other
  property("meld2heaps_min") = forAll { (h1: H, h2: H) =>
    val m = meld(h1, h2)
    val m_min = findMin(m)
    m_min == findMin(h1) || m_min == findMin(h2)
  }

  //after adding 2 elements to the heap and deleting minimun
  //the resulting heap minimum shoud be the bigger number out
  //of the given 2 numbers
  property("add2delMin_minBigger") = forAll { (a: Int, b: Int) =>
    val h = insert(a, insert(b, empty))
    val h1 = deleteMin(h)
    val minH1 = findMin(h1)

    val biggerNum = Math.max(a, b)
    minH1 == biggerNum
  }

  //after adding 3 numbers to the heap and deleting minimum
  //the resulting heap minimum should be the middle number out
  //of the given 3 numbers
  property("add3delmin_check") = forAll { (a: Int, b: Int, c: Int) =>
    val h = insert(a, insert(b, insert(c, empty)))
    val h1 = deleteMin(h)
    val sortedElems = Vector(a, b, c).sorted
    val middle = sortedElems(1)
    middle == findMin(h1)
  }
}
