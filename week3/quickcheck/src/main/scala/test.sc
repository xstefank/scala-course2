import quickcheck._

object Bogus1 extends Bogus1BinomialHeap with IntHeap {
  var h1 = empty
}

Bogus1.insert(1, Bogus1.h1)
//Bogus1.findMin(Bogus1.h1)


object Bogus3 extends Bogus3BinomialHeap with IntHeap {
}

var h3 = Bogus3.insert(6, Bogus3.empty)
h3 = Bogus3.insert(2, h3)
Bogus3.deleteMin(h3)
Bogus3.findMin(h3)
