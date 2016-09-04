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
h3 = Bogus3.deleteMin(h3)
Bogus3.findMin(h3)

object Bogus4 extends Bogus4BinomialHeap with IntHeap {
}

var h4 = Bogus4.insert(3, Bogus4.empty)
h4 = Bogus4.insert(7, h4)
h4 = Bogus4.insert(4, h4)
h4 = Bogus4.deleteMin(h4)
Bogus4.findMin(h4)
