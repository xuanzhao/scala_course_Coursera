package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {


  lazy val genHeap: Gen[H] = for {
    a <- arbitrary[Int]
    h <- Gen.frequency((1, empty), (2, genHeap))
  } yield insert(a, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)


  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  //If you insert an element into an empty heap, then delete the minimum, the resulting heap should be empty.
  property("delete a return empty") = forAll { v: Int =>
    deleteMin(insert(v, empty)) == empty
  }

  // if you insert any two elements into an empty heap, finding the minimum of the resulting heap should get
  // the smallest of the two elements back.
  property("find_min") = forAll { (v1: Int, v2: Int) =>
    val h = insert(v1, insert(v2, empty))
    findMin(h) == (v1 min v2)

  }



  // Finding a minimum of the melding of any two heaps should return a minimum of one or the other.
  property("meld") = forAll { (a: H, b: H) =>
    findMin(meld(a, b)) == List(findMin(a), findMin(b)).min
  }

  // Given any heap, you should get a sorted sequence of elements when continually finding and deleting minima.
  // (Hint: recursion and helper functions are your friends.)
  property("sorted") = forAll { (h: H) =>
    def delete(a: H, accu: List[Int]): List[Int] = {
      if (isEmpty(a)) accu
      else {
        val min = findMin(a)
        delete(deleteMin(a), accu :+ min)
      }
    }
    val result = delete(h, List())
    result == result.sorted
  }

  // if you insert any 3 elements into an empty heap and delete the minimum,
  // the next minimum should be the second largest element
  property("delete") = forAll { (a: Int, b: Int, c: Int) =>
    val h = insert(c, insert(b, insert(a, empty)))
    findMin(deleteMin(h)) == List(a, b, c).sorted.drop(1).head
  }





}
