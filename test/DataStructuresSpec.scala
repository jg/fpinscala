import fpinscala.datastructures._
import org.scalatest._
import org.scalatest.BeforeAndAfter
import org.scalatest.matchers.ShouldMatchers


class DataStructuresSpec extends FunSpec with ShouldMatchers {
  describe("list") {
    val lst = List[Int](1, 2, 3, 4, 5)
    import List._

    describe("drop") {
      it("should drop the n elements of the list") {
        drop(lst, 0) should (equal (lst))
        drop(lst, 1) should (equal (List(2, 3, 4, 5)))
        drop(lst, 2) should (equal (List(3, 4, 5)))
        drop(lst, 3) should (equal (List(4, 5)))
        drop(lst, 4) should (equal (List(5)))
        drop(lst, 5) should (equal (Nil))
      }
    }

    describe("dropWhile") {
      it("should drop elements from the list while predicate is true") {
        dropWhile(lst, (x: Int) => x <= 3) should (equal (List(4, 5)))
        dropWhile(lst, (x: Int) => x <= 100) should (equal (Nil))
      }
    }

    describe("init") {
      it("should return the list of all but the last elements of the list") {
        init(lst) should (equal (List(1, 2, 3, 4)))
      }
    }

    describe("length") {
      it("should return the length of the list") {
        List.length(lst) should equal (5)
      }
    }

    describe("foldLeft") {
      it("should return the length of the list") {
        foldLeft(lst, 0)(_ + _) should equal (15)
      }
    }

    describe("reverse") {
      it("should reverse the list") {
        reverse(lst) should (equal (List(5, 4, 3, 2, 1)))
      }
    }

    describe("foldLeftViaFoldRight") {
      it("should behave as foldLeft") {
        foldLeftViaFoldRight(lst, 0)(_ + _) should equal (15)
      }
    }

    describe("map") {
      it("should be map") {
        map(lst)(_ + 5) should (equal (List(6, 7, 8, 9, 10)))
      }
    }
  }

}
