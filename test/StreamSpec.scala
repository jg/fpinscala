import fpinscala.laziness._
import org.scalatest._
import org.scalatest.BeforeAndAfter
import org.scalatest.matchers.ShouldMatchers


class StreamSpec extends FunSpec with ShouldMatchers {
  describe("stream") {
    val ones = Stream.ones

    describe("take") {
      it("should take first n elements from the stream") {
        ones.take(3).toList should (equal (List(1, 1, 1)))
      }
    }

    describe("takeWhile") {
      it("should take the elements while predicate is true") {
        Stream.oneToFive.takeWhile(_ % 5 != 0).toList should
        (equal (List(1, 2, 3, 4)))
      }
    }

    describe("forAll") {
      it("should be true if the predicate is true forall stream elements") {
        Stream.oneToFive.take(5).forAll(_ <= 5) should be (true)
      }
    }

    describe("takeWhile2") {
      it("should behave like takeWhile") {
        Stream.oneToFive.takeWhile(_ <= 3).toList should equal(List(1, 2, 3))
      }
    }


  }
}
