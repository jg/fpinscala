import fpinscala.state._
import org.scalatest._
import org.scalatest.BeforeAndAfter
import org.scalatest.matchers.ShouldMatchers

class StateSpec extends FunSpec with ShouldMatchers {

  describe("positiveInt") {
    val rng = RNG.simple(123)
    val positiveInt: (Int, RNG) = 
      rng.positiveInt_v2(rng);

    it("should return a positive random integer as the first field") {
      assert(positiveInt._1 === 47324114);
    }
  }

  describe("double") {
    val rng = RNG.simple(123)
    val (result, _): (Double, RNG) = 
      rng.double(rng);

    it("should return a positive random integer as the first field") {
      assert(result === 0.022037007845938206);
    }
  }

  describe("ints") {
    val rng = RNG.simple(123)
    val lst = rng.ints_v2(10)(rng)

    it("should return a list of random integers") {
      val list = List(47324114, -386449838, 806037626, -1537559018,
                      936386220, -1513553461, -550387564, -694240338,
                      -39326366, 696723296)
      list should have length (10)
      lst._1 should equal(list)
    }
  }

  describe("positiveMax") {
    val rng = RNG.simple(123);
    val int = rng.positiveMax(10)(rng);

    it("returns a positive random number in the specified range") {
      int._1 should equal(2)

    }
  }
}
