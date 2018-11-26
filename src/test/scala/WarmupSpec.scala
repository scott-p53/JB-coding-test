import org.scalatest.FlatSpec

class WarmupSpec extends FlatSpec {
    "A warmup" should "calculate the powers of 2" in {
        val warmup = new Warmup
        assert(warmup.pow_of_2(0) === 1)
        assert(warmup.pow_of_2(10) === 1024)
        assert(warmup.pow_of_2(5) === 32)
    }

    it should "throw IllegalArgumentException if a negative number is used as input" in {
        val warmup = new Warmup
        assertThrows[IllegalArgumentException] {
            warmup.pow_of_2(-1)
        }
    }
}
