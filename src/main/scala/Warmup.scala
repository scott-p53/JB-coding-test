class Warmup {
    def pow_of_2(x: Int): Int = {
        require(x >= 0, "x must be >= 0")
        if (x == 0) {
            return 1
        }

        pow_of_2(x - 1) + pow_of_2(x - 1)
    }
}
