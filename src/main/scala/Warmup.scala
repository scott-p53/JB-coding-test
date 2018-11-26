class Warmup {
    def pow_of_2(x: Int): Int = {
        if (x == 0) {
            return 1
        }

        pow_of_2(x - 1) + pow_of_2(x - 1)
    }
}
