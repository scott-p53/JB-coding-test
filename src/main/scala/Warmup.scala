class Warmup {
    /**
      *
      * @param x the power we wish to raise 2 to. X Must be >= 0 or else an IllegalArgumentException is thrown
      * @return 2^^x
      *
      * 1. The big-O complexity O(2^^n) for time requirement and O(n) stack space memory usage
      * 2. There are a few problems with this naive implementation. The first issue is that for large values of x we
      *         will run out of stack space. Another issue is that because we are returning
      *         an Int depending on what the requirements are for calculating the power of 2s are we will overflow an Integer. On top
      *         of these issues the algorithm is very slow and uses unneeded memory to perform the recursion. Ideally in this case
      *         we should use the built in pow function. However, if we needed to implement our own algorithm we could do this in
      *         O(1) time and O(1) space by using the fact that we are calculating the power of 2 which can be represented by 1 << x. This would still have the same issue that if
      *         we wanted to calculate a large enough power of 2 we would overflow the integer. To fix this we would want to use
      *         the built-in math.BigInt class which would allow us to handle arbitrarily large integers.
      *
      */
    def pow_of_2(x: Int): Int = {
        require(x >= 0, "x must be >= 0")
        if (x == 0) {
            return 1
        }

        pow_of_2(x - 1) + pow_of_2(x - 1)
    }
}
