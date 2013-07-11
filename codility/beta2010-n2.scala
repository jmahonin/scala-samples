object Solution {
  def solution(A: Array[Int]): Int = {
    // Check if i's left edge overlap's j's right, or vice versa
    def intersect(A: Array[Int], i: Int, j: Int): Boolean = {
      if (i == j) return false;
      else if (i < j && i + A(i) >= j - A(j)) return true
      else if (i > j && j + A(j) >= i - A(i)) return true
      return false
    }   

    // O(n^2) check for intersections
    val pairs = for {
      i <- 0 to A.length - 1
      j <- i to A.length - 1
      if (intersect(A, i, j))
    } yield (i, j)

    if (pairs.size > 10000000) return -1;
    return pairs.size
  }
}

val foo = Array(1, 5, 2, 1, 4, 0);
println(Solution.solution(foo))