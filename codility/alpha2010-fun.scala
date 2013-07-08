import scala.annotation.tailrec

object Solution {
    def solution(A: Array[Int]): Int = {
    	@tailrec
    	def solutionHelper(A: Array[Int], S: Set[Int], i: Int): Int = {
    		if (A.isEmpty) return i;

    		if (!S(A.head)) {
    			solutionHelper(A.tail, S + A.head, i+1)
    		}
    		else {
    			solutionHelper(A.tail, S, i);
    		}
    	}
    	return solutionHelper(A, Set(), 0) - 1;
    }
}

println(Solution.solution(Array(1, 2, 3, 1, 2, 3)))