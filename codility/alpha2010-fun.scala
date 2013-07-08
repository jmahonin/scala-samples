import scala.annotation.tailrec

object Solution {
    def solution(A: Array[Int]): Int = {
    	@tailrec
    	def solutionHelper(A: Array[Int], S: Set[Int], i: Int, prefix: Int): Int = {
    		if (A.isEmpty) return prefix;

    		if (!S(A.head)) {
    			solutionHelper(A.tail, S + A.head, i+1, i+1)
    		}
    		else {
    			solutionHelper(A.tail, S, i+1, prefix);
    		}
    	}
    	return solutionHelper(A, Set(), -1, -1);
    }
}

println(Solution.solution(Array(1,2,3,1,2,3)))
