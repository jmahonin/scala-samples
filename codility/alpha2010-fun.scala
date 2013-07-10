import scala.annotation.tailrec

object Solution {
    def solution(A: Array[Int]): Int = {
        // Recursive, functional solution
    	@tailrec
    	def solutionHelper(A: Array[Int], S: Set[Int], i: Int, prefix: Int): Int = {
    		// Base case, return last recorded prefix
            if (A.isEmpty) return prefix;

    		// Never seen the element, add to list, increment index and prefix, recurse
            if (!S(A.head)) {
    			solutionHelper(A.tail, S + A.head, i+1, i+1)
    		}
    		// Seen this element, increment index and recurse
            else {
    			solutionHelper(A.tail, S, i+1, prefix);
    		}
    	}
    	// Start off with an empty set, and initial seed/prefix values
        return solutionHelper(A, Set(), -1, -1);
    }
}

println(Solution.solution(Array(1,2,3,1,2,3)))