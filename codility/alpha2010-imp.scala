import scala.collection.mutable.Set
import scala.annotation.tailrec

object Solution {
    def solution(A: Array[Int]): Int = {
        val checkSet: Set[Int] = Set()
        var prefix = 0;
        for (i <- A.indices) {
        	if ( !checkSet.contains(A(i)) ) { checkSet += A(i); prefix = i; }
        }
        return prefix;
    }
}

println(Solution.solution(Array(1, 2, 3, 1, 2, 3)))

