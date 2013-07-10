import scala.collection.immutable.TreeSet

// Inspired from
// http://stackoverflow.com/questions/4801242/algorithm-to-calculate-number-of-intersecting-discs
object Solution {
	def solution(A: Array[Int]): Int = {
		class Marker(val i: Int, val left: Boolean) extends Ordered[Marker] {
			def compare(c: Marker): Int = {
				if (i < c.i) return - 1
				else if (i > c.i) return 1
				else if (left) return -1
				else return 1
			}
			override def toString = "Val: " + i + " Left: " + left
		}

 		// Create a list of Markers from array 'A'
 		// O(n) time
 		val markers = A.take(100000).filter(_.isValidInt).zipWithIndex.flatMap ( x => x match { case (r, i) => List(new Marker(i - r, true), new Marker(i + r, false)) } ).toList
 		
 		// Add each circle to a TreeSet (sorted)
 		// O(n log(n))
 		val emptySet: TreeSet[Marker] = TreeSet[Marker]()
 		//val markerSet = emptySet ++ markers
 		val markerSet: TreeSet[Marker] = TreeSet[Marker]() ++ markers

 		// Loop through each circle and count overlaps
 		// O(n)
 		var total = -1
 		var overlaps = 0

 		for(i <- markerSet) {
 			if (i.left) {
 				total += 1
 				if (total > 0) overlaps += total;
 				if (overlaps > 10000000) return -1
 			}
 			else {
 				total -= 1
 			}
 		}

 		return overlaps
	}
}

val foo = Array(1, 5, 2, 1, 4, 0);
println(Solution.solution(foo))