import scala.collection.immutable.TreeSet

object Solution {
	def solution(A: Array[Int]): Int = {
		//return naiveSolution(A)
		return smartSolution(A)
	}

	// Adapted from http://stackoverflow.com/questions/14042447/counting-disk-intersections-using-treeset
	def smartSolution(A: Array[Int]): Int = {
		
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



	// Compare each circle to eachother to determine overlaps
	// Runs in O(n^2)
	def naiveSolution(A: Array[Int]): Int = {
		// Check if i's left edge overlap's j's right, or vice versa
		def intersect(A: Array[Int], i: Int, j: Int): Boolean = {
			if (i == j) return false;
			else if (i < j && i + A(i) >= j - A(j)) return true
			else if (i > j && j + A(j) >= i - A(i)) return true
			return false
		}		

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