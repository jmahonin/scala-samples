object qsorter {

  val aList = List(10, 3, 25, 29, 8, 7, 42, 11, 1, 19, 30, 6, 33)

  def qsort(list: List[Int]): List[Int] = list match {
    // Base case, return empty list
    case Nil => Nil

    // Have a list with > 0 elements
    case x :: xs =>

      // Naive partitioning based on 1st element
      val (start, end) = xs partition (_ < x)

      // Sort the first half, appended to partition, appended to second half
      qsort(start) ++ (x :: qsort(end))
  }

  val sorted = qsort(aList)
}

println(qsorter.sorted)
