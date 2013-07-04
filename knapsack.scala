object knapsack {
 
 	val capacity = 45

 	// Items list of (value, weight)
	val items = List(	(4.0,3.0), (4.0,3.0), (4.0,3.0), (4.0,3.0), (4.0,3.0),
						(5.0,4.0), (5.0,4.0), (5.0,4.0), (10.0,7.0), (10.0,7.0),
						(11.0,8.0),	(11.0,8.0), (13.0,9.0) )
  
  // Brute force algorithm: Take every possible combination, filter by 
  // combinations that satisfy the capacity limit
  val combs = for {
  	count <- 1 to items.length
  	comb <- items combinations(count)
  	if ((comb.unzip _2).sum <= capacity)
  } yield comb

  // Now sort the satisfactory list by those with the highest value
  val sorted = combs.sortWith( (x, y) => (x.unzip _1).sum > (y.unzip _1).sum )

  /*
  sorted(0)		//> res0: List[(Double, Double)] = List((4.0,3.0), (4.0,3.0), (10.0,7.0), (10.0,
                //| 7.0), (11.0,8.0), (11.0,8.0), (13.0,9.0)
  /*

 }