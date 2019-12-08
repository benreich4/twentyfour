package twentyfour

case class Board(solution: Int)(nums: Int*) {
	def solutions = {
		val numCombos = nums.map(Fraction(_)).permutations.toList
		val allOperators = Seq.fill(nums.length - 1)(Operator.all).flatten
		val operatorCombos = allOperators.combinations(nums.length - 1).flatMap(_.permutations.toList).toList
		val operatorOrders = (1 until nums.length).permutations.toList

		val tries = for { 
			nums <- numCombos
			operators <- operatorCombos
			operatorOrder <- operatorOrders
		} yield Expression(nums, operators, operatorOrder) 

		tries.iterator.filter(_.value == Rational(solution))
	}

	def solvable = solutions.nonEmpty
}

case object Board {
	private def allBoards(solution: Int, n: Int) = Seq.fill(n)(1 to 13).flatten.combinations(n).map(nums => Board(solution)(nums:_*))

	def noSolutions(solution: Int, n: Int) = allBoards(solution, n).filterNot(_.solvable)
}


object SolutionRate {
	def calc = {
		(1 to 50).foreach { solution =>
			val len = Board.noSolutions(solution, 4).length
			println(s"Number of unsolvable boards for $solution: $len")
		}
	}
}