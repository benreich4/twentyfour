package twentyfour

case class Board(nums: Int*) {
	def solve = {
		val numCombos = nums.map(Fraction(_)).permutations.toList
		val allOperators = Seq.fill(nums.length - 1)(Operator.all).flatten
		val operatorCombos = allOperators.combinations(nums.length - 1).flatMap(_.permutations.toList).toList
		val operatorOrders = (1 until nums.length).permutations.toList

		val tries = for { 
			nums <- numCombos
			operators <- operatorCombos
			operatorOrder <- operatorOrders
		} yield Expression(nums, operators, operatorOrder) 

		tries.filter(_.value == Rational(24))
	}
}

case object Board {
	private def allBoards(n: Int) = Seq.fill(n)(1 to 13).flatten.combinations(4).map(nums => Board(nums:_*))

	def noSolutions(n: Int) = allBoards(n).filter(_.solve.isEmpty)
}