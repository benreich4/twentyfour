package twentyfour

case class Board(a: Int, b: Int, c: Int, d: Int) {
	def solve = {
		val numCombos = Seq(Rational(a), Rational(b), Rational(c), Rational(d)).permutations.toList
		val operatorCombos = (for { i <- Operator.all; j <- Operator.all; k <- Operator.all } yield Seq(i, j, k)).toList
		val operatorOrders = (1 to 3).permutations.toList

		val tries = for { 
			nums <- numCombos.toList
			operators <- operatorCombos
			operatorOrder <- operatorOrders
		} yield Expression(nums, operators, operatorOrder) 

		tries.filter(_.value == Rational(24))
	}
}

case object Board {
	private val allBoards = {
		val nums = (1 to 13)
		for { i <- nums; j <- nums; k <- nums; l <- nums } yield Board(i, j, k, l)
	}

	def noSolutions = allBoards.filter(_.solve.isEmpty)
}