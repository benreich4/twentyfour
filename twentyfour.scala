case class Rational(num: Int, den: Int = 1) {
	override def toString = if(den == 1) num.toString else s"$num/$den"
	def +(that: Rational) = Rational(this.num * that.den + that.num * this.den, this.den * that.den)
	def *(that: Rational) = Rational(this.num * that.num, this.den * that.den)
	def -(that: Rational) = this + that.neg
	def /(that: Rational) = this * that.recip

	def neg = Rational(-1 * num, den)
	def recip = Rational(den, num)

	override def equals(that: Any) = that match {
		case that: Rational => this.num * that.den == that.num * this.den 
		case _ => false
	}
}

sealed trait Operator { def exec(a: Rational, b: Rational): Rational }
case object Plus extends Operator { override def exec(a: Rational, b: Rational) = a + b; override def toString = "+" }
case object Times extends Operator { override def exec(a: Rational, b: Rational) = a * b; override def toString = "*" }
case object Minus extends Operator { override def exec(a: Rational, b: Rational) = a - b; override def toString = "-" }
case object DividedBy extends Operator { override def exec(a: Rational, b: Rational) = a / b; override def toString = "/" }
case object Operator {
	val all = Seq(Plus, Times, Minus, DividedBy)
}

case class Board(a: Int, b: Int, c: Int, d: Int) {
	def solve = {
		val numCombos = Seq(Rational(a), Rational(b), Rational(c), Rational(d)).permutations.toList
		val operatorCombos = (for { i <- Operator.all; j <- Operator.all; k <- Operator.all } yield Seq(i, j, k)).toList
		val operatorOrders = (1 to 3).permutations.toList

		val tries = for { 
			nums <- numCombos.toList
			operators <- operatorCombos.toList
			operatorOrder <- operatorOrders.toList
		} yield Expression(nums, operators, operatorOrder) 

		tries.filter(_.value == Rational(24))
	}
}

case object Board {
	def noSolutions = {
		val nums = (1 to 13)
		val allBoards = for { i <- nums; j <- nums; k <- nums; l <- nums } yield Board(i, j, k, l)

		allBoards.zipWithIndex.map { case (b, i) => 
			if (i % 1000 == 0) println(i)
			b
		}.filter(_.solve.isEmpty)
	}
}

case class Expression(nums: Seq[Rational], operators: Seq[Operator], operatorOrder: Seq[Int]) {
	case class SolvePart(value: Rational, operator: Option[Operator], operatorOrder: Option[Int])

	@scala.annotation.tailrec
	private def solve(partials: Seq[SolvePart], rep: Int): Rational = {
		if (partials.length == 1) partials.head.value else {
			val (before, after) = partials.span(_.operatorOrder.get != rep)
			val hd :: next :: tl = after
			val newValue = hd.operator.get.exec(hd.value, next.value)
			val newSolvePart = SolvePart(newValue, next.operator, next.operatorOrder)
			val newAfter = newSolvePart :: tl
			solve(before ++ newAfter, rep + 1)
		}
	}

	def value = {
		val initialPartials = nums.zip(operators.map(Some(_)):+None).zip(operatorOrder.map(Some(_)):+None).map { case ((a, b), c) => SolvePart(a, b, c) }
		solve(initialPartials, 1)
	}

	case class StringifyPart(value: String, operator: Option[Operator], operatorOrder: Option[Int])

	@scala.annotation.tailrec
	private def stringify(partials: Seq[StringifyPart], rep: Int): String = {
		if (partials.length == 1) partials.head.value else {
			val (before, after) = partials.span(_.operatorOrder.get != rep)
			val hd :: next :: tl = after
			val newValue = s"(${hd.value} ${hd.operator.get} ${next.value})"
			val newSolvePart = StringifyPart(newValue, next.operator, next.operatorOrder)
			val newAfter = newSolvePart :: tl
			stringify(before ++ newAfter, rep + 1)
		}
	}

	override def toString = {
		val initialPartials = nums.zip(operators.map(Some(_)):+None).zip(operatorOrder.map(Some(_)):+None).map { case ((a, b), c) => StringifyPart(a.toString, b, c) }
		stringify(initialPartials, 1)
	}
}