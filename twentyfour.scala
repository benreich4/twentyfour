sealed trait Rational {
	def +(that: Rational): Rational = (this, that) match {
		case (NaN, _) => NaN
		case (_, NaN) => NaN
		case (a: Fraction, b: Fraction) => Fraction(a.num * b.den + b.num * a.den, a.den * b.den)
	}
	def -(that: Rational): Rational = (this, that) match {
		case (NaN, _) => NaN
		case (_, NaN) => NaN
		case (a: Fraction, b: Fraction) => Fraction(a.num * b.den - b.num * a.den, a.den * b.den)
	}
	def *(that: Rational): Rational = (this, that) match {
		case (NaN, _) => NaN
		case (_, NaN) => NaN
		case (a: Fraction, b: Fraction) => Fraction(a.num * b.num, a.den * b.den)
	}
	def /(that: Rational): Rational = (this, that) match {
		case (NaN, _) => NaN
		case (_, NaN) => NaN
		case (a: Fraction, b: Fraction)=> if(b.num == 0) NaN else Fraction(a.num * b.den, a.den * b.num)
	}
}

object Rational { def apply(num: Int) = Fraction(num) }

case object NaN extends Rational { override def toString = "NaN" }

case class Fraction(num: Int, den: Int = 1) extends Rational {
	override def toString = if(den == 1) num.toString else s"$num/$den"

	override def equals(that: Any) = that match {
		case that: Fraction => this.num * that.den == that.num * this.den 
		case _ => false
	}
}

sealed trait Operator { def exec(a: Rational, b: Rational): Rational }
case object Plus extends Operator { override def exec(a: Rational, b: Rational) = a + b; override def toString = "+" }
case object Times extends Operator { override def exec(a: Rational, b: Rational) = a * b; override def toString = "*" }
case object Minus extends Operator { override def exec(a: Rational, b: Rational) = a - b; override def toString = "-" }
case object DividedBy extends Operator { override def exec(a: Rational, b: Rational) = a / b; override def toString = "/" }
case object Operator { val all = Seq(Plus, Times, Minus, DividedBy) }

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