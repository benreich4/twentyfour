package twentyfour

sealed trait Operator { def exec(a: Rational, b: Rational): Rational }
case object Plus extends Operator { override def exec(a: Rational, b: Rational) = a + b; override def toString = "+" }
case object Times extends Operator { override def exec(a: Rational, b: Rational) = a * b; override def toString = "*" }
case object Minus extends Operator { override def exec(a: Rational, b: Rational) = a - b; override def toString = "-" }
case object DividedBy extends Operator { override def exec(a: Rational, b: Rational) = a / b; override def toString = "/" }
case object Operator { val all = Seq(Plus, Times, Minus, DividedBy) }

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