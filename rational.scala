package twentyfour

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