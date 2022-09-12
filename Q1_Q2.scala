case class RationalNumber(p: Int, q: Int){
    if(q == 0) throw new IllegalArgumentException("Denominator cannot be zero")
    
    def this(p: Int) = this(p, 1)

    private def GCD(a: Int, b: Int): Int = if (b == 0) a.abs else GCD(b, (a % b))
    
    private def gcd = GCD(q, p)
    private def numerator = p / gcd
    private def denominator = q / gcd
    
    def -(that: RationalNumber) = RationalNumber(numerator * that.denominator - that.numerator * denominator, denominator * that.denominator)
    override def toString = s"$numerator/$denominator"
    
}

object MyApp extends App{
        val x = new RationalNumber(3, 4)
        val y = new RationalNumber(5, 8)
        val z = new RationalNumber(2, 7)

        println(x - y - z)
}