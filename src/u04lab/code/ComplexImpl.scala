package u04lab.code

case class ComplexImpl(re:Double, im:Double) extends Complex {
  override def +(c: Complex): Complex = ComplexImpl(c.re + re, c.im + im)

  override def *(c: Complex): Complex = ComplexImpl(re*c.re-im*c.im, re*c.im+im*c.re)
}
