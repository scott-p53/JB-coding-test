import org.scalatest.FlatSpec

class AlgebraicTransformerSpec extends FlatSpec{
    "A AlgebraicTransformer" should "be able to convert any algebraic expression into DNF form" in {
        val transformer = new AlgebraicTransformer

        var andExp = And(Variable("A"), Variable("B"))
        assert(transformer.DNFTransform(andExp) === And(Variable("A"), Variable("B")))

        val orExp = Or(Variable("A"), Variable("B"))
        assert(transformer.DNFTransform(orExp) === Or(Variable("A"), Variable("B")))

        val negativeExp = Not(Variable("A"))
        assert(transformer.DNFTransform(negativeExp) === Not(Variable("A")))

        val doubleNegationExp = Not(Not(Variable("A")))
        assert(transformer.DNFTransform(doubleNegationExp) === Variable("A"))

//        val nestedAndExp = And(And(And(True, True), And(False, False)), And(True, True))
//        assert(transformer.DNFTransform(doubleNegationExp) === Not(Variable("A")))


        val complexExpression = Not(And(And(Variable("A"), Variable("B")), Or(Variable("A"), Variable("B"))))
        assert(transformer.DNFTransform(complexExpression) ===
            Or(And(Variable("A"), Not(Variable("B"))), And(Not(Variable("A")), Variable("B"))))
    }

}
