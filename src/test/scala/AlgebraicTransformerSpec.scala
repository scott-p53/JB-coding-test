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

        val nestedAndExp = And(And(And(True, True), And(False, False)), And(True, True))
        assert(transformer.DNFTransform(nestedAndExp) ===
            And(And(And(True, True), And(False, False)), And(True, True)))

        val complexExpression = Not(And(And(Variable("A"), Variable("B")), Or(Variable("A"), Variable("B"))))
        assert(transformer.DNFTransform(complexExpression) ===
            Or(Or(Not(Variable("A")), Not(Variable("B"))), And(Not(Variable("A")), Not(Variable("B")))))

        val simplifyNegationsExp = And(Not(True), Not(False))
        assert(transformer.DNFTransform(simplifyNegationsExp) === And(False, True))

        val rootNotExpression = Not(Or(Or(Variable("A"), Variable("B")), Variable("A")))
        assert(transformer.DNFTransform(rootNotExpression) == And(And(Not(Variable("A")), Not(Variable("B"))), Not
        (Variable("A"))))
    }

}
