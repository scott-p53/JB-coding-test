import scala.collection.mutable.ListBuffer

class AlgebraicTransformer {
    def DNFTransform(exp: BooleanExpression): BooleanExpression = {
        exp match {
            case x: Variable => x
            case Or(e1, e2) => Or(DNFTransform(e1), DNFTransform(e2))
            case And(e1, e2) => buildDNFAndExpression(e1, e2)
            case Not(e) => buildDNFNotExpression(e)
        }
    }

    private def expandExpression(exp: BooleanExpression, accumulator: ListBuffer[BooleanExpression]): Unit  = {
        exp match {
            case x: Variable => x +=: accumulator
            case And(e1, e2) =>
                expandExpression(e1, accumulator)
                expandExpression(e2, accumulator)
            case Or(e1, e2) =>
                expandExpression(e1, accumulator)
                expandExpression(e2, accumulator)
            case Not(e) => expandExpression(e, accumulator)
        }
    }

    private def buildDNFAndExpression(leftExp:  BooleanExpression, rightExp: BooleanExpression): BooleanExpression = {
        val leftExpList = ListBuffer[BooleanExpression]()
        val rightExpList = ListBuffer[BooleanExpression]()

        expandExpression(DNFTransform(leftExp), leftExpList)
        expandExpression(DNFTransform(rightExp), rightExpList)
        var resultExp: BooleanExpression = null
        for (leftExp <- leftExpList) {
            for (rightExp <- rightExpList) {
                if (resultExp == null) {
                    resultExp = And(leftExp, rightExp)
                } else {
                    resultExp = Or(resultExp, And(leftExp, rightExp))
                }
            }
        }

        resultExp
    }

    private def buildDNFNotExpression(exp: BooleanExpression): BooleanExpression = {
        exp match {
            case x: Variable => Not(x)
            case Not(p) => DNFTransform(p)
            case Or(e1, e2) => DNFTransform(And(Not(e1), Not(e2)))
            case And(e1, e2) => DNFTransform(Or(Not(e1), Not(e2)))
        }
    }
}
