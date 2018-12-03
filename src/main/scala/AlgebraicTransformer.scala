/**
  * A class to perform algebraic transformations on BooleanExpressions. The result of the transformations are
  * BooleanExpressions. Currently the class can only handle simplifying to disjunctive normal form.
  */
class AlgebraicTransformer {
    /**
      *
      * @param exp A BoolExpressionObject to simplify into disjunctive normal form
      * @return a simplified expression in disjunctive normal form. This simplified form doesn't eliminate redundant
      *         variables or expressions such as And(True, True) to further simplify the expression.
      *
      * The algorithm reducing a boolean expression into DNF form is to push down the negations onto the leaf nodes
      * and then distributing Or expressions over And expressions. This pushes the And expressions to be closer to
      * the leaf nodes of the tree and therefore pushes the Or expressions higher in the tree.
      */
    def DNFTransform(exp: BooleanExpression): BooleanExpression = {
        transform(exp)
    }

    //Assumes to perform DNF transformation by distributing Or's over And's
    private def transform(exp: BooleanExpression): BooleanExpression = {
        val negationNormalForm = pushDownNegations(exp)
        distribute(negationNormalForm)
    }

    //Push Not nodes down to the leaf nodes of the expression tree
    private def pushDownNegations(exp: BooleanExpression): BooleanExpression = {
        exp match {
            //variables. True, and False values are all leaf nodes so there is nothing further to push down
            case x:Variable => x
            case x @ (True | False) => x
            case Not(e) => e match {
                case x:Variable => Not(x)
                //flip true and false cases when in a Not expression to simplify a bit
                case True => False
                case False => True
                case Not(innerExp) => pushDownNegations(innerExp)
                case And(e1, e2) => Or(pushDownNegations(Not(e1)), pushDownNegations(Not(e2)))
                case Or(e1, e2) =>  And(Not(pushDownNegations(e1)), Not(pushDownNegations(e2)))
            }
            case And(e1, e2) => And(pushDownNegations(e1), pushDownNegations(e2))
            case Or(e1, e2) => Or(pushDownNegations(e1), pushDownNegations(e2))
        }
    }

    //Assumes to perform DNF distribute by distributing Or's over And's
    private def distribute(exp: BooleanExpression): BooleanExpression = {
        exp match {
            case x: Variable => x
            case Or(e1, e2) => Or(DNFTransform(e1), DNFTransform(e2))
            //Expand distribute and over or by taking e1 and applying And to the left hand side of the or expression
            // and then taking e1 and applying And to the right hand side of the or expression
            case And(e1, e2:Or) =>  Or(And(e1, e2.e1), And(e1, e2.e2))
            case And(e1, e2) => And(distribute(e1), distribute(e2))
            case _ => exp
        }
    }
}
