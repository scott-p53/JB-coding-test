import io.circe.generic.auto._, io.circe.parser._, io.circe.syntax._

/**
  * Uses the circe library to encode and decode BooleanExpression objects into and out of JSON format.
  */
class BooleanExpressionEncoderDecoder {
    /**
      *
      * @param booleanExp BooleanExpressionObject
      * @return a JSON encoded BooleanExpressionObject or an emptry string if the expression is null
      */
    def booleanEncode(booleanExp: BooleanExpression): String = {
        if (booleanExp == null) {
            System.err.println("BooleanExpression object is null!")
            return ""
        }

        booleanExp.asJson.noSpaces
    }

    /**
      *
      * @param json A JSON encoded string
      * @return The BooleanExpresssion object or None if the JSON string was invalid (null, invalid JSON string, or type
      *         mismatch between json object and BooleanExpression)
      */
    def booleanDecode(json: String): Option[BooleanExpression] = {
        decode[BooleanExpression](json) match {
            case Left(failure) =>
                System.err.println(s"Error decoding JSON: $json. Error $failure")
                None
            case Right(result) =>  Some(result)
        }
    }
}
