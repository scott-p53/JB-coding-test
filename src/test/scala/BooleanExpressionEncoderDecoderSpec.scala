import org.scalatest.FlatSpec

//TODO: add more complex encoding/decoding tests and check for emptry string decodings and documentation
class BooleanExpressionEncoderDecoderSpec extends FlatSpec{
    "A BooleanExpressionEncoderDecoder" should "encode BooleanExpression objects to valid JSON" in {
        val encoderDecoder = new BooleanExpressionEncoderDecoder

        val andExp = And(True, False)
        var encoding = encoderDecoder.booleanEncode(andExp)
        assert(encoding === "{\"And\":{\"e1\":{\"True\":{}},\"e2\":{\"False\":{}}}}")

        val orExp = Or(True, True)
        encoding = encoderDecoder.booleanEncode(orExp)
        assert(encoding === "{\"Or\":{\"e1\":{\"True\":{}},\"e2\":{\"True\":{}}}}")

        val notExp = Not(False)
        encoding = encoderDecoder.booleanEncode(notExp)
        assert(encoding === "{\"Not\":{\"e\":{\"False\":{}}}}")

        val variableExp = Variable("Hello Test")
        encoding = encoderDecoder.booleanEncode(variableExp)
        assert(encoding === "{\"Variable\":{\"symbol\":\"Hello Test\"}}")
    }

    it should "decode a BooleanExpression JSON String into the proper object" in {
        val encoderDecoder = new BooleanExpressionEncoderDecoder


        val jsonAnd = "{\"And\":{\"e1\":{\"True\":{}},\"e2\":{\"False\":{}}}}"
        val expectedAnd = Some(And(True, False))
        var decoding = encoderDecoder.booleanDecode(jsonAnd)
        assert(decoding === expectedAnd)

        val jsonOr = "{\"Or\":{\"e1\":{\"True\":{}},\"e2\":{\"True\":{}}}}"
        val expectedOr = Some(Or(True, True))
        decoding = encoderDecoder.booleanDecode(jsonOr)
        assert(decoding === expectedOr)

        val jsonNot = "{\"Not\":{\"e\":{\"False\":{}}}}"
        val expectedNot = Some(Not(False))
        decoding = encoderDecoder.booleanDecode(jsonNot)
        assert(decoding === expectedNot)

        val jsonVariable = "{\"Variable\":{\"symbol\":\"Hello Test\"}}"
        val expectedVariable = Some(Variable("Hello Test"))
        decoding = encoderDecoder.booleanDecode(jsonVariable)
        assert(decoding === expectedVariable)
    }

    it should "throw an error when trying to decode invalid JSON" in {
        val encoderDecoder = new BooleanExpressionEncoderDecoder


        val missingCurlyBrace = "{\"And\":{\"e1\":{\"True\":{}},\"e2\":{\"False\":{}}}"
        var decoding = encoderDecoder.booleanDecode(missingCurlyBrace)
        assert(decoding === None)

        val incorrectVariableName = "{\"And\":{\"badName\":{\"True\":{}},\"e2\":{\"False\":{}}}"
        decoding = encoderDecoder.booleanDecode(incorrectVariableName)
        assert(decoding === None)

        val differentJSONObject = "[\"A\",\"B\",\"C\",\"D\"]"
        decoding = encoderDecoder.booleanDecode(differentJSONObject)
        assert(decoding === None)
    }

    it should "handle null inputs" in {
        val encoderDecoder = new BooleanExpressionEncoderDecoder

        //test null object for encoding
        val encoding = encoderDecoder.booleanEncode(null)
        assert(encoding === "")

        //test null string for decoding
        val decoding = encoderDecoder.booleanDecode(null)
        assert(decoding === None)

    }
}
