import org.scalatest.FlatSpec

/**
  * Test class to test BoolExpressionEncoderDecoderSpec. Each test example handles different cases
  */
class BooleanExpressionEncoderDecoderSpec extends FlatSpec{
    "A BooleanExpressionEncoderDecoder" should "encode BooleanExpression objects to valid JSON" in {
        val encoderDecoder = new BooleanExpressionEncoderDecoder

        val andExp = And(True, False)
        var encoding = encoderDecoder.booleanEncode(andExp)
        var expectedResult = Some("{\"And\":{\"e1\":{\"True\":{}},\"e2\":{\"False\":{}}}}")
        assert(encoding === expectedResult)

        val orExp = Or(True, True)
        encoding = encoderDecoder.booleanEncode(orExp)
        expectedResult = Some("{\"Or\":{\"e1\":{\"True\":{}},\"e2\":{\"True\":{}}}}")
        assert(encoding === expectedResult)

        val notExp = Not(False)
        encoding = encoderDecoder.booleanEncode(notExp)
        expectedResult = Some("{\"Not\":{\"e\":{\"False\":{}}}}")
        assert(encoding === expectedResult)

        val variableExp = Variable("Hello Test")
        encoding = encoderDecoder.booleanEncode(variableExp)
        expectedResult =  Some("{\"Variable\":{\"symbol\":\"Hello Test\"}}")
        assert(encoding === expectedResult)
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
        assert(encoding === None)

        //test null string for decoding
        val decoding = encoderDecoder.booleanDecode(null)
        assert(decoding === None)
    }

    it should "handle empty string inputs when attempting to decode" in {
        val encoderDecoder = new BooleanExpressionEncoderDecoder

        //test null string for decoding
        val decoding = encoderDecoder.booleanDecode("")
        assert(decoding === None)
    }
}
