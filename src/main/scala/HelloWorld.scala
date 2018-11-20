import io.circe.generic.auto._, io.circe.parser._, io.circe.syntax._

object HelloWorld {
    sealed trait Foo
    case class Bar(xs: Vector[String]) extends Foo
    case class Qux(i: Int, d: Option[Double]) extends Foo

    def main(args: Array[String]): Unit = {
    }
}