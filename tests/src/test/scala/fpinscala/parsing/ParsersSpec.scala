package fpinscala.parsing

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ParsersSpec extends Specification {

  val P: Parsers[String]
  import P._

  "The parser laws  should be correct" in {
    run(or(string("abra"),string("cadabra")))("abra") == Right("abra")
  }

}