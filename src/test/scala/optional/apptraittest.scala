package optional

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.matchers.{ Matcher, MatchResult }

import scala.reflect.runtime.universe._
import scala.reflect.runtime.currentMirror

class ApplicationTraitTestSuite extends FunSuite with ShouldMatchers {
  trait TestApp extends optional.Application {
    protected override def catchUsageError = false
  }
  
  
  class Test1(ev1: Int = 1, ev2: String = "2") extends TestApp {
    def main(pos1: Int, pos2: String) {
      pos1 should be (1)
      pos2 should be ("2")
    }
  }
  test("positional agruments only") {
    val Test1 = new Test1(1, "2")
    val args = Array("1", "2")
    Test1.main(args)
  }
  test("insuffient positional arguments") {
    val Test1 = new Test1(1, "2")
    val args = Array("1")
    val thrown = evaluating {
      Test1.main(args)
    } should produce [UsageError]
    thrown.msg should be ("Received 1 positional arguments and expected 2")
  }
  test("malformed positional argument") {
    val Test1 = new Test1(1, "2")
    val args = Array("one", "2")
    val thrown = evaluating {
      Test1.main(args)
    } should produce [UsageError]
    thrown.msg should be ("Malformed argument pos1: the value \"one\" could not be converted into a Int")
  }
  test("test program name") {
    val Test1 = new Test1(1, "2")
    Test1.programName should be ("Test1")
  }
  
  def checkUsageMessageHeader(expected: String, app: TestApp) {
    val mm = Application.findMainMethod(app)
    val (posArgs, namedArgs) = MainArg.splitArgs(Application.extractArgs(mm.symbol))
    val r = app.usageMessageHeader(posArgs, namedArgs)
    r should be (expected)
  }
  
  test("test usage message header with just positional arguments") {
    checkUsageMessageHeader("Test1: <pos1: Int> <pos2: String>", new Test1)
  }
  
  def checkUsageMessage(expected: String, app: TestApp) {
    val mm = Application.findMainMethod(app)
    val args = Application.extractArgs(mm.symbol)
    val r = app.usageMessage(args)
    r should be (expected)
  }
  
  test("usage message with just positional arguments") {
    checkUsageMessage("Test1: <pos1: Int> <pos2: String>", new Test1)
  }
  
  class Test2(ev1: Option[Int] = None, ev2: Option[String] = None) extends TestApp {
    def main(opt1: Option[Int], opt2: Option[String]) {
      opt1 should be (ev1)
      opt2 should be (ev2)
    }
  }
  test("option arguments only w/o defined defaults and all options specified") {
    val Test2 = new Test2(Some(1), Some("2"))
    val args = Array("-opt1", "1", "-opt2", "2")
    Test2.main(args)
  }
  test("option arguments w/o defaults and no options specified") {
    val Test2 = new Test2(None, None)
    val args = Array[String]()
    Test2.main(args)
  }
  test("option arguments w/o defaults, one specified, one not") {
    val Test2 = new Test2(None, Some("2"))
    val args = Array("-opt2", "2")
    Test2.main(args)
  }
  test("usage message header for just Option arguments") {
    checkUsageMessageHeader("Test2: [options]", new Test2)
  }
  test("malformed option argument") {
    val Test2 = new Test2
    val args = Array("-opt1", "blah", "-opt2", "2")
    val thrown = evaluating {
      Test2.main(args)
    } should produce [UsageError]
    thrown.msg should be ("Malformed argument opt1: the value \"blah\" could not be converted into a Int")
  }
  test("int option argument missing value") {
    val Test2 = new Test2
    val args = Array("-opt1", "-opt2", "2")
    val thrown = evaluating {
      Test2.main(args)
    } should produce [UsageError]
    thrown.msg should be ("Missing argument for option: opt1")
  }
  test("string option argument missing value") {
    val Test2 = new Test2
    val args = Array("-opt1", "1", "-opt2")
    val thrown = evaluating {
      Test2.main(args)
    } should produce [UsageError]
    thrown.msg should be ("Missing argument for option: opt2")
  }
  
  def checkUsageMsg(app: TestApp, header: String, tableHeader: Option[String], rows: Seq[String]) {
    val expected = header + tableHeader.map(v => "\n" + v + "\n").getOrElse("") + rows.mkString("\n")
    val mm = Application.findMainMethod(app)
    val args = Application.extractArgs(mm.symbol)
    val r = app.usageMessage(args)
    r should be (expected)
  }
  
  test("usage message for just Option arguments") {
    val header = "Test2: [options]"
    val namedTableHeader = "Name    Type      Default Value    Usage"
    val row1 = "opt1    Int                        -opt1 <Int>"
    val row2 = "opt2    String                     -opt2 <String>"
    val Test2 = new Test2
    checkUsageMsg(Test2, header, Some(namedTableHeader), List(row1, row2))
  }
  
  class Test3(ev1: Int = 5, ev2: String = "hello") extends TestApp {
    def main(p1: Int = ev1, p2: String = ev2) {
      p1 should be (ev1)
      p2 should be (ev2)
    }
  }
  test("default arguments with all present") {
    val Test3 = new Test3(1, "2")
    val args = Array("-p1", "1", "-p2", "2")
    Test3.main(args)
  }
  test("default arguments with none present") {
    val Test3 = new Test3(5, "hello")
    val args = Array[String]()
    Test3.main(args)
  }
  test("default arguments with one present, one missing") {
    val Test3 = new Test3(5, "two")
    val args = Array("-p2", "two")
    Test3.main(args)
  }
  test("usage message header for just arguments with defaults") {
    checkUsageMessageHeader("Test3: [options]", new Test3)
  }
  test("usage message for default arguments") {
    val header = "Test3: [options]"
    val th = "Name    Type      Default Value    Usage"
    val r1 = "p1      Int       5                -p1 <Int>"
    val r2 = "p2      String    hello            -p2 <String>"
    checkUsageMsg(new Test3, header, Some(th), List(r1, r2))
  }
  
  class Test4(ev1: Boolean = false, ev2: Boolean = false) extends TestApp {
    def main(p1: Boolean, p2: Boolean) {
      p1 should be (ev1)
      p2 should be (ev2)
    }
  }
  test("boolean flags with both present") {
    val Test4 = new Test4(true, true)
    val args = Array("-p1", "-p2")
    Test4.main(args)
  }
  test("boolean flags with both missing") {
    val Test4 = new Test4(false, false)
    val args = Array[String]()
    Test4.main(args)
  }
  test("boolean flags with one missing, one present") {
    val Test4 = new Test4(false, true)
    val args = Array[String]("-p2")
    Test4.main(args)
  }
  test("usage message header for just boolean arguments") {
    checkUsageMessageHeader("Test4: [options]", new Test4)
  }
  test("usage message for just boolean arguments") {
    val mh = "Test4: [options]"
    val th = "Name    Type       Default Value    Usage"
    val r1 = "p1      Boolean                     -p1"
    val r2 = "p2      Boolean                     -p2"
    checkUsageMsg(new Test4, mh, Some(th), List(r1, r2))
  }
  
  class Test5(ev1: Option[Boolean] = None) extends TestApp {
    def main(p1: Option[Boolean]) {
      p1 should be (ev1)
    }
  }
  test("option arg with a type of boolean with arg specified") {
    val Test5 = new Test5(Some(false))
    val args = Array("-p1", "false")
    Test5.main(args)
  }
  test("option arg with type of boolean with arg unspecified") {
    val Test5 = new Test5(None)
    val args = Array[String]()
    Test5.main(args)
  }
  test("usage message for optional boolean argument") {
    val h  = "Test5: [options]"
    val th = "Name    Type       Default Value    Usage"
    val r1 = "p1      Boolean                     -p1 <Boolean>"
    checkUsageMsg(new Test5, h, Some(th), List(r1))
  }
  
  class Test6(ev1: Int = 1, ev2: Boolean = false) extends TestApp {
    def main(pos1: Int, b1: Boolean) {
      pos1 should be (ev1)
      b1 should be (ev2)
    }
  }
  test("usage header with one positional and one boolean") {
    checkUsageMessageHeader("Test6: [options] <pos1: Int>", new Test6)
  }
  test("usage message with one positional and one boolean") {
    val mh = "Test6: [options] <pos1: Int>"
    val th = "Name    Type       Default Value    Usage"
    val r1 = "b1      Boolean                     -b1"
    checkUsageMsg(new Test6, mh, Some(th), List(r1))
  }
  
  class Test7(ev1: String = "hello") extends TestApp {
    def main(anArgumentWithAReallyLongName: String = ev1) {
      anArgumentWithAReallyLongName should be (ev1)
    }
  }
  test("usage message with long argument name") {
    val mh = "Test7: [options]"
    val th = "Name                             Type      Default Value    Usage"
    val r1 = "anArgumentWithAReallyLongName    String    hello            -anArgumentWithAReallyLongName <String>"
    checkUsageMsg(new Test7, mh, Some(th), List(r1))
  }
  
  class Test8(ev1: String = "A Really Long Default Value") extends TestApp {
    def main(arg: String = ev1) {
      arg should be (ev1)
    }
  }
  test("usage message with a long default value") {
    val mh = "Test8: [options]"
    val th = "Name    Type      Default Value                  Usage"
    val r1 = "arg     String    A Really Long Default Value    -arg <String>"
    checkUsageMsg(new Test8, mh, Some(th), List(r1))
  }
  
  case class AnArgumentTypeWithALongName(s: String)
  class Test9(ev1: Option[AnArgumentTypeWithALongName] = None) extends TestApp {
    def main(arg: Option[AnArgumentTypeWithALongName]) {
      arg should be (ev1)
    }
  }
  test("usage message with a long argument type name") {
    val mh = "Test9: [options]"
    val th = "Name    Type                           Default Value    Usage"
    val r1 = "arg     AnArgumentTypeWithALongName                     -arg <AnArgumentTypeWithALongName>"
    checkUsageMsg(new Test9, mh, Some(th), List(r1))
  }
  
  class <=>(ev: String) extends TestApp {
    def main(arg: String) {
      arg should be (ev)
    }
  }
  test("usage message header on class with symbolic name") {
    checkUsageMessageHeader("<=>: <arg: String>", new <=>("hello"))
  }
}