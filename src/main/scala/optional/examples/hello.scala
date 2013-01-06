package optional.examples

import optional.ConverterRegistry

// A simple hello world with two optional arguments and one required argument.
// Try running it like so:
// 
//   scala optional.examples.Hello -times 2 .
//
object Hello extends optional.Application {
  
  override protected def registerCustomConversions(r: ConverterRegistry[String]) {
    r.register(s => new java.io.File(s))
  }
  
  def main(times: Option[Int], greeting: Option[String], file: java.io.File) {
    val _greeting = greeting getOrElse "hello"
    val _times = times getOrElse 1
    
    println(
      List.fill(_times)(_greeting).mkString(", ") + 
      " and %s does %sexist.".format(file.getAbsolutePath, (if (file.exists) "" else "not "))
    )
  }
}
