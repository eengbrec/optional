package optional

import collection.mutable.HashSet

import scala.reflect.runtime.universe._
import scala.reflect.runtime.currentMirror

import org.apache.commons.{ cli => acli }

case class DesignError(msg: String) extends Error(msg)
case class UsageError(msg: String) extends RuntimeException(msg) {
  if (msg.startsWith("optional.UsageError")) throw new RuntimeException("boom")
}

import scala.util.{ Try, Success, Failure }

private class TypeExtractor(val t: Type) {
  def unapply(x: Any): Option[Type] = x match {
    case tpe: Type if tpe =:= t => Some(t)
    case _ => None
  }
}

private object BooleanType extends TypeExtractor(typeOf[Boolean])
private object StringType extends TypeExtractor(typeOf[String])
private object ArrayStringType extends TypeExtractor(typeOf[Array[String]])

object OptionType {
  def unapply(x: Any): Option[Type] = x match {
    case tpe: Type if tpe.erasure =:= typeOf[Option[Any]].erasure => {
      val arg = typeArgs(tpe).head
      Some(arg)
    }
    case _ => None
  }
  def typeArgs(tpe: Type) = tpe match {
    case TypeRef(_, _, args) => args
    case _ => Nil
  }
}

object MainArg {
  def apply(term: TermSymbol, index: Int): MainArg = {
    val tpe = term.typeSignature
    tpe match {
      case BooleanType(_) => BoolArg(term, index)
      case OptionType(_)  => OptionArg(term, index)
      case _ => if (term.isParamWithDefault) {
        ArgWithDefault(term, index)
      } else {
        PositionalArg(term, index)
      }
    }    
  }
  
  def splitArgs(args: Seq[MainArg]): (Seq[PositionalArg], Seq[NamedArg]) = {
    val pos = args.collect {
      case parg: PositionalArg => parg
    }
    val named = args.collect {
      case narg: NamedArg => narg
    }
    (pos, named)
  }
}

sealed trait MainArg {
  def term: TermSymbol
  final def name: String = term.name.decoded
  def tpe: Type
  final def typeName = tpe.typeSymbol.name.decoded
  def isOptional: Boolean
  def index: Int
  
  def =:=(other: MainArg): Boolean = name == other.name && 
                                     tpe =:= other.tpe &&
                                     isOptional == other.isOptional &&
                                     index == other.index
}

/**
 * An argument that can be specified by name
 */
sealed trait NamedArg extends MainArg {
  def cliOption: acli.Option
}

/**
 * An argument with a default value
 */
sealed trait DefaultArg extends NamedArg {
  def defaultValue(m: InstanceMirror): Any
}

/**
 * Definition for an optional argument
 * 
 * @name the name of the argument
 * @tpe the type of the argument without the wrapping Option, e.g. Int for Option[Int]
 * @originalType the type of the argument as defined on the main method
 */
case class OptionArg(term: TermSymbol, index: Int) extends DefaultArg {
  val tpe = term.typeSignature match {
    case OptionType(t) => t
  }
  def isOptional = true
  val cliOption = new acli.Option(name, true, "TBD")
  def defaultValue(m: InstanceMirror) = None
}

case class ArgWithDefault(term: TermSymbol, index: Int) extends DefaultArg {
  val tpe = term.typeSignature
  def isOptional = false
  
  val cliOption = new acli.Option(name, true, "")
  
  def defaultValue(m: InstanceMirror): Any = {
    val baseMethodSym = term.owner
    val methodName = "%s$default$%d".format(baseMethodSym.name.decoded, index + 1)
    val termName = newTermName(methodName)
    val methSym = m.symbol.toType.member(termName).asMethod
    val methMirror = m.reflectMethod(methSym)
    methMirror()
  }
}

case class PositionalArg(term: TermSymbol, index: Int) extends MainArg {
  val tpe = term.typeSignature
  def isOptional = false
}

case class BoolArg(term: TermSymbol, index: Int) extends DefaultArg {
  val tpe = typeOf[Boolean]
  def isOptional = true
  val cliOption = new acli.Option(name, false, "include if true, omit if false")
  
  def defaultValue(m: InstanceMirror) = false
}


object Application {
  
  def isEligibleMain(m: MethodSymbol) = m.name.decoded == "main" && !isRealMain(m)
  
  def isRealMain(m: MethodSymbol) = {
    (m.name.decoded == "main") && (m.paramss.length == 1) && (m.paramss.head.length == 1) && (m.paramss.head.head.typeSignature == typeOf[Array[String]])
  }
  
  def findMainMethod(t: Type): MethodSymbol = {
    val candidateMethods = t.members.collect {
      case m if m.isMethod => m.asMethod
    }.filter(isEligibleMain(_)).toList
    
    candidateMethods match {
      case Nil => throw DesignError("No valid main method found!")
      case method :: Nil => method
      case _ => throw DesignError("%d potential main methods found!".format(candidateMethods.length))
    }    
    
  }
  
  def findMainMethod(obj: AnyRef): MethodMirror = {
    val im = currentMirror.reflect(obj)
    val s = im.symbol
    val t = s.toType
    
    val method = findMainMethod(t)
    im.reflectMethod(method)
  }
  
  def extractArgs(meth: MethodSymbol) = meth.paramss.flatten.zipWithIndex.map { t =>
    val term = t._1.asTerm
    val pos = t._2
    MainArg(term, pos)
  }
  
  def signature(m: MethodSymbol): String = {
    m.name.decoded + m.paramss.flatMap { pl =>
      "(" + pl.flatMap { p => 
        p.name.decoded + ": " + p.typeSignature.toString
      } + ")"
    }
  }
  
  def makeRegistry = {
    val r = new ConverterRegistry[String]
    registerDefaultConversions(r)
    r
  }
  
  def registerDefaultConversions(r: ConverterRegistry[String]) {
	import ConverterRegistry._
    r.register(makeConverter(java.lang.Integer.parseInt _))
    r.register(makeConverter(java.lang.Double.parseDouble _))
    r.register(makeConverter(java.lang.Long.parseLong _))
    r.register(makeConverter(java.lang.Float.parseFloat _))
    r.register(makeConverter(java.lang.Short.parseShort _))
    r.register(makeConverter(java.lang.Boolean.parseBoolean _ ))
    r.register(makeConverter(scala.math.BigDecimal(_)))
    r.register(makeConverter(scala.math.BigInt(_)))
    r.register(makeConverter(s => new java.io.File(s)))
    r.register { s => 
      if (s.length == 1) s(0)
      else {
        val msg = "cannot convert a string \"%s\" of length %d to a character".format(s, s.length)
        throw new RuntimeException(msg) 
      }
	}
    r.register(s => s)
  }
  
  def makeOptions(args: List[MainArg]): acli.Options = {
    val options = new acli.Options
    args.collect {
      case arg: NamedArg => arg
    }.foreach { arg =>
      options.addOption(arg.cliOption)
    }
    options
  }  
}

trait Application {
  /**
   * Returns a fresh Apache Commons CLI PosixParser
   * Override this method to use a different type of parser.
   */
  protected def makeParser: acli.CommandLineParser = new acli.PosixParser
  
  /**
   * Prefix for named arguments, defaults to "-"
   * Override this method to change the prefix.  It should correspond to what's used by
   * the Apache Commons CLI parser that is used.
   */
  def namedArgumentPrefix = "-"
  
  /** override this method to register any additional converters for processing arguments */
  protected def registerCustomConversions(r: ConverterRegistry[String]) {
    // nothing
  }
  
  /**
   * Adds a flag called "help" which will trigger usage to be printing to the console
   * Override this method to return None if no help flag is desired
   * Override this method to return Some("flagName") if a different name is desired
   */
  protected def helpFlag: Option[String] = Some("help")
  
  /**
   * Returns the name of this class to be used in usage messages
   * Override this method if a different name is desired.
   */
  def programName: String = {
    val im = currentMirror.reflect(this)
    val sym = im.symbol
    sym.name.decoded
  }
  
  /** override to return a false value in order to have usage errors thrown (useful for testing) */
  protected def catchUsageError: Boolean = true
  
  def main(cmdline: Array[String]) {
    try {
      val f = buildCommandFunction(cmdline)
      f()
    } catch {
      case UsageError(msg) if catchUsageError => print(msg) 
      case ite: java.lang.reflect.InvocationTargetException => {
        val toThrow = if (ite.getTargetException ne null) ite.getTargetException else ite
        throw toThrow
      }
    }
  }
  
  protected def buildCommandFunction(cmdline: Array[String]): Function0[Unit] = {
    val mainMethodMirror = Application.findMainMethod(this)
    val args = Application.extractArgs(mainMethodMirror.symbol)
    val options = {
      val r = Application.makeOptions(args)
      helpFlag.map { name =>
        val opt = new acli.Option(name, false, "display usage information (this message)")
        r.addOption(opt)
      }
      r
    }
    val parser = makeParser
    val parsed = parser.parse(options, cmdline)
    for(name <- helpFlag if parsed.hasOption(name)) {
      val msg = usageMessage(args)
      throw UsageError(msg)
    }
    
    val positionalArgIndices = args.collect {
      case p: PositionalArg => p  
    }.zipWithIndex.map { t =>
      val (arg, idx) = t
      (arg.name, idx)
    }.toMap
    
    val positionalArgValues = parsed.getArgs()
    if (positionalArgIndices.size != positionalArgValues.length) {
      throw UsageError("Received %d positional arguments and expected %d".format(positionalArgValues.length, positionalArgIndices.size))
    } else {
      val registry = Application.makeRegistry
      registerCustomConversions(registry)
      val im = currentMirror.reflect(this)
      
      val processedArgs: List[Try[Any]] = args.map { arg => 
        registry.get(arg.tpe) match {
          case Some(cf) => {
            arg match {
              case barg: BoolArg => Success(parsed.hasOption(barg.name))
              case oarg: OptionArg => if (parsed.hasOption(oarg.name)) {
                val sv = parsed.getOptionValue(oarg.name)
                convertArg(s => Some(cf(s)), oarg, sv) 
              } else Success(None)
              case darg: ArgWithDefault => if (parsed.hasOption(darg.name)) {
                val sv = parsed.getOptionValue(darg.name)
                convertArg(cf, darg, sv)
              } else {
                Success(darg.defaultValue(im))
              }
              case narg: NamedArg => {
                val sv = parsed.getOptionValue(narg.name)
                convertArg(cf, narg, sv) 
              }
              case parg: PositionalArg => {
                val sv = parsed.getArgs()(positionalArgIndices(parg.name))
                convertArg(cf, parg, sv)
              }
            }
          }
          case None => throw DesignError("No conversion to a %s for argument %s defined".format(arg.tpe.typeSymbol.name.decoded, arg.name))
        }
      }
      
      if (processedArgs.exists(_.isFailure)) {
        val failures = processedArgs.collect {
          case Failure(e) => e
        }
        val msgs = failures.map { f =>
          f match {
            case UsageError(msg) => msg
            case e => e.getMessage 
          }
        }.mkString("\n")
        val msg = if(failures.size > 1) "%d errors occurred while processing arguments\n%s".format(failures.size, msgs) else msgs
        throw UsageError(msg)
      }
      
      val argArray = processedArgs.collect {
        case Success(a) => a
      }.toArray
      
      () => {
        mainMethodMirror(argArray: _*)
      }
    }
  }
  
  private def convertArg(cf: String => Any, arg: MainArg, sv: String) = Try(cf(sv)) match {
    case Success(r) => Success(r)
    case Failure(_ @ UsageError(m)) => {
      val msg = "Malformed argument %s: %s".format(arg.name, m)
      val ue = UsageError(msg)
      Failure(ue)
    }
    case Failure(e) => {
      val msg = "Malformed argument value \"%s\" for %s of type %s:\n\t%s".format(sv, arg.name, arg.typeName, e.getMessage)
      val ue = UsageError(msg)
      Failure(ue)
    }
  }
  
  /**
   * padding inserted into usage message columns, defaults to 4 spaces
   * Override this method to change the amount of padding
   **/
  def pad: Int = 4
  
  def usageMessageHeader(posArgs: Seq[PositionalArg], optArgs: Seq[NamedArg]): String = {
    val name = programName
    val posArgMsg = positionalArgMsg(posArgs)
    val posArgSep = if (posArgs.isEmpty) "" else " "
    val optsStr = if (optArgs.isEmpty) "" else " [options]"      
    "%s:%s%s%s".format(name, optsStr, posArgSep, posArgMsg)     
  }
  
  /**
   * Label for the name column in the usage message, default value is "Name"
   * Override this method to change the label 
   **/
  def nameColumnHeader    = "Name"
  /**
   * Label for the default value column in the usage message, default value is "Default Value"
   * Override this method to change the label
   */
  def defaultColumnHeader = "Default Value"
  /**
   * Label for the type column in the usage message, the default value is "Type"
   * Override this method to change the label
   */
  def typeColumnHeader    = "Type"
  /**
   * Label for the usage column in the usage message, the default value is "Usage"
   * Override this method to change the label
   */
  def usageColumnHeader   = "Usage"
        
  def usageMessage(args: Seq[MainArg]): String = {
    val (posArgs, namedArgs) = MainArg.splitArgs(args)
    val header = usageMessageHeader(posArgs, namedArgs)
    val namedMsg = makeNamedArgsUsageMsg(namedArgs) 
    if (namedMsg.isEmpty) header else "%s\n%s".format(header, namedMsg)
  }
  
  def makeNamedArgsUsageMsg(namedArgs: Seq[NamedArg]): String = if (namedArgs.isEmpty) "" else {
    val im = currentMirror.reflect(this)
    val defaults = namedArgs.map { arg =>
      arg match {
        case darg: ArgWithDefault => {
          val dvs = darg.defaultValue(im).toString //TODO: may want to define inverse converters instead
          (darg.name, dvs)
        }
        case narg => (narg.name, "")
      }
    }.toMap

    val optFormat = makeOptionFormatString(namedArgs, defaults) 
    val header = optFormat.format(nameColumnHeader, typeColumnHeader, defaultColumnHeader, usageColumnHeader)
    val rows = namedArgs.map(makeOptionArgUsageRow(_, defaults, optFormat)).mkString("\n")
    "%s\n%s".format(header, rows)
  }
  
  def makeOptionArgUsageRow(arg: NamedArg, defaults: Map[String, String], optFormat: String) = {
    val nameUsage = "%s%s".format(namedArgumentPrefix, arg.name)
    val usage = arg match {
      case barg: BoolArg => nameUsage
      case _ => "%s <%s>".format(nameUsage, arg.typeName)
    }
    optFormat.format(arg.name, arg.typeName, defaults(arg.name), usage)
  }
  
  def makeOptionFormatString(optArgs: Seq[NamedArg], defaults: Map[String, String]) = {
    val nameWidth = {
      val widestName = optArgs.map(_.name.length).max
      scala.math.max(nameColumnHeader.length, widestName) + pad
    }
    val typeWidth = {
      val widestTypeName = optArgs.map(_.typeName.length).max
      scala.math.max(typeColumnHeader.length, widestTypeName) + pad
    }
    val defaultWidth = {
      val widestDefaultValue = defaults.valuesIterator.map(_.length).max
      scala.math.max(defaultColumnHeader.length, widestDefaultValue) + pad
    }
    "%1$-" + nameWidth + "s%2$-" + typeWidth + "s%3$-" + defaultWidth + "s%4$s"    
  }

  
  def positionalArgMsg(args: Seq[PositionalArg]): String = args.map(positionalArgUsageString _).mkString(" ")
  
  def positionalArgUsageString(arg: PositionalArg): String = "<%s: %s>".format(arg.name, arg.typeName)
  
}
