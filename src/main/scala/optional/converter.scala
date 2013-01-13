package optional

import scala.reflect.runtime.universe._
import scala.reflect.runtime.currentMirror

case class DuplicateConverter(existing: Type, duplicate: Type) 
     extends Exception("Cannot register %s because %s is already registered".format(existing.toString, duplicate.toString))

object ConverterRegistry {
  import scala.util.{ Try, Success, Failure }
  def makeMsg(s: String, t: String) = {
    "the value \"%s\" could not be converted into a %s".format(s, t)
  }
    
  def makeConverter[T](f: String => T)(implicit rtt: TypeTag[T]) = { s: String =>
    Try(f(s)) match {
      case Success(v) => v
      case Failure(e) => {
        val msg = makeMsg(s, rtt.tpe.typeSymbol.name.decoded)
        throw UsageError(msg)
      }
    }
  }
}


/**
 * Utility class that 
 */
class ConverterRegistry[I](implicit val itt: TypeTag[I]) {
  import scala.collection.mutable.{ ArrayBuffer }
  
  private case class Entry(rt: Type, f: I => Any)
  private val converters = new ArrayBuffer[Entry]
  private def getEntry(rt: Type) = converters.find(e => e.rt =:= rt)
  
  def register[R](f: I => R, removeOnDuplicate: Boolean = true)(implicit rtt: TypeTag[R]) {
    val rt: Type = rtt.tpe
    getEntry(rt) match {
      case Some(e) if removeOnDuplicate => {
        val idx = converters.indexWhere(e => e.rt =:= rt)
        converters(idx) = Entry(rt, f)
      }
      case Some(e) => throw DuplicateConverter(e.rt, rt)
      case None => converters.append(Entry(rt, f)) 
    }
  }
  def get[R](implicit rtt: TypeTag[R]): Option[I => R] = {
    val rt: Type = rtt.tpe
    getEntry(rt) match {
      case Some(e) => Some(e.f.asInstanceOf[I => R])
      case None => None
    }
  }
  def get(rt: Type): Option[I => Any] = getEntry(rt).map(_.f)
}