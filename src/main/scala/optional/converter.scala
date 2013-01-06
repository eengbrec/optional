package optional

import scala.reflect.runtime.universe._
import scala.reflect.runtime.currentMirror

object Converter {
  def apply[T](conversions: Function1[T, Any]*)(implicit ct: TypeTag[T]) = {
    
  }
}

case class DuplicateConverter(existing: Type, duplicate: Type) 
     extends Exception("Cannot register %s because %s is already registered".format(existing.toString, duplicate.toString))

/**
 * Utility class that 
 */
class ConverterRegistry[I](implicit val itt: TypeTag[I]) {
  import scala.collection.mutable.{ ArrayBuffer }
  
  private case class Entry(rt: Type, f: I => Any)
  private val converters = new ArrayBuffer[Entry]
  private def getEntry(rt: Type) = converters.find(e => e.rt =:= rt)
  
  def register[R](f: I => R)(implicit rtt: TypeTag[R]) {
    val rt: Type = rtt.tpe
    getEntry(rt) match {
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