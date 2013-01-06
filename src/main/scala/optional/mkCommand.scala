package optional

import scala.reflect.runtime.universe._
import scala.reflect.runtime.currentMirror

object MakeCommand extends Application {
  val template = """
_%s() 
{
    local cur prev opts
    COMPREPLY=()
    cur="${COMP_WORDS[COMP_CWORD]}"
    prev="${COMP_WORDS[COMP_CWORD-1]}"
    opts="%s"

    if [[ ${cur} == -* ]] ; then
        COMPREPLY=( $(compgen -W "${opts}" -- ${cur}) )
        return 0
    fi
}
complete -F _%s %s
alias %s='scala %s $*'
  """
  def mkTemplate(name: String, className: String, opts: Seq[String]): String =
    template.format(name, opts mkString " ", name, name, name, className)
  

  
  def main(scriptName: String, className: String) {
    val modSymbol = currentMirror.staticModule(className)
    val t = modSymbol.typeSignature
    val mms = Application.findMainMethod(t)
    val args = Application.extractArgs(mms)
    
    val opts = args.map(_.name)

    val txt = mkTemplate(scriptName, className, opts)
    val tmpfile = java.io.File.createTempFile(scriptName, "", null)
    val writer = new java.io.FileWriter(tmpfile)
    writer write txt
    writer.close()
    
    println("# run this command in bash")
    println("source " + tmpfile.getAbsolutePath())
  }
}