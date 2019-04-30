package cs320

package object hw03 extends Homework03 {

  trait MRFWAEValue
  case class NumV(n: Int) extends MRFWAEValue
  case class CloV(param: List[String], body: MRFWAE, env: Env) extends MRFWAEValue
  case class RecV(param: Map[String, MRFWAEValue]) extends MRFWAEValue

  type Env = Map[String, MRFWAEValue]

  // (String, env) => MRFWAEValue
  def lookup(name: String, env: Env):MRFWAEValue =
    env.getOrElse(name, error(s"free identifier error"))

  // (String, env) => MRFWAEValue
  def lookup_rec(name: String, map: Env):MRFWAEValue =
    map.getOrElse(name ,error(s"no such field") )

  // (MRFWAEValue,MRFWAEValue)=>MRFWAEValue
  def numAdd(x: MRFWAEValue, y: MRFWAEValue): MRFWAEValue = (x, y) match{
    case (NumV(n), NumV(m)) => NumV(n+m)
    case _=> error(s"not both numbers: $x, $y")
  }
  def numSub(x: MRFWAEValue, y: MRFWAEValue): MRFWAEValue = (x, y) match{
    case (NumV(n), NumV(m)) => NumV(n-m)
    case _=> error(s"not both numbers: $x, $y")
  }
  //list(String),list(MRFWAE), fenv,env => fenv (a->interp,env)
  def putlist_to_fenv(lstr: List[String], alist: List[MRFWAE], fenv:Env, env:Env): Env = {
    lstr match {
      case Nil => alist match {
        case Nil =>
          //println("finall",fenv)
          fenv
        case _ => error(s"wrong arity")
      }
      case l :: rest => alist match {
        case Nil => error(s"wrong arity")
        case a :: arest =>
          putlist_to_fenv(rest, arest, fenv + (l -> interp(a, env)), env);
      }
    }
  }


  //MRFWAE, Env-> MRFWAEValue
  def interp(e: MRFWAE, env:Env): MRFWAEValue = {
    e match {
      case Num(n) => NumV(n)
      case Add(l, r) => numAdd(interp(l, env), interp(r, env))
      case Sub(l, r) => numSub(interp(l, env), interp(r, env))
      case With(x,i,b) => interp(b,env+(x->interp(i,env)))
      case Id(x)=> lookup(x,env) //x= list(string)
      case Fun(x,b)=> CloV(x,b,env) //x= list(string)
      case App(f,a)=> interp(f,env) match{ //fenv+(x->interp(a,env)
        case CloV(x,b,fenv) =>
          interp(b, putlist_to_fenv(x,a,fenv,env))
        case v => error (s"not a closure: $v")
      }
        case Rec(record_map) =>
          RecV (record_map.transform((key, value) => interp(value,env)))
        case Acc(expr: MRFWAE, name: String)=>interp(expr,env) match{
          case RecV(rm)=> lookup_rec(name, rm)
          case v=> error(s"not a record: $v")
        }
      }
  }

  def value_to_str(v: MRFWAEValue): String ={
    v match{
      case NumV(n)=> n.toString
      case CloV(x,b,fenv) => "function"
      case RecV(m)=> "record"
    }
  }

  def run(str: String): String = {
    value_to_str(interp(MRFWAE(str),Map()))
  }

  def tests: Unit = {


    test(run("{{fun {x y} {+ x y}} 1 2}"), "3")
    test(run("{{fun {} {+ 3 4}}}"), "7")
    testExc(run("{{fun {x y} {+ x y}} 1}"), "wrong arity")
    test(run("{access {record {x 1} {y 2}} x}"), "1")
    testExc(run("{access {record {x 1} {y 2}} z}"), "no such field")
    testExc(run("{record {x {access {record {y 1}} z}}}"), "no such field")
    test(run("42"), "42")
    test(run("{fun {x} x}"), "function")
    test(run("{record {x 1}}"), "record")

    /* Write your own tests */
    //test(run(str=""), "")

    test(run(str="{with {f 3} {+f 10}}"), "13")
    test(run(str="{with {f {fun {a b} {- a b}} } {f 10 4}}"), "6")
    test(run(str="{with {c 100} {with {f {fun {a b} {- a b}} } {f c 4}} }"), "96")
    test(run(str="{with {a 100} {with {f {fun {a b} {- a b}} } {f a 4}} }"), "96")
    //test(run("{{fun {x} {+ x}} 1}"), "error")
    test(run("{access {record {x {+1 2}} {y 2}} x}"), "3")
    test(run("{access {record {x {+1 2}} {y {fun {a} {-a 1} } } } y}"), "function")
    test(run("{access {record {x {+1 2}} {y {fun {a b} {-a b} } } } y}"), "function")
    test(run("{access {record {x {+1 2}} {y {fun {a b} {-a b} } } } y}"), "function")
    test(run("{{access {record {x {+1 2}} {y {fun {a} {-a 1} } } } y} 10}"), "9")
    test(run("{{fun {x y} {+ x y}} 10 2}"), "12")
    test(run("{{fun {x} {+ 1 x}} 2}"), "3")
    test(run("{{fun {x y z} {+ {+ x y} z}} 1 2 3}"), "6")
    test(run("{+ 4 2}"), "6")
    test(run("{with {f {fun {x} x}} {f 30}}"), "30")
    testExc(run("{with {f {fun {x} x} } {f}}"), "wrong arity")
    test(run("{with {f {fun {x} x}} {record{ x f} } }"), "record")
    test(run("{record {x 1}{y {fun {a b}{+a b} } }} "), "record")
  }
}
