package cs320

package object hw07 extends Homework07 {
  trait KXCFAEValue
  case class NumV(n: Int) extends KXCFAEValue
  case class CloV(param: List[String], body: KXCFAE, env: Env) extends KXCFAEValue
  case class ContV(proc: Cont) extends KXCFAEValue
  case class ThrowV() extends KXCFAEValue

  type Cont = KXCFAEValue => KXCFAEValue
  type Env = Map[String, KXCFAEValue]

  //list(String),list(KXCFAE), fenv,env => fenv (a->interp,env)
  def putlist_to_fenv(lstr: List[String], alist: List[KXCFAE], fenv:Env, env:Env, k: Cont): Env = {
    lstr match {
      case Nil => alist match {
        case Nil =>
          print(fenv)
          fenv
        case _ => error(s"wrong arity")
      }
      case l :: rest => alist match {
        case Nil => error(s"wrong arity")
        case a :: arest =>
          print(a)
            putlist_to_fenv(rest, arest, fenv + (l -> interp(a,env,k)), env, k)

      }
    }
  }
  // (KXCFAEValue,KXCFAEValue)=>KXCFAEValue
  def numAdd(x: KXCFAEValue, y: KXCFAEValue): KXCFAEValue = (x, y) match{
    case (NumV(n), NumV(m)) => NumV(n+m)
    case _=> error(s"not both numbers: $x, $y")
  }
  def numSub(x: KXCFAEValue, y: KXCFAEValue): KXCFAEValue = (x, y) match{
    case (NumV(n), NumV(m)) => NumV(n-m)
    case _=> error(s"not both numbers: $x, $y")
  }
  // (String, env) => KXCFAEValue
  def lookup(name: String, env: Env):KXCFAEValue =
    env.getOrElse(name, error(s"free identifier error"))


  def interp(e: KXCFAE, env:Env, k:Cont): KXCFAEValue = {
    e match {
      case Throw => env.getOrElse("Throw", error(s"no enclosing try-catch"))
      case Num(n) => k(NumV(n))
      case Add(l, r) =>
        interp(l, env, lv =>
        lv match {
          case ThrowV() => ThrowV()
          case _ => interp(r, env, rv =>
            rv match {
              case ThrowV() => ThrowV()
              case _ => k(numAdd(lv, rv))
            })
        })
      case Sub(l, r) =>
        interp(l, env, lv =>
          lv match {
            case ThrowV() => ThrowV()
            case _ => interp(r, env, rv =>
              rv match {
                case ThrowV() => ThrowV()
                case _ => k(numSub(lv, rv))
              })
          })
      case Id(x)=> k(lookup(x,env)) //x= string
      case Fun(x,b)=> CloV(x,b,env) //x= list(string)
      case App(f,a)=> interp(f,env, fv => // a= list(kxcfae)이니까 하나씩 반복
          fv match {
            case CloV(xlist, b, fenv) => //p=list(string)
              print(b)
              interp(b, putlist_to_fenv(xlist,a,fenv,env,k), k)
            case ContV(kv)=>
              a match{
                case one:: rest =>
                  interp(one, env, av => kv(av))
                case _ =>error(s"can contv get many var?")
              }

            case ThrowV() => ThrowV()
            case v => error(s"not a closure or continuation: $v")
          })
      case Withcc(x, b)=>
        interp(b, env + (x->ContV(k)), k)
      case If0(c,t,e)=> interp(c, env, cv =>
      cv match{
        case NumV(0) => interp(t, env, k)
        case ThrowV() => ThrowV()
        case _ => interp(e, env, k)
      })
      case Try(t,c)=> interp(t, env+("Throw"-> ThrowV()), tv=>
      tv match{
        case ThrowV() =>
          interp(c, env, k)
        case _=> tv
      }
      )
    }
  }

  def run(str: String): String =
    interp(KXCFAE(str), Map(), x=>x) match {
      case NumV(n) => n.toString
      case CloV(a,b,c) => s"function: $a $b $c"
      case ContV(c) => "continuation"
      case ThrowV() =>"throw"
    }

  def tests: Unit = {
    test(run("{{fun {x y} {- y x}} 10 12}"), "2")
    test(run("{fun {} 12}"), "function")
    testExc(run("{{fun {x y} 1} 2}"), "wrong arity")
    test(run("{withcc esc {{fun {x y} x} 1 {esc 3}}}"), "3")
    test(run("{{fun {x y} {- y x}} 10 12}"), "2")
    test(run("{fun {} 12}"), "function")
    testExc(run("{{fun {x y} 1} 2}"), "wrong arity")
    test(run("{withcc esc {{fun {x y} x} 1 {esc 3}}}"), "3")
    test(run("{try 1 catch 2}"), "1")
    test(run("{try {throw} catch 2}"), "2")
    test(run("{try {+ 1 {throw}} catch 2}"), "2")
    test(run("{{fun {f} {try {f} catch 1}} {fun {} {throw}}}"), 1)
    testExc(run("{throw}"), "no enclosing try-catch")


    /* Write your own tests */
  }
}
