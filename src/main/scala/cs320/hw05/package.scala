package cs320

package object hw05 extends Homework05 {
  trait SRBFAEValue
  case class NumV(n: Int) extends SRBFAEValue
  case class CloV(param: String, body: SRBFAE, env: Env) extends SRBFAEValue
  //case class BoxV(var boxV: SRBFAEValue) extends SRBFAEValue
  case class BoxV(addr: Addr) extends SRBFAEValue
  case class RecV(param: Rec_addr) extends SRBFAEValue

  type Env = Map[String, SRBFAEValue]
  type Addr = Int
  type Sto = Map[Addr, SRBFAEValue]
  type Rec_addr = Map[String, Addr]

  //malloc : Sto => Addr
  def malloc(sto: Sto):Addr = maxAddress(sto)+1

  //maxAddress : Sto => Addr
  def maxAddress(sto: Sto):Addr = sto.keySet.+(0).max

  def numVAdd(x: SRBFAEValue, y: SRBFAEValue): SRBFAEValue = (x, y) match{
    case (NumV(n), NumV(m)) => NumV(n+m)
    case _=> error(s"not both numbers: $x, $y")
  }
  def numVSub(x: SRBFAEValue, y: SRBFAEValue): SRBFAEValue = (x, y) match{
    case (NumV(n), NumV(m)) => NumV(n-m)
    case _=> error(s"not both numbers: $x, $y")
  }
  // (String, env) => SRBFAEValue
  def lookup(name: String, env: Env):SRBFAEValue =
    env.getOrElse(name, error(s"free identifier error"))

  // (String, Rec_addr) => Addr
  def lookup_rec(name: String, map: Rec_addr):Addr =
    map.getOrElse(name ,error(s"no such field"))

  // (String, sto) => SRBFAEValue
  def storeLookup(addr: Int, sto: Sto):SRBFAEValue =
    sto.getOrElse(addr, error(s"free identifier of box error"))

  def list_to_sto(l1: List[String], l2: List[SRBFAE], env: Env, ra: Rec_addr, sto:Sto ):(Rec_addr, Env, Sto) = l1 match{
    case Nil => (ra, env, sto)
    case k :: rest => l2 match{
      case v:: vrest =>
        val (kv,ks) = interp(v, env, sto)
        val kaddr = malloc(ks)
        list_to_sto(rest,vrest,env, ra+ (k->kaddr), ks+(kaddr ->kv))
      case Nil => error(s"illegal record")
    }


  }



  // interp :(SRBFAE, Env, Sto, Sto for rec) => (SRBFAEValue, Sto)
  def interp(e: SRBFAE, env:Env, sto:Sto): (SRBFAEValue, Sto) = e match {
      case Num(n) => (NumV(n), sto)

      case Add(l, r) =>
        val (lv, ls) = interp(l,env,sto)
        val (rv, rs) = interp(r,env,ls)
        (numVAdd(lv,rv),rs)

      case Sub(l, r) =>
        val (lv, ls) = interp(l,env,sto)
        val (rv, rs) = interp(r,env,ls)
        (numVSub(lv,rv),rs)

      case Id(x)=>
        (lookup(x,env), sto) //x= String
      case Fun(x,b)=>
        (CloV(x,b,env),sto) //x= list(string)
      case App(f,a)=>
        interp(f,env,sto) match{ //fenv+(x->interp(a,env)
        case (CloV(x,b,fenv), fsto) =>
          val (xv, xs)=interp(a,env,sto)
          interp(b, fenv + (x-> xv) ,xs) // xs or fsto??
        case (v, sto) => error (s"not a closure: $v")
      }
      case NewBox(expr) =>
        val (v,s) = interp(expr, env, sto)
        val addr = malloc(s)
        (BoxV(addr), s+(addr ->v))

      case SetBox(b,e) =>
        val (bv, bs) = interp(b,env,sto)
        bv match {
          case BoxV(addr) =>
            val (v,s) = interp(e,env,bs)
            (v,s+(addr->v))
          case _ => error(s"not a box: $bv")
        }

      case OpenBox(b) =>

        val(bv, bs) = interp(b,env,sto)
        bv match {
          case BoxV(addr) => (storeLookup(addr,bs), bs)
          case _ => error(s"not a box: $bv")
        }

      case Seqn(l,rs)=> rs match {
        case Nil => interp(l, env, sto)
        case r :: rest =>
          val (lv, ls) = interp(l, env, sto)
          interp(Seqn(r, rest), env, ls)
      }
      //case class Rec(fields: Map[String, SRBFAE]) extends SRBFAE
      case Rec(record_map) => // map(str, addr), addr->boxv
        val klist= record_map.keys.toList
        val vlist= record_map.values.toList
        val (rec_addr,renv,rsto) = list_to_sto(klist, vlist, env, Map[String, Addr](), sto)
        (RecV(rec_addr), rsto)

      case Get(expr: SRBFAE, name: String)=>
        val (xv, xs) = interp(expr, env, sto)
        xv match {
          case RecV(rm) =>
            val v= storeLookup(lookup_rec(name,rm), xs)//interp(lookup_rec(name, rm), xe, sto)
            (v,xs)
          case xv => error(s"no such field")
        }
      case Set(r, name, e) => { // Set(record: SRBFAE, field: String, expr: SRBFAE)

        val (rv, rs) = interp(r, env, sto)
        rv match {
          case RecV(rm) =>
            val (ev, ee) = interp(e, env, rs)
            val ad = lookup_rec(name,rm)
            (ev,ee+(ad -> ev))

          case _ => error(s"no such field")
        }
      }

  }


  def value_to_str(v: SRBFAEValue): String ={
    v match{
      case NumV(n)=> n.toString
      case CloV(x,b,fenv) => "function"
      case BoxV(m)=> "box"
      case RecV(r)=>"record"
    }
  }

  def run(str: String): String = {
    //value_to_str(
    val (vv, vs) =interp(SRBFAE(str),Map(),Map())
    value_to_str(vv)
  }

  def tests: Unit = {

    test(run("""{{fun {b} {seqn {setbox b {+ 2 {openbox b}}}
                          {setbox b {+ 3 {openbox b}}}
                          {setbox b {+ 4 {openbox b}}}
                          {openbox b}}}
                {newbox 1}}"""), "10")
    testExc(run("{get {rec {x 1}} y}"), "no such field")
    test(run("{{fun {r} {seqn {set r x 5} {get r x}}} {rec {x 1}}}"), "5")
    test(run("42"), "42")
    test(run("{fun {x} x}"), "function")
    test(run("{newbox 1}"), "box")
    test(run("{rec}"), "record")

    /* Write your own tests */
    test(run("{seqn {newbox 1}{newbox 2}}"), "box")
    test(run(str="{setbox {newbox 10} 3}"),"3")
    test(run(str="{openbox {newbox 2}}"),"2")
    test(run("{{fun {x} {seqn {setbox x 10} {setbox x {+ 1 {openbox x}}} {openbox x}}}{newbox 1}}"), "11")
    test(run(str="{seqn {fun {a} {seqn {setbox a {setbox {newbox 10} 3} }{openbox a}}}{newbox 1}}"),"box")
    test(run(str="{{fun{a}{seqn{setbox a 10}{openbox a}}}{newbox 0}}"),"10")
    test(run("{rec {a 3}}"), "record")
    test(run("{{fun {r} {seqn {set r x 5} {get r x}}} {rec {x 1}}}"), "5")
    test(run("{seqn {get {rec {a 3}{b 2}} b} }"), "2")
    test(run("{seqn {get {rec {a 3}{b 2}} a} }"), "3")
    test(run("{set {rec {a 3}} a 4 }"), "4")
    test(run("{{fun {a} {openbox a}}{newbox 10}}"), "10")
    test(run(str="{seqn {{fun {a} {openbox a}}{newbox 0}} {{fun {b} {setbox b {- 10 2}}} {newbox 0}}}"),"8")
    testExc(run("{get {set {rec {a 3}} a 4 } a }"), "no such field")
    test(run("{seqn {set {rec {a 3}{b 2}} b 5} }"), "5")



  }
}
