package org.typhon.compiler

object BasicTypes {
  
type Uniq = Int
type VarName 	= String
type ConName	= String
type TyVarName 	= String
type TyConName	= String

case class Module(dataDecls:List[DataDecl], termDecls:List[TermDecl]) {
  override def toString = {
    val tds = for {
      (v, ty, ftm) <- termDecls
    } yield v + " :: " + ty + "\n" + v + " = " + ftm
    "Module(\n" + dataDecls.mkString("\n") + "\n\n" + tds.mkString("\n") + "\n)"
  }
}

case class DataDecl(id:TyConName, tyVars:List[TyVarName], cons:List[TyCon]) {
  override def toString = "data " + id + " " + tyVars.mkString(" ") + " = " + cons.mkString(" | ")
}

type TermDecl = (VarName, Sigma, FTerm)

sealed trait Term {
  def $(tm:Term):Term = TmApp(this, tm)
}

case class TmLit(lit:String) extends Term {
  override def toString = lit
}

case class TmVar(name:VarName) extends Term {
  override def toString = name
}

case class TmLam(vars:List[VarName], body:Term) extends Term {
  override def toString = "(\\" + vars.mkString(" ") + " -> " + body + ")"
}

case class TmApp(func:Term, arg:Term) extends Term {
  override def toString = "(" + func + " " + arg + ")"
}

case class TmLet(decls:List[(VarName, Term)], body:Term) extends Term {
  override def toString = "(let " + decls.map(pair => pair._1 + " = " + pair._2).mkString("; ") + 
		  				  " in " + body + ")"
}

case class TmCase(obj:Term, cases:List[(Pattern, Term)]) extends Term {
  override def toString = "(case " + obj + " of " + 
		  cases.map(pair => pair._1 + " -> " + pair._2).mkString("; ") + ")"
}

sealed trait FTerm {
  def $(tm:FTerm):FTerm = FApp(this, tm)
}

case class FLit(lit:String) extends FTerm {
  override def toString = lit
}

case class FVar(name:VarName, appTys:List[Tau]) extends FTerm {
  override def toString = name + (if (appTys.isEmpty) "" else "[" + appTys.mkString(",") + "]")
}

case class FLam(vars:List[VarName], body:FTerm, ty:Tau) extends FTerm {
  val (argTys, resTy) = ty.uncurry
  override def toString = "(\\" + (vars, argTys).zipped.map("("+_+"::"+_+")").mkString(" ") + " -> ("+body+"::"+resTy+"))"
}

case class FApp(func:FTerm, arg:FTerm) extends FTerm {
  override def toString = "(" + func + " " + arg + ")"
}

case class FLet(decls:List[(VarName, Sigma, FTerm)], body:FTerm, bodyTy:Tau) extends FTerm {
  override def toString = "(let " + decls.map(_ match { case (lhs, ty, rhs) => 
    "(" + lhs + "::" + ty + ") = " + rhs
  }).mkString("; ") + " in (" + body + "::" + bodyTy + ")"
}

case class FCase(v:VarName, cases:List[FTerm]) extends FTerm {
  override def toString = "(case " + v + " of " + cases.mkString("; ") + ")"
}

sealed trait Pattern

case class PatVar(name:VarName) extends Pattern {
  override def toString = name
}

case class PatCon(name:ConName, args:List[Pattern]) extends Pattern {
  override def toString = "(" + (name::args).mkString(" ") + ")"
}

sealed trait Tau {
  def -->(resTy:Tau):Tau = TyArr(this, resTy)
  def arrTys:List[Tau] = this match {
    case TyArr(argTy, resTy) => argTy :: resTy.arrTys
    case _ => List(this)
  }
  def uncurry:(List[Tau], Tau) = {
    val tys = arrTys
    (tys.dropRight(1), tys.last)
  }
}

case class TyCon(name:TyConName, tyArgs:List[Tau]) extends Tau {
  override def toString() = "(" + (name::(tyArgs.map(_.toString()))).mkString(" ") + ")"
}

case class TyArr(argTy:Tau, resTy:Tau) extends Tau {
  override def toString() = "(" + argTy + " -> " + resTy + ")"
}

case class TyQuantVar(name:TyVarName) extends Tau {
  override def toString() = name
}

case class TyMetaVar(id:Uniq) extends Tau {
  override def toString() = "$" + id
}

case class Sigma(tvs:List[TyVarName], body:Tau) {
  override def toString() = "(" + {
    if (tvs.isEmpty)
      body.toString()
    else 
      "forall " + tvs.mkString(" ") + " . " + body
  } + ")"
}

implicit def tauToSigma(rho:Tau):Sigma = Sigma(Nil, rho)

def lam(xs:VarName*)(body:Term):Term = TmLam(xs.toList, body)
def let(bindings:(VarName, Term)*)(body:Term):Term = TmLet(bindings.toList, body)
def let(v:VarName, binding:Term)(body:Term):Term = TmLet(List((v, binding)), body)
def cas(obj:Term)(cases:(Pattern, Term)*):Term = TmCase(obj, cases.toList)
def pat(s:String, ps:Pattern*):Pattern = if (ps.isEmpty && s.head.isLower) PatVar(s) else PatCon(s, ps.toList)
def dat(id:TyConName, tyVars:TyVarName*)(cons:TyCon*) = DataDecl(id, tyVars.toList, cons.toList)
def con(id:String, tyArgs:Tau*) = TyCon(id, tyArgs.toList)
implicit def v(s:VarName) = TmVar(s)
implicit def tv(s:VarName) = TyQuantVar(s)
implicit def n(i:Int) = TmLit("" + i)
implicit def p(s:String):Pattern = pat(s)

}