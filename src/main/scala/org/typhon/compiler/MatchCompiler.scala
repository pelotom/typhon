package org.typhon.compiler

import BasicTypes._
import Tc._
import TypeChecker._

object MatchCompiler {
  
  type Equation = (List[Pattern], Term)
  
  def checkMatch(us:List[VarName], qs:List[Equation], default:FTerm, expTy:Tau):Tc[FTerm] = us match {
    case Nil => qs match {
      case Nil => pure(default)
      case (Nil,e)::_ => checkTau(e, expTy)
      // TODO: if guard support is added, we would have to check for FAIL and use the rest of the 
      // equations as alternatives
    }
    case u::us => foldGroups(u, us, partition(isVar, qs), default, expTy)
  }
  
  def foldGroups(u:VarName, us:List[VarName], qss:List[List[Equation]], default:FTerm, expTy:Tau):Tc[FTerm] = qss match {
    case Nil => pure(default)
    case qs::qss => for {
      fDefault <- foldGroups(u, us, qss, default, expTy)
      fCase <- matchVarCon(u, us, qs, fDefault, expTy)
    } yield fCase
  }
  
  /**
   * Preconditions: 
   * 1) (u::us).length == q._1.length for all q <- qs
   * 2) qs is either all variables or all constructors of the same type
   */
  def matchVarCon(u:VarName, us:List[VarName], qs:List[Equation], default:FTerm, expTy:Tau):Tc[FTerm] = {
    val q = qs.head
    if (isVar(q)) 
      checkMatch(us, for {(PatVar(v)::ps, e) <- qs} yield (ps, subVar(u, v, e)), default, expTy)
    else for {
      cs <- constructors(getCon(q))
      clauses <- mapM((c:ConName) => matchClause(c, u, us, qs.filter(getCon(_) == c), default, expTy), cs)
    } yield makeCase(u, clauses)
  }
  
  /**
   * Preconditions:
   * (u::us).length == q._1.length, and q._1.head is a PatCon(c, ...) for all q <- qs
   */
  def matchClause(c:ConName, u:VarName, us:List[VarName], qs:List[Equation], default:FTerm, expTy:Tau):Tc[FTerm] = for {
    (parmTys, ty) <- conTys(c)
    uTy <- instVarTy(u)
    _ <- unify(uTy, ty)
    us2 <- mapM((_:Tau) => freshVar, parmTys)
    qs2 <- mapM(pushConPatterns(parmTys.length), qs)
    body <- extendEnv(us2 zip parmTys, checkMatch(us2 ++ us, qs2, default, expTy))
  } yield if (us2.isEmpty) body else FLam(us2, body, curry(parmTys, expTy))
  
  def pushConPatterns(n:Int)(q:Equation):Tc[Equation] = q match {
    case (PatCon(c, cps)::ps, e) => {
      val ncps = cps.length
      check(ncps == n, "Constructor '" + c + "' should have " + n + " arguments, but has been given " + ncps) >> 
      pure(cps ++ ps, e)
    }
    case q => sys.error("Couldn't match " + q)
  }
  
  def isVar(q:Equation):Boolean = q._1.head.isInstanceOf[PatVar]
  
  def getCon(q:Equation):ConName = q._1.head.asInstanceOf[PatCon].name
  
  def partition[A, B](f:A => B, xs:List[A]):List[List[A]] = xs match {
    case Nil => Nil
    case x::Nil => List(xs)
    case x::y::xs => {
      val rest = partition(f, y::xs)
      if (f(x) == f(y)) 
        (x::rest.head)::rest.tail
      else
        List(x)::rest
    }
  }
  
  def freshVar:Tc[VarName] = newUniq map ("$m" + _)
  
  /**
   * Substitute u for v in e.
   */
  def subVar(u:VarName, v:VarName, tm:Term):Term = {
    val recurse:(Term => Term) = subVar(u, v, _)
    val res = tm match {
      case TmLit(l) => tm
      case TmVar(w) => if (w == v) TmVar(u) else tm
      case TmLam(vs, body) => if (vs contains v) tm else TmLam(vs, recurse(body))
      case TmLet(decls, body) => if (decls map (_._1) contains v) tm else TmLet(decls, recurse(body))
      case TmApp(e1, e2) => TmApp(recurse(e1), recurse(e2))
      case TmCase(obj:Term, cases:List[(Pattern, Term)]) => {
        val (pats, terms) = cases.unzip
        if (pats.toSet flatMap patVars contains v) tm 
        else TmCase(recurse(obj), pats zip (terms map recurse))
      }
    }
//    println("replacing " + v + " with " + u + " in " + tm + ": " + res)
    res
  }
  
  def makeCase(u:VarName, clauses:List[FTerm]):FTerm = FCase(u, clauses)
  
  def resTyName(s:Sigma):Option[TyConName] = s.body.uncurry._2 match {
    case TyCon(name, _) => Some(name)
    case _ => None
  }
  
  def isCon(v:VarName) = v.charAt(0).isUpper
  
  def constructors(c:ConName):Tc[List[ConName]] = for { 
    // FIXME this is pretty inefficient... we should just keep a map of type constructors to the lists of their data constructors
    env <- getEnv
    tyName = resTyName(env(c))
    cs = env.filter((p:(VarName, Sigma)) => isCon(p._1) &&  tyName == resTyName(p._2)).keys.toList
  } yield cs
}