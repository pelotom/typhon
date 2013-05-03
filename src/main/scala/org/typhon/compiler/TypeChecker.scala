package org.typhon.compiler

import scala.Predef.Set._
import BasicTypes._
import Tc._
import MatchCompiler._
import Builtin._
import JavaGen._

object TypeChecker {
  
def checkModule(dataDecls:List[DataDecl], termDecls:List[(VarName, Term)]):TcResult[Module] = {
  val importEnv = Map(/*TODO*/)
  val localEnv = (dataDecls flatMap (dd => dd.cons map (c => 
    (c.name, getConTy(dd.id, dd.tyVars, c))))).toMap
  val state = defaultState copy (env = defaultState.env ++ importEnv ++ localEnv)
  val tc = for {
    fDecls <- inferTau(TmLet(termDecls, 0)).map(_._2.asInstanceOf[FLet].decls)
    (vs, sigmas, rhss) = fDecls.unzip3
    rhss <- mapM(zonkTm, rhss)
  } yield Module(dataDecls, (vs, sigmas, rhss).zipped.toList)
  tc.run(state)._1
}

def checkModule(dDecls:List[DataDecl]):Map[VarName, Sigma] = {
  (dDecls flatMap (dd => dd.cons map (c => (c.name, getConTy(dd.id, dd.tyVars, c))))).toMap
}

def getConTy(tyName:TyConName, tvs:List[TyVarName], con:TyCon):Sigma = {
  Sigma(tvs, curry(con.tyArgs, TyCon(tyName, tvs.map(TyQuantVar(_)))))
}

def curry(argTys:List[Tau], resTy:Tau):Tau = argTys.foldRight(resTy)(_ --> _)

/**
 * Algorithm M -- propagate expected type inward to better-localized error messages.
 */
def checkTau(tm:Term, expTy:Tau):Tc[FTerm] = tm match {
  case TmLit(lit) => unify(TyCon(litType(lit), Nil), expTy) >> pure(FLit(lit))
  
  case TmVar(v) => for {
    sigma 	<- lookupVarTy(v)
    tyApps 	<- mapM((v:TyVarName) => newMetaVar, sigma.tvs)
	tau 	= replaceBoundTyVars((sigma.tvs zip tyApps).toMap, sigma.body)
    _ 		<- unify(tau, expTy)
  } yield FVar(v, tyApps)
    
  case TmApp(fun, arg) => for {
    (funTy, fFun) 	<- inferTau(fun)
    (argTy, resTy) 	<- unifyFun(funTy)
    fArg 			<- checkTau(arg, argTy)
    _ 				<- unify(resTy, expTy)
  } yield FApp(fFun, fArg)
  
  case TmLam(vs, body) => checkLam(vs, body, expTy) map identity
  
  case TmLet(decls, body) => checkDecls(declGroups(decls.toSet).map(_.toList), body, expTy) map identity
  
  case TmCase(obj, cases) => checkCase(obj, cases, expTy, true)
}

def checkLam(vs:List[VarName], body:Term, expTy:Tau):Tc[FLam] = vs match {
  case Nil => for {
	fBody <- checkTau(body, expTy)
  } yield FLam(Nil, fBody, expTy)
  
  case v::vs => for {
    (argTy, resTy) <- unifyFun(expTy)
    fLam <- extendEnv(v, argTy, checkLam(vs, body, resTy))
  } yield fLam copy (vars = v :: fLam.vars, ty = TyArr(argTy, fLam.ty))// FIXME shouldn't this just be ty = expTy?
}

def checkDecls(groups:List[List[(VarName, Term)]], body:Term, expTy:Tau):Tc[FLet] = groups match {
  case Nil => for {
    fBody <- checkTau(body, expTy) 
  } yield FLet(Nil, fBody, expTy)
  
  case group :: groups => {
    val (lhss, rhss) = group.unzip
    for {
      // give each v a monotype while typing the right-hand sides to allow recursion
      recTys 	<- mapM((x:String) => newMetaVar, lhss) 
      fRhss	 	<- extendEnv(lhss zip recTys, sequence((rhss, recTys).zipped map checkTau))
      bvs		= fRhss.toSet flatMap getBoundTyVars
      s		 	<- quantify(recTys.head, bvs) // only need to quantify the first; the rest will be bound by the same set of variables (right?)
      ss		<- mapM(zonk, recTys.tail) map (_.map(Sigma(s.tvs, _)))
      rhsTys 	= s :: ss
      fLet      <- extendEnv(lhss zip rhsTys, checkDecls(groups, body, expTy))
      m 		= (lhss zip rhsTys.map(_.tvs.map(TyQuantVar(_)))).toMap
      fRhss2 	= fRhss map (addMissingTyApps(m, _))
      fDecls 	= (lhss, rhsTys, fRhss2).zipped.toList
    } yield fLet copy (decls = fDecls ++ fLet.decls)
  }
}

def checkCase(obj:Term, cases:List[(Pattern, Term)], expTy:Tau, preprocess:Boolean):Tc[FTerm] = for {
    (vTy, fObj) <- inferTau(obj)
    (pats, tms) = cases.unzip
    v <- freshVar
    fError <- checkTau(TmVar(TM_ERROR) $ TmLit("\"Pattern match failed\""), expTy)
    fMatch <- extendEnv(v, vTy, checkMatch(List(v), pats.map(List(_)).zip(tms), fError, expTy))
} yield FLet(List((v, vTy, fObj)), fMatch, expTy)

def conTys(c:ConName):Tc[(List[Tau], Tau)] = for {
  conTy <- instVarTy(c)
} yield conTy.uncurry

def addMissingTyApps(m:Map[VarName,List[Tau]], ftm:FTerm):FTerm = ftm match {
  case FLit(_) 					=> ftm
  case FVar(name, appTys) 		=> if (appTys.isEmpty && m.isDefinedAt(name)) FVar(name, m(name)) else ftm
  case FApp(func, arg) 			=> FApp(addMissingTyApps(m, func), addMissingTyApps(m, arg))
  case FLam(vs, body, resTy) 	=> FLam(vs, addMissingTyApps(m -- vs, body), resTy)
  case FLet(decls, body, ty)    => FLet(decls, addMissingTyApps(m -- decls.map(_._1), body), ty)
  case FCase(v, cases)          => FCase(v, cases map (addMissingTyApps(m, _)))
}

def getBoundTyVars(ftm:FTerm):Set[TyVarName] = ftm match {
  case FLit(_) 				=> empty
  case FVar(_, _) 			=> empty
  case FLam(_, body, _) 	=> getBoundTyVars(body)
  case FApp(func, arg) 		=> getBoundTyVars(func) ++ getBoundTyVars(arg)
  case FLet(decls, body, _) => decls.flatMap(decl => decl._2.tvs).toSet ++ getBoundTyVars(body)
  case FCase(v, cases) 	    => cases.toSet flatMap getBoundTyVars
}

def inferTau(tm:Term):Tc[(Tau, FTerm)] = for {
  ty <- newMetaVar
  fTm <- checkTau(tm, ty)
} yield (ty, fTm)

def unifyFun(ty:Tau):Tc[(Tau, Tau)] = ty match {
  case TyArr(arg, res) => pure(arg, res)
  case _ => for {
    argTy 	<- newMetaVar
    resTy 	<- newMetaVar
    _ 		<- unify(ty, TyArr(argTy, resTy))
  } yield (argTy, resTy)
}

def inferSigma(t:Term):Tc[Sigma] = inferTau(t) map (_._1) flatMap quantify

/**
 * Unify the metavariables between two types, or die trying.
 */
def unify(ty1:Tau, ty2:Tau):Tc[Unit] = if (ty1 == ty2) pure() else (ty1, ty2) match {
  // if either type is a variable, use unifyVar
  case (TyMetaVar(tv), ty) => unifyVar(TyMetaVar(tv), ty)
  case (ty, TyMetaVar(tv)) => unifyVar(TyMetaVar(tv), ty)
  
  // if both are function types, unify the respective arg and result types
  case (TyArr(arg1, res1), TyArr(arg2, res2)) => unify(arg1, arg2) >> unify(res1, res2)
  
  // similarly for arbitrary type constructors...
  case (TyCon(c1, args1), TyCon(c2, args2)) 
  	if (c1 == c2 && args1.size == args2.size) => sequence((args1, args2).zipped map unify) >> pure()
 
  // all other cases are irreconcilable
  case _ => for {
    mvs1 	<- getMetaTvs(ty1)
    mvs2 	<- getMetaTvs(ty2)
    _ 		<- replaceMetaVars(getBindings(mvs1 ++ mvs2))
    z1 		<- zonk(ty1)
    z2 		<- zonk(ty2)
    _ 		<- fail[Unit]("Cannot unify '" + z1 + "' with '" + z2 + "'")
  } yield ()
}

def unifyVar(tv1:TyMetaVar, ty2:Tau):Tc[Unit] = for {
  oRef1 <- getSub(tv1) 
  u <- oRef1 match {
    case Some(ref1)	=> unify(ref1, ty2)
    case None	    => unifyUnboundVar(tv1, ty2)
  }
} yield u

def unifyUnboundVar(tv1:TyMetaVar, ty2:Tau):Tc[Unit] = ty2 match {
  case TyMetaVar(tv2) => getSub(TyMetaVar(tv2)) flatMap {_ match {
    case Some(refTy2) 	=> unify(tv1, refTy2)
    case None 			=> addSub(tv1, ty2)
  }}
  case _ => for {
    ids2 <- getMetaTvs(ty2)
    u <- if (ids2 contains tv1) {
      val sub = getBindings(ids2) 
      for {
    	_ 	<- replaceMetaVars(sub)
    	z1 	<- zonk(tv1)
    	z2 	<- zonk(ty2)
        _ 	<- fail[Unit]("Occurs check: cannot construct the infinite type: " + z1 + " = " + z2) 
      } yield ()
    } else
      addSub(tv1, ty2)
  } yield u
}

def litType(lit:String):String = {
  // TODO
  if ("True" == lit || "False" == lit)
    "Bool"
  if (lit.startsWith("\""))
    "String"
  else try {
    Integer.parseInt(lit)
	"Int"
  } catch {case _:NumberFormatException => "Object"}
}

def lookupVarTy(v:VarName):Tc[Sigma] = for {
  env <- getEnv
  result <- env.get(v) match {
  	case Some(ty) => pure(ty)
    case None => fail("Not in scope: '" + v + "'")
  }
//  val _ = println(id + " :: " + result)
} yield result

def instVarTy(v:VarName):Tc[Tau] = lookupVarTy(v) flatMap instantiate

/**
 * Gets all unsubstituted metavariables mentioned in a type.
 */
def getMetaTvs(ty:Tau):Tc[Set[TyMetaVar]] = ty match {
  case TyCon(_, args) 	=> mapM(getMetaTvs, args) map (_.toSet) map (_.flatten)
  case TyQuantVar(_)    => pure(Set.empty)
  case TyMetaVar(v) 	=> for {
    oRef <- getSub(TyMetaVar(v))
    vs <- oRef match {
      case Some(ty) => getMetaTvs(ty)
      case None 	=> pure(Set(TyMetaVar(v)))
    }
  } yield vs
  case TyArr(arg, res) 	=> for (ids1 <- getMetaTvs(arg); ids2 <- getMetaTvs(res)) yield ids1 ++ ids2
}

/**
 * Instantiate a polytype's quantified variables with fresh metavariables.
 */
def instantiate(forall:Sigma):Tc[Tau] = for {
  types <- mapM((v:TyVarName) => newMetaVar, forall.tvs)
} yield replaceBoundTyVars((forall.tvs zip types).toMap, forall.body)

def quantify(ty:Tau):Tc[Sigma] = quantify(ty, empty)

/**
 * Quantify over all flexible unsubstituted metavariables in the given type.
 */
def quantify(ty:Tau, usedBinders:Set[TyVarName]):Tc[Sigma] = for {
  ty <- zonk(ty)
//  val _ = println("quantifying: " + ty)
  mvs <- getMetaTvs(ty)
  env <- getEnv
  envMvs <- mapM(getMetaTvs, env.values.map(_.body))
  flexMvs = envMvs.foldLeft(mvs)((acc, tvs) => acc -- tvs)
  bindings = getBindings(flexMvs, usedBinders)
  _ <- replaceMetaVars(bindings)
  ty <- zonk(ty)
  sigma = Sigma(bindings.map(_._2), ty)
//  val _ = println("\t=> " + sigma)
} yield sigma

def getBindings(mvs:Set[TyMetaVar]):List[(TyMetaVar, TyVarName)] = getBindings(mvs, empty)

def getBindings(mvs:Set[TyMetaVar], usedBinders:Set[TyVarName]):List[(TyMetaVar, TyVarName)] = {
  val newBinders = (allBinders filter (x => !(usedBinders contains x))) take mvs.size
  mvs.toList zip newBinders.toList
}

/**
 * Apply a substitution to the quantified type variables in a given type.
 */
def replaceBoundTyVars(sub:Map[TyVarName, Tau], ty:Tau):Tau = ty match {
  case TyCon(c, args) 	=> TyCon(c, args map (replaceBoundTyVars(sub, _)))
  case TyQuantVar(v) 	=> if (sub.isDefinedAt(v)) sub(v) else ty
  case TyMetaVar(v) 	=> ty // TODO why don't we need to look up what this refers to?
  case TyArr(arg, res) 	=> TyArr(replaceBoundTyVars(sub, arg), replaceBoundTyVars(sub, res))
}

def replaceMetaVars(subs:List[(TyMetaVar, TyVarName)]):Tc[Unit] = 
  mapM((p:(TyMetaVar, TyVarName)) => addSub(p._1, TyQuantVar(p._2)), subs) >> pure()

val allBinders = {
  val aToZ = ('a' to 'z').map(_.toString).toStream
  aToZ ++ (for (n <- Stream.from(1); c <- aToZ) yield c.toString + n)
}

/**
 * Eliminates all substituted metavariables from the given type.
 */
def zonk(ty:Tau):Tc[Tau] = ty match {
  case TyCon(c, args) 	=> mapM(zonk, args) map (TyCon(c, _))
  case TyArr(arg, res) 	=> for (zarg <- zonk(arg); zres <- zonk(res)) yield TyArr(zarg, zres)
  case TyMetaVar(v) 	=> for {
    oRef <- getSub(TyMetaVar(v))
    result <- oRef match {
      case Some(ref) => zonk(ref)
      case None => pure(ty)
    }
  } yield result
  case TyQuantVar(_)	=> pure(ty)
}

def zonkTm(tm:FTerm):Tc[FTerm] = tm match {
  case FLit(_) 						=> pure(tm)
  
  case FVar(v, appTys) 				=> mapM(zonk, appTys) map (FVar(v, _))
  
  case FApp(func, arg) 				=> for {
    zFunc <- zonkTm(func)
    zArg <- zonkTm(arg)
  } yield FApp(zFunc, zArg)
  
  case FLam(vs, body, resTy) 	=> for {
    zBody <- zonkTm(body)
    zResTy <- zonk(resTy)
  } yield FLam(vs, zBody, zResTy)
  
  case FLet(decls, body, bodyTy) 	=> {
    val (lhss, sigmas, rhss) = decls.unzip3
    for {
      zTys <- mapM(zonk, sigmas.map(_.body))
      zSigmas = (sigmas.map(_.tvs), zTys).zipped map Sigma
      zRhss <- mapM(zonkTm, rhss)
      zBody <- zonkTm(body)
      zBodyTy <- zonk(bodyTy)
    } yield FLet((lhss, zSigmas, zRhss).zipped.toList, zBody, zBodyTy)
  }
  
  case FCase(v, cases) 			=> for {
	  zCases <- mapM(zonkTm, cases)
  } yield FCase(v, zCases)
}

/**
 * Takes a set of declarations, discovers their interdependencies, groups the maximal 
 * mutually recursive components together and sorts the groups in dependency order.
 */
def declGroups(decls:Set[(VarName, Term)]):List[Set[(VarName, Term)]] = {
  val declMap = decls.toMap
  val pairs = decls map {d =>
    val mapsTo = freeVars(d._2) flatMap {x => 
      if (declMap isDefinedAt x) 
        Set((x, declMap(x))) 
      else 
        empty[(VarName, Term)]}
    (d, mapsTo)
  }
  Depend.tarjan(decls, pairs.toMap).reverse
}

def freeVars(tm:Term):Set[VarName] = tm match {
  case TmLit(lit) 		=> empty
  case TmVar(v)			=> Set(v)
  case TmApp(fun, arg) 	=> freeVars(fun) ++ freeVars(arg)
  case TmLam(vs, body) 	=> freeVars(body) -- vs
  case TmLet(decls, body) => {
    val binders = decls.map(_._1)
    val declVars = decls.map(_._2).flatMap(freeVars).toSet
    val bodyVars = freeVars(body)
    (declVars ++ bodyVars) -- binders
  }
  case TmCase(obj, cases) => freeVars(obj) ++
    (cases map (c => freeVars(c._2) -- patVars(c._1))).toSet.flatten
}

def patVars(pat:Pattern):Set[VarName] = pat match {
  case PatVar(name) => Set(name)
  case PatCon(name, args) => args.toSet flatMap patVars
}

def main(args:Array[String]) {
  
  val intTy = TyCon("Int", Nil)
  val boolTy = TyCon("Bool", Nil)
  val True = TmLit("True")
  val False = TmLit("False")

  val f = "f"
  val x = "x"
  val xs = "xs"
  val y = "y"
  val ys = "ys"
  
  val P1 = "P1"
  val P2 = "P2"
  val P3 = "P3"
  
  // \x -> x x
  val t1 = lam(x)(x $ x)
  val t2 = True
  val t3 = lam(x)(t2)
  val t4 = lam(x, f)(f $ (x $ 5))
  val t5 = 5 $ TmLit("hello")
  // let id = \x -> x in id id :: t -> t
  val t6 = let("id", lam(x)(x))("id" $ "id")
  // let x = x in x :: t
  val t7 = let(x, x)(x)
  // let fix = \x -> f (f x) in f :: t -> t
  val t8 = let("fix", lam(x)("fix" $ ("fix" $ x)))("fix")
  val t9 = let("id", lam(x)(v(x)))(P2 $ ("id" $ True) $ ("id" $ 9))
  val t10 = let("bar", lam(x)(let("foo", lam(y)(x))("foo")))("bar")
  
  val t11 = f $ lam(x)(True)
  val s11 = defaultState.copy(env = defaultState.env + (f -> ((intTy --> intTy) --> boolTy)))
  
  val t12 = lam(f)(f $ (P2 $ 32 $ True)) $ lam(x)(x)
  
  val t13 = lam(f)(P2 $ (f $ P2) $ (f $ P2))
  
  val t14 = let(f, f $ 9)(f)
  
  val t15 = lam(x, y)((x $ y) $ 9)
  
  // let f = \x -> x; y = \z -> f z in y
  val bindings = List((f, lam(x)(x)), (y, lam(x)(f $ x)));
  val t16 = TmLet(bindings, y)
  val t17 = TmLet(bindings, y $ 9)
  val t18 = TmLet(bindings, P2 $ (y $ 9) $ (y $ True))

  // let f = \x -> g x; g = \x -> f x in f g :: (a -> c, b -> d)
  val t19 = let((f, lam(x)("g" $ x)), 
		  		("g", lam(x)(f $ x)))		(/*pair $*/ f $ "g")
  
  val t20 = let((f, lam(x)(x)), 
		  		(y, P2 $ (f $ 6) $ (f $ True)))		(y)
  // the above doesn't typecheck under the basic implementation of mutually recursive let bindings because f is not allowed to
  // have a polytype in the definition of y... but if we can compute a dependency graph between the definitions, we can convert
  // it to a nested let which *does* type check and is semantically equivalent; because y depends on f we type f first and then 
  // generalize it before typing y
  
  // this doesn't typecheck even in Haskell; the addition of the seemingly inconsequential "let a = y" causes the definitions of
  // f and y to be mutually recursive, destroying the inferred polymorphism of f which is needed to type y.
  val t21 = let((f, lam(x)(let("a", y)(x))), 
		  		(y, P2 $ (f $ 6) $ (f $ True)))		(y)
  
  val t22 = cas(P2 $ True $ 9)(
      (pat(P2, "a", "b"), "a")
  )
	
  // bad
  val t23 = cas(P2 $ True $ 9)(
	(pat(P2, "a", "b"), "a"),
    (pat(P2, x, y), y)
  )
  
  val t24 = let((f, lam(x)(cas(x)(
      (pat(P2, "a", "b"), "+" $ "a" $ "b")
  ))))(f $ (P2 $ 3 $ 9))
		  		
  val p1Decl = dat(P1, "a")(con(P1, "a"))
  val p2Decl = dat(P2, "a", "b")(con(P2, "a", "b"))
  val p3Decl = dat(P3, "a", "b", "c")(con(P3, "a", "b", "c"))
  
  val L = "List"
  val N = "Nil"
  val C = "Cons"
  
  val listDecl = dat(L, "a")(
      con(N),
      con(C, "a", con("List", "a"))
  )
  
  val mapDecl = ("map", lam(f, xs)(cas(xs)(
      (pat(C, x, xs), C $ (f $ x) $ ("map" $ f $ xs)),
      (N, N)
  )))
  
  val headDecl = ("head", lam(xs)(cas(xs)(
      (N, let(x, x)(x)),
      (pat(C, x, xs), x)
  )))
  
  val zipWithDecl = ("zipWith", lam(f, xs, ys)(cas(P3 $ f $ xs $ ys)(
        (pat(P3, f, pat(C, x, xs), N), N),
        (pat(P3, f, pat(C, x, xs), pat(C, y, ys)), C $ (f $ x $ y) $ 
            ("zipWith" $ f $ xs $ ys)),
        (pat(P3, f, N, ys), N)
      )))
  
  val demo = ("demo", lam("A", "B", "C", f, xs, ys)(cas(P3 $ f $ xs $ ys)(
        (pat(P3, f, N, ys), "A" $ f $ ys),
        (pat(P3, f, xs, N), "B" $ f $ xs),
        (pat(P3, f, pat(C, x, xs), pat(C, y, ys)), "C" $ f $ x $ xs $ y $ ys)
      )))
      
  val zipWithDecl2 = ("zipWith", lam(f, xs, ys)(cas(xs)(
        (pat(N), N),
      	(pat(C, x, xs), cas(ys)(
      	  (pat(N), N),
      	  (pat(C, y, ys), C $ (f $ x $ y) $ ("zipWith" $ f $ xs $ ys))
      	))
      )))
      
  val stupidDecl = ("stupid", lam("ps")(cas("ps")(
        (pat(C, pat(P2, x, y), "ps2"), x)
      )))

  val dumbDecl = ("dumbDecl", let("foo", lam("a", "b", "c", "d", "e", "f")(N))(let("bar", (lam(y)(cas(y)(
      	(pat(N), 3)
      ))))("foo")))
      
//  printCheck(t10)
      
  val dataDecls = List(p1Decl, p2Decl, p3Decl, listDecl)
  val termDecls = List(mapDecl, zipWithDecl)
  
  println(termDecls.map(p => p._1 + " = " + p._2).mkString("\n"))

  val result = checkModule(dataDecls, termDecls)
  if (result.isLeft)
    println(result.left.get)
  else {
    val module = result.right.get
    println(module)
    println(genModule(module))
  }
  
  for (t <- List(t1, t2, t3, t4, t5, t6, /*t7,*/ t8, t9, t10, t11, t12, t13, t14, t15, t16, 
      t17, t18, /*t19,*/ t20, t21, t22, t23, t24)) {
//    printInfer(t, s11)
    printCheck(t, s11)
  }
  
  def printCheck(t:Term, initialState:TcState = defaultState) {
    val result = (for { 
      (ty, ftm) <- inferTau(t)
//      ty <- quantify(ty)
      ftm <- zonkTm(ftm) 
    } yield (ty, ftm)).run(initialState)
    if (result._1.isLeft)
      println("//  Failed to type " + t + ". " + result._1.left.get)
    else {
      val (sigma, ftm) = result._1.right.get
      println("//  " + t + " :: " + sigma)
      println("//->" + ftm)
      println("o = " + genTm(ftm) + ";")
      println("System.err.println(\"" + t.toString().replaceAllLiterally("\\", "\\\\") + "\\n\" + o);");
    }
//    println("Final sub:\n\t" + result._2.sub)
    println()
  }
}

}