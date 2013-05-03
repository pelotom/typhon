package org.typhon.compiler

import BasicTypes._
import TypeChecker._
import Builtin._

object JavaGen {
  
  val F1 = "$F"
  val APPLY = "$apply"
    
  def genModule(m:Module):String = {
    val (dataClasses, cons) = m.dataDecls.map(genData).unzip
    val genDecl = genPubDecl.tupled
    dataClasses.mkString(" ") + cons.flatten.mkString(" ") +
    (m.termDecls map genDecl).mkString(" ") +
    genError
  }
  
  def genError = "private static <a> " + F1 + "<String, a> " + 
      TM_ERROR + "(final a _a) { return new " + F1 + "<String, a>() { " +
      "public a " + APPLY + "(final String msg) { throw new Error(msg); } }; }"
  
  def genData(d:DataDecl):(String, List[String]) = {
    val brackets = bracket(d.tyVars.map(TyQuantVar(_)))
    val resTy = "M"
    val matchSig = "<" + resTy + "> " + resTy + " match(" +
        (d.cons map (c => "final " +
          genTy(curry(c.tyArgs, TyQuantVar(resTy))) + 
          " " + conSelector(c))).mkString(",") + ")"
    val (conClasses, cons) = (d.cons map genCon(d, matchSig)).unzip
    ("public static abstract class " + d.id + brackets + " { " +
      conClasses.mkString(" ") + "public abstract " + matchSig + ";" + " }"
     , cons)
  }
  
  def conSelector(c:TyCon):String = "$if" + c.name
  
  def conClassName(c:TyCon):String = "$" + c.name
      
  def genCon(d:DataDecl, matchSig:String)(c:TyCon):(String, String) = {
    val brackets = bracket(d.tyVars.map(TyQuantVar(_)))
    val fieldNames:List[VarName] = c.tyArgs.zipWithIndex.map(ty => " $" + (ty._2 + 1))
    val typedFields = (c.tyArgs map genTy, fieldNames).zipped map (_ + " " + _)
    val className = conClassName(c)
    val conClass = "private static final class " +
      className + brackets + " extends " + d.id + brackets + " { " + 
      typedFields.map(field => "private final " + field + ";").mkString(" ") +
      " private " + className + "(" + 
      	typedFields.map(field => "final " + field).mkString(", ") +
      ") { " + 
      	fieldNames.map(name => "this." + name + " = " + name + ";").mkString(" ") +
      " } " +
      "public " + matchSig + " { " + " return " + 
      	conSelector(c) + fieldNames.map(field => "." + APPLY + "(" + field + ")").mkString("") + 
      "; }" +
  " } "
    val conTy = getConTy(d.id, d.tyVars, c)
    val conTm = FLam(fieldNames, 
        FLit("new " + d.id + "." + className + brackets + "(" + fieldNames.mkString(", ") + ")"), conTy.body)
    val conStr = genPubDecl(c.name, conTy, conTm)
    (conClass, conStr)
  }
  
  def genTm(tm:FTerm):String = tm match {
    case FLit(l) => genLit(l)
    
    case FVar(name, appTys) => name + (
      if (appTys.isEmpty)
        ""
      else
    	"(" + appTys.map(ty => "(" + genTy(ty) + ")null").mkString(", ") + ")"
    )
    
    case FLam(Nil, body, _) => genTm(body)
    case FLam(v::vs, body, TyArr(argTy, resTy)) => "new " + genTy(TyArr(argTy, resTy)) + "() { " + 
		"public " + genTy(resTy) + " " + APPLY + "(final " + genTy(argTy) + " " + v + ") { " +
		"return " + genTm(FLam(vs, body, resTy)) + "; }" +
	"}"
      
    case FApp(func, arg) => genTm(func) + "." + APPLY + "(" + genTm(arg) + ")"
    
    case FLet(decls, body, bodyTy) => {
//      if (decls.forall(_._2.tvs.isEmpty)) // 
      "new Object() {" + 
		(decls.unzip3.zipped map genDecl("private")).mkString(" ") + 
		"private " + genTy(bodyTy) + " body() { return " + genTm(body) + "; }" +
	  "}.body()"
    }
		
    case FCase(body, cases) => "(" + body + ").match(" + (cases map genTm).mkString(", ") + ")"
  }
  
  def genLit(lit:String) = lit match {
    case "True" 	=> "true"
    case "False" 	=> "false"
    case other		=> other
  }
  
  def genPubDecl = genDecl("public", "static") _
  
  def genDecl(modifiers:String*)(v:VarName, sigma:Sigma, rhs:FTerm):String = {
    val bodyTy = genTy(sigma.body)
    val tvs = sigma.tvs.map((TyQuantVar(_)))
    val tyArgs = tvs.map(ty => "final " + genTy(ty) + " " + "$" + ty).mkString(", ")
    modifiers.mkString(" ") + " final " + bracket(tvs) + " " +
      bodyTy + " " + v +
      (if (tyArgs.isEmpty()) { // monomorphic; use a field instead of a method
        " = " + genTm(rhs) + ";"
      } else { 
        "(" + tyArgs + ") { " + 
//      "final " + bodyTy + " " + v + " = " + genTm(FVar(v, tvs)) + ";" +
    	 "return " + genTm(rhs) + "; }"
    })
  }
  
  def genTy(ty:Tau):String = ty match {
    case TyCon(name, tyArgs) 	=> genTyCon(name) + bracket(tyArgs)
    case TyArr(argTy, resTy)	=> F1 + bracket(List(argTy, resTy))
    case TyQuantVar(name) 		=> name
    case TyMetaVar(id) 			  => "Object"
  }
  
  def genTyCon(name:String):String = name match {
    case "Bool" => "Boolean"
    case "Int"  => "Integer"
    case other	=> other
  }
  
  def bracket(tys:List[Tau]):String = if (tys.isEmpty) "" else tys.map(genTy).mkString("<", ",", ">")
}