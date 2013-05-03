package org.typhon.compiler

import Tc._
import BasicTypes._

object Builtin {

  val TM_PLUS   = "+"
  val TM_P2     = "P2"
  val TM_P3     = "P3"
  val TM_ERROR  = "error"
  
  val TY_INT    = TyCon("Int", Nil)
  val TY_ERROR  = Sigma(List("a"), TyCon("String", Nil) --> "a")
  
  val defaultState = TcState(env = Map(
    TM_PLUS     -> (TY_INT --> (TY_INT --> TY_INT)),
    TM_P2       -> Sigma(List("a", "b"), "a" --> ("b" --> TyCon("P2", List("a", "b")))),
    TM_ERROR    -> TY_ERROR
  ))
}