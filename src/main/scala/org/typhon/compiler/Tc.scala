package org.typhon.compiler

import BasicTypes._

object Tc {

  type TcEnv = Map[VarName, Sigma]
  type TcSub = Map[Uniq, Tau]

  /**
   * A record for storing the current state of the typechecking algorithm.
   */
  case class TcState(
      env:TcEnv = Map(), 
      sub:TcSub = Map(),
      nextUnique:Uniq = 0)

  type TcError = String
  type TcResult[A] = Either[TcError, A]
  type TcTrans[A] = TcState => (TcResult[A], TcState)

  /**
   * The Tc monad -- carries an environment, a substitution and a supply of fresh unique metavariable ids.
   */
  case class Tc[A](run:TcTrans[A]) {
    def map[B](f:A=>B):Tc[B] = Tc(state => {val (res, newState) = run(state); (res.right.map(f), newState)})
    def flatMap[B](f:A=>Tc[B]):Tc[B] = Tc{state =>
      val (res1, newState) = run(state)
  //    println("ran " + state + " to produce " + res1)
      res1.right.map(f).right.map(_.run(newState)) match {
        case Left(err) => (Left(err), newState)
        case Right(res2) => res2
      }
    }
    def withFilter(f:A=>Boolean):Tc[A] = flatMap(a => if (f(a)) pure(a) else fail("Failed filter: " + a))
  
    def >>[B](tc2:Tc[B]) = for(_ <- this; result <- tc2) yield result
  }
  
  // operations in the Tc monad
  def pure[A](a:A) = Tc(state => (Right(a), state))
  def fail[A](err:TcError):Tc[A] = Tc(state => (Left(err), state))
  def check(test:Boolean, msg:String):Tc[Unit] = if (test) pure(()) else fail(msg)
  def getEnv:Tc[TcEnv] = Tc(state => (Right(state.env), state))
  def getSub(tv:TyMetaVar):Tc[Option[Tau]] = Tc(state => (Right(state.sub get tv.id), state))
  def addSub(tv:TyMetaVar, ty:Tau):Tc[Unit] = Tc{state =>
    (Right(()), state.copy(sub = state.sub + (tv.id -> ty)))
  }
  def newMetaVar:Tc[Tau] = newUniq map TyMetaVar

  def newUniq:Tc[Int] = Tc(state => (Right(state.nextUnique), state.copy(nextUnique = state.nextUnique + 1)))

  /**
   * Run tc with the new assumption (id:ty). The type binding (id:ty) is 
   * scoped to the computation and does not carry over into the result.
   */
  def extendEnv[A, S <% Sigma](tyBinds:List[(VarName, S)], tc:Tc[A]):Tc[A] = tyBinds match {
    case Nil => tc
    case (name, ty)::tyBinds => Tc { state =>
      // first augment the type environment with the new binding
      val augmentedState = state.copy(env = state.env + (name -> ty))
      // run the inner computation in this environment and get its result and end state
      val (result, endState) = extendEnv(tyBinds, tc).run(augmentedState)
      // return the result along with new state, but restoring the original type environment
      (result, endState.copy(env = state.env))
    }
  }

  def extendEnv[A](name:VarName, ty:Sigma, tc:Tc[A]):Tc[A] = extendEnv(List((name, ty)), tc)

  // standard monadic utilities
  def sequence[A](ms:Iterable[Tc[A]]):Tc[List[A]] = ms.toList match {
    case Nil => pure(Nil)
    case m::ms => for {
      x <- m
      xs <- sequence(ms)
    } yield x::xs
  }

  def mapM[A, B](f:A=>Tc[B], ms:Iterable[A]) = sequence(ms map f)

}