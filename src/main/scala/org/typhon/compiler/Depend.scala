package org.typhon.compiler

import Set._
import scala.math._
import scala.util.Random

object Depend {
	
	case class TarjanState[V](index:Map[V, Int], lowlink:Map[V, Int], counter:Int, stack:List[V], sccs:List[Set[V]])
	
	def tarjan[V](vs:Set[V], adjacent:V=>Set[V]):List[Set[V]] = {
	  var state = TarjanState[V](Map(), Map(), 0, Nil, Nil)
	  for (v <- vs)
	    if (!state.index.isDefinedAt(v))
	      state = strongconnect(v, adjacent, state)
//	  println(state.sccs)
	  state.sccs
	}
	
	def strongconnect[V](v:V, adjacent:V=>Set[V], s:TarjanState[V]):TarjanState[V] = {
//	  println("processing node " + v);
	  var s2 	= s.copy(
	      index 	= s.index + (v -> s.counter),
	      lowlink 	= s.lowlink + (v -> s.counter),
	      counter 	= s.counter + 1,
	      stack 	= v :: s.stack
	  )
	  
	  for (w <- adjacent(v)) {
//		  println("processing edge " + v + " -> " + w);
	    if (!s2.index.isDefinedAt(w)) {
//	      println(w + " hasn't been seen yet");
	      s2 = strongconnect(w, adjacent, s2)
	      s2 = s2 copy (lowlink = s2.lowlink + (v -> min(s2.lowlink(v), s2.lowlink(w))))
	    } else if (s2.stack contains w) {
//	      println(w + " is part of the current SCC");
	      s2 = s2 copy (lowlink = s2.lowlink + (v -> min(s2.lowlink(v), s2.index(w))))
	    }
	  }
	  
	  if (s2.lowlink(v) == s2.index(v)) {// v is the root of a scc
	    val (newScc, newStack) = s2.stack splitAt ((s2.stack indexWhere(_ == v)) + 1)
//	    println(v + " is the root of an SCC: " + newScc)
	    s2 = s2 copy (sccs = newScc.toSet :: s2.sccs, stack = newStack)
	  }
	    
	  s2
	}

	def mkDeps[A, B](elems: (A, Set[B])*):(A => Set[B]) = elems.toMap.lift andThen (_ getOrElse empty)
	
	def main(args:Array[String]) {
//	  println(List(1,2,3) splitAt 1)
//	  println(findDeps(5, mkDeps(3 -> Set(4,5,6), 5 -> Set(4, 9), 9 -> Set(3, 7)), Set(3)))
	  val vs = Random.shuffle((1 to 7).toSet)
	  val adj = mkDeps(7->Set(6), 6->Set(5, 2), 5->Set(1), 1->Set(2), 2->Set(4), 4->Set(1,3))
	  println(tarjan(vs, adj))
	}
}