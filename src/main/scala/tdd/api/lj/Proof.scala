package tdd
package api
package lj

import cats.* 
import cats.syntax.all.*
import cats.arrow.*
import tdd.util.*
import SearchSpace.given 
import SearchSpace.SearchF

type Proof[F] = Mu[[t] =>> CalculusF[F, t]]

object Proof: 

    type SearchStrategy = [F] => Form[F] ?=> SearchSpace[F] => LazyList[Proof[F]]

    val DepthFirst: SearchStrategy = [F] => (_: Form[F]) ?=> (ss: SearchSpace[F]) =>
        Mu.sequenceF(ss)

    val IterativeDeepening: SearchStrategy = [F] => (_: Form[F]) ?=> (ss: SearchSpace[F]) =>

        val MAX = 10

        def go(depth: Int): LazyList[Proof[F]] =
            val results = Mu.sequenceF_Until(depth)(ss)
            if results != Nil then results
            else if depth > MAX then LazyList.empty
            else go(depth+1)

        go(1)

    extension [F: Form](form: F)
        
        def allProofs(strategy: SearchStrategy = IterativeDeepening): LazyList[Proof[F]] = 
            val ss: SearchSpace[F] = Mu.unfold(SearchSpace.coalg)(api.Sequent.proof(form))
            strategy[F](ss)
        
        def proof(strategy: SearchStrategy = IterativeDeepening): Option[Proof[F]] = 
            allProofs(strategy).headOption
            
    extension [F: Form](proof: Proof[F])
        def program[T: Term.Aux[F]]: Option[T] = 
            Mu.fold(CalculusF.alg)(proof)

    extension [F: Form, T: Term.Aux[F]](form: F)
        
        def allPrograms(strategy: SearchStrategy = IterativeDeepening): LazyList[T] = 
            form.allProofs(strategy).flatMap(_.program)

        def program(strategy: SearchStrategy = IterativeDeepening): Option[T] = 
            allPrograms(strategy).headOption
