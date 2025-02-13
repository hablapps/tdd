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

    type SearchStrategy = [F] => Form[F] ?=> SearchSpace[F] => Option[Proof[F]]

    given FunctorFilter_Search[F: Form]: FunctorFilter[SearchF[F]] with 
        val functor: Functor[SearchF[F]] = Functor[SearchF[F]] 
        def mapFilter[A, B](fa: SearchF[F][A])(f: A => Option[B]): SearchF[F][B] = 
            FunctorFilter[List].mapFilter(fa)(_.traverse(f))
    
    def chooseFirst[F] = new FunctionK[SearchF[F], [t] =>> Option[CalculusF[F, t]]]: 
        def apply[A](fa: SearchF[F][A]): Option[CalculusF[F, A]] = 
            fa.headOption

    // Plain DFS, not safe with infinite search spaces
    
    val DepthFirst: SearchStrategy = [F] => (_: Form[F]) ?=> (ss: SearchSpace[F]) => 
        Mu.mapFilterF(chooseFirst)(ss)

    // Iterative deepning with MAX depth: safe, but not complete

    val IterativeDeepening: SearchStrategy = [F] => (_: Form[F]) ?=> ss =>

        val MAX = 10

        def go(depth: Int)(ss: SearchSpace[F]): Option[Proof[F]] = 
            Mu.mapFilterFUntil(depth)(chooseFirst)(ss).orElse(
                if depth < MAX then go(depth+1)(ss)
                else None)

        go(1)(ss)
            
    extension [F: Form: Show](form: F)
        
        def proof: Option[Proof[F]] = 
            val ss: SearchSpace[F] = Mu.unfold(SearchSpace.coalg)(api.Sequent.proof(form))
            IterativeDeepening[F](ss)
        
        def proofWith(strategy: SearchStrategy): Option[Proof[F]] = 
            val ss: SearchSpace[F] = Mu.unfold(SearchSpace.coalg)(api.Sequent.proof(form))
            strategy[F](ss)
            
    extension [F: Form: Show, T: Term.Aux[F]](form: F)
        
        def program: Option[T] = 
            val derivation: Option[Proof[F]] = form.proof
            derivation.flatMap(Mu.fold(CalculusF.alg))

        def programThrough(strategy: SearchStrategy): Option[T] = 
            val derivation: Option[Proof[F]] = form.proofWith(strategy)
            derivation.flatMap(Mu.fold(CalculusF.alg))
