package tdd
package api
package lj

import cats.data.* 
import cats.*
import cats.free.*
import cats.syntax.all.*
import tdd.util.*

type SearchSpace[F] = Mu[[t] =>> LazyList[LJ[F, t]]]

object SearchSpace: 

    type SearchF[F] = [t] =>> LazyList[LJ[F, t]]

    given [F: Form]: Functor[SearchF[F]] = 
        Functor[LazyList].compose[LJ.Aux[F]]

    extension [A, B](f: PartialFunction[A, B])
        def apply(l: LazyList[A]): LazyList[B] = 
            l.flatMap: a => 
                LazyList.from(f.lift(a))

    def coalg[F: Form]: Coalgebra[Sequent[F], SearchF[F]] = seq =>
        
        val rotations: LazyList[Sequent[F]] = LazyList.from(seq.rotations)

        Rule[LJ.Axiom].coalg(rotations) ++ 
        Rule[LJ.LeftFalse].coalg(rotations) ++
        Rule[LJ.RightImplication].coalg(LazyList(seq)) ++ 
        Rule[LJ.RightDisjunctionL].coalg(LazyList(seq)) ++ 
        Rule[LJ.RightDisjunctionR].coalg(LazyList(seq)) ++ 
        Rule[LJ.RightConjunction].coalg(LazyList(seq)) ++ 
        Rule[LJ.LeftImplication].coalg(rotations) ++ 
        Rule[LJ.LeftConjunction].coalg(rotations) ++ 
        Rule[LJ.LeftDisjunction].coalg(rotations)

    def alg[F: Form, T: Term.Aux[F]]: Algebra[SearchF[F], List[T]] = 
        _.toList.flatMap(_.sequence.map(LJ.alg[F, T]))

    given toTree[F: Form: Show]: Algebra[SearchF[F], Tree[Any]] = 
        nel => Tree((), nel.toList.map(LJ.toTree))

    def apply[F: Form](p: F): SearchSpace[F] = 
        Mu.unfold(coalg)(Sequent.proof(p))

    def trace[F: Form](p: F): Cofree[SearchF[F], Sequent[F]] = 
        Cofree.unfold(Sequent.proof(p))(coalg)

    def proof[F: Form, T: Term.Aux[F]](p: F): List[T] = 
        Mu.hylo[SearchF[F]](coalg, alg)(Sequent.proof(p)) 

        