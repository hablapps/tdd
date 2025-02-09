package tdd
package api
package lj

import cats.data.* 
import cats.*
import cats.free.*
import cats.syntax.all.*
import tdd.util.*

type SearchSpace[F] = Mu[[t] =>> List[CalculusF[F, t]]]

object SearchSpace: 

    type SearchF[F] = [t] =>> List[CalculusF[F, t]]

    given [F: Form]: Functor[SearchF[F]] = 
        Functor[List].compose(using Functor[[t] =>> CalculusF[F, t]])

    def coalg[F: Form]: Coalgebra[api.Sequent[F], SearchF[F]] = 
        Sequent.fromSequent(_)
            .map(CalculusF.coalg)
            .map(_.map(Sequent.toSequent))

    def alg[F: Form, T: Term.Aux[F]]: Algebra[SearchF[F], List[T]] = 
        _.toList.map(
            _.sequence
                .map(_.map(Some.apply))
                .map(CalculusF.alg[F, T])
                .map(_.toList)
        ).flatten.flatten

    given toTree[F: Form: Show]: Algebra[SearchF[F], Tree[Any]] = 
        nel => Tree((), nel.toList.map(CalculusF.toTree))

    def apply[F: Form](p: F): SearchSpace[F] = 
        Mu.unfold(coalg)(api.Sequent.proof(p))

    def trace[F: Form](p: F): Cofree[SearchF[F], api.Sequent[F]] = 
        Cofree.unfold(api.Sequent.proof(p))(coalg)

    def proof[F: Form, T: Term.Aux[F]](p: F): List[T] = 
        Mu.hylo[SearchF[F]](coalg, alg)(api.Sequent.proof(p)) 

        