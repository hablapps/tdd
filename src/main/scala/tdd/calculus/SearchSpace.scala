package tdd
package calculus

import cats.data.* 
import cats.*
import cats.free.*
import cats.syntax.all.*
import tdd.util.*
import Calculus.given
import lambda.*

type SearchSpace[F, C[_, _]] = Mu[[t] =>> LazyList[C[F, t]]]

object SearchSpace: 

    type SearchF[F, C[_, _]] = [t] =>> LazyList[C[F, t]]

    given Functor_SearchF[F: Form, C[_, _]](using Functor[[t] =>> C[F, t]]): Functor[SearchF[F, C]] = 
        Functor[LazyList].compose[[t] =>> C[F, t]]

    extension [A, B](f: PartialFunction[A, B])
        def apply(l: LazyList[A]): LazyList[B] = 
            l.flatMap(f.lift andThen LazyList.from)

    def alg[F: Form, T: Term.Aux[F], C[_, _]: Calculus]: Algebra[SearchF[F, C], LazyList[T]] = 
        _.flatMap(_.sequence[LazyList, T].map(Calculus.alg))

    given toTree[F: Form: Show, C[_, _]: Calculus]: Algebra[SearchF[F, C], Tree[Any]] = 
        nel => Tree((), nel.toList.map(Calculus.toTree))

    def apply[F: Form, C[_, _]: Calculus](p: F): SearchSpace[F, C] = 
        Mu.unfold(Calculus.coalg)(Sequent.initial(p))

    def trace[F: Form, C[_, _]: Calculus](p: F): Cofree[SearchF[F, C], Sequent[F]] =
        Cofree.unfold(Sequent.initial(p))(Calculus.coalg)

    def allPrograms[F: Form, T: Term.Aux[F], C[_, _]: Calculus](p: F): LazyList[T] = 
        Mu.hylo[SearchF[F, C]](Calculus.coalg, alg)(Sequent.initial(p)) 

        