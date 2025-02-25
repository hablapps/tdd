package tdd
package calculus

import util.*
import SearchSpace.SearchF
import cats.*
import lambda.*

trait Calculus[C[_, _]]: 

    given IsTraverse[F: Form]: Traverse[[t] =>> C[F, t]]

    def coalg[F: Form]: Coalgebra[Sequent[F], SearchF[F, C]]

    def alg[F: Form, T](using Term[F, T]): Algebra[[t] =>> C[F, t], T]

    given toTree[F: Form: Show]: Algebra[[t] =>> C[F, t], Tree[Any]]

object Calculus: 

    def apply[C[_, _]](using C: Calculus[C]): Calculus[C] = C

    given IsTraverse[F: Form, C[_, _]](using C: Calculus[C]): Traverse[[t] =>> C[F, t]] = 
        C.IsTraverse[F]

    def coalg[F: Form, C[_, _]](using C: Calculus[C]): Coalgebra[Sequent[F], SearchF[F, C]] = 
        C.coalg[F]
    
    def alg[F: Form, T: Term.Aux[F], C[_, _]](using C: Calculus[C]): Algebra[[t] =>> C[F, t], T] = 
        C.alg[F, T]

    given toTree[F: Form: Show, C[_, _]](using C: Calculus[C]): Algebra[[t] =>> C[F, t], Tree[Any]] = 
        C.toTree[F]
