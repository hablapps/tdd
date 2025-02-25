package tdd
package calculus

import cats.* 
import cats.syntax.all.*
import cats.arrow.*
import tdd.util.*
import SearchSpace.given 
import SearchSpace.SearchF
import Calculus.given 
import lambda.*

type Proof[F, C[_, _]] = Mu[[t] =>> C[F, t]]

object Proof: 
        
    extension [F](form: F)(using F: Form[F])

        def allProofs[C[_, _]](using C: Calculus[C])(
                search: SearchStrategy = SearchStrategy.ID): LazyList[Proof[F, C]] = 
            search(Mu.unfold(Calculus.coalg)(Sequent.initial(form)))
        
        def someProof[C[_, _]](using Calculus[C])(
                search: SearchStrategy = SearchStrategy.ID): Option[Proof[F, C]] = 
            allProofs(search).headOption
        
        def allPrograms[T: Term.Aux[F], C[_, _]](using Calculus[C])(
                search: SearchStrategy = SearchStrategy.ID): LazyList[T] = 
            form.allProofs(search).map(_.program)

        def someProgram[T: Term.Aux[F], C[_, _]](using Calculus[C])(
                search: SearchStrategy = SearchStrategy.ID): Option[T] = 
            allPrograms(search).headOption
            
    extension [F: Form, C[_, _]: Calculus](proof: Proof[F, C])
        def program[T: Term.Aux[F]]: T = 
            Mu.fold(Calculus.alg)(proof)

        