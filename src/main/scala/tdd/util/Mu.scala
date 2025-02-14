package tdd
package util

import cats.* 
import cats.syntax.all.*

case class Mu[F[_]](out: () => F[Mu[F]])

object Mu:

    def in[F[_]](f: => F[Mu[F]]): Mu[F] = Mu(() => f)

    // recursion schemes

    def fold[F[_]: Functor, A](alg: Algebra[F, A]): Mu[F] => A = 
        case Mu(f) => alg(f().map(fold(alg)))

    def unfold[F[_]: Functor, A](coalg: Coalgebra[A, F]): A => Mu[F] =
        a => Mu(() => coalg(a).map(unfold(coalg)))

    trait hylo[F[_]]:
        def apply[A, B](coalg: Coalgebra[A, F], alg: Algebra[F, B])(using Functor[F]): A => B = 
            unfold(coalg) andThen fold(alg)

    object hylo: 
        def apply[F[_]] = new hylo[F]{}

    // map analogue for Mu and F

    def mapF[F[_]: Functor, G[_]](nat: F ~> G)(cf: Mu[F]): Mu[G] = 
        Mu(() => nat(cf.out().map(mapF(nat))))

    // mapFilterF: Not exactly the analogue of mapFilter for Mu and F

    def mapFilterFRec[F[_]: FunctorFilter, H[_]](nat: F ~> Compose[Option, H])(rec: Mu[F] => Option[Mu[H]])(cf: Mu[F]): Option[Mu[H]] = 
        nat(cf.out().mapFilter(rec))
            .map(Mu.in)
        
    def mapFilterF[F[_]: FunctorFilter, H[_]](nat: F ~> Compose[Option, H])(cf: Mu[F]): Option[Mu[H]] = 
        mapFilterFRec(nat)(mapFilterF(nat))(cf)
        
    def mapFilterFUntil[F[_]: FunctorFilter, H[_]](depth: Int)(nat: F ~> Compose[Option, H])(cf: Mu[F]): Option[Mu[H]] = 
        if depth == 0 then None
        else mapFilterFRec(nat)(mapFilterFUntil(depth-1)(nat))(cf)

    // sequence anologue for Mu and G Compose H

    def sequenceF_Rec[G[_]: Monad, H[_]: Traverse](rec: Mu[G Compose H] => G[Mu[H]])(ss: Mu[G Compose H]): G[Mu[H]] = 
        Monad[G].flatMap(ss.out()): 
            _.traverse(rec)
                .map(Mu.in)
    
    def sequenceF[G[_]: Monad, H[_]: Traverse](ss: Mu[G Compose H]): G[Mu[H]] = 
        sequenceF_Rec(sequenceF[G, H])(ss)
    
    def sequenceF_Until[G[_]: Monad: Alternative, H[_]: Traverse](depth: Int)(ss: Mu[G Compose H]): G[Mu[H]] = 
        if depth == 0 then Alternative[G].empty
        else sequenceF_Rec(sequenceF_Until[G, H](depth-1))(ss)

    // traverse analogue for Mu and F   
     
    def traverseF[F[_]: Functor, G[_]: Monad, H[_]: Traverse](nat: F ~> Compose[G, H])(ss: Mu[F]): G[Mu[H]] = 
        sequenceF(mapF(nat)(ss))

    // Show instance

    given [F[_]: Functor](using toTree: Algebra[F, Tree[Any]]): Show[Mu[F]] with 
        def show(t: Mu[F]): String =
            fold(toTree)(t).show

    