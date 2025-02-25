package tdd

package object util:

    type Algebra[F[_], A] = F[A] => A

    object Algebra: 
        def apply[F[_], A](using A: Algebra[F, A]): Algebra[F, A] = A

    type Coalgebra[A, F[_]] = A => F[A]

    infix type Compose[F[_], G[_]] = [t] =>> F[G[t]]
