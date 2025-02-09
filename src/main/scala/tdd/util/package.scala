package tdd

package object util:

    type Algebra[F[_], A] = F[A] => A

    type Coalgebra[A, F[_]] = A => F[A]
