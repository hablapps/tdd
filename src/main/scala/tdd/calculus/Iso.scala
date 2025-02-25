package tdd
package calculus

import lambda.* 

trait Iso[C[_, _], R[_, _]]:
        def from[F: Form, T]: C[F, T] => R[F, T]
        def to[F: Form, T]: R[F, T] => C[F, T]

object Iso: 
    def apply[C[_, _], R[_, _]](
            f: [f, t] => Form[f] ?=> C[f, t] => R[f, t], 
            t: [f, t] => Form[f] ?=> R[f, t] => C[f, t]) = 
        new Iso[C, R]: 
            def from[F: Form, T] = f[F, T]
            def to[F: Form, T] = t[F, T]

