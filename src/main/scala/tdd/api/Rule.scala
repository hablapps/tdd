package tdd.api

trait Rule[R[_, _]]: 

    def coalg[F: Form]: PartialFunction[Sequent[F], R[F, Sequent[F]]]

    def alg[F: Form, T: Term.Aux[F]]: R[F, T] => T  

object Rule: 

    def apply[R[_, _]](using R: Rule[R]): Rule[R] = R
