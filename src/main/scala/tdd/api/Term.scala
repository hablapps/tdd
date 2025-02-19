package tdd.api 

trait Term[T, TF: Form]: 
    
    extension (v: Int)
        def `var`: T

    extension (v: (Int, TF))
        def lam(b: T): T
        
    extension (t1: T)
        def inl(tpe: TF): T
        def inr(tpe: TF): T
        def `match`(inl: T, inr: T): T
        def and(t2: T): T
        def _1: T 
        def _2: T
        def apply(a: T): T
        def subst(i: Int, t: T): T

object Term: 

    type Aux[TF] = [t] =>> Term[t, TF]

    object Syntax: 
        given [F: Form, T: Aux[F]]: Conversion[Int, T] = _.`var`

        given [F: Form, T: Aux[F]]: Conversion[T, T] = identity
        
        given [F: Form, T: Aux[F], A, B](using A: Conversion[A, T], B: Conversion[B, T]): Conversion[(A, B), T] = 
            case (a, b) => A(a) `and` b

        extension [F: Form, T, A](f: A)(using C: Conversion[A, T], T: Term[T, F])
            def apply(a: T): T = T.apply(f)(a)
            def _1: T = T._1(f)
            def _2: T = T._2(f)
            def inl(tpe: F): T = T.inl(f)(tpe)
            def inr(tpe: F): T = T.inr(f)(tpe)
            def `match`(inl: T, inr: T): T = T.`match`(f)(inl, inr)
        
        