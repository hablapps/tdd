package tdd
package lambda

import Form.*

trait Term[F: Form, T]: 
    
    extension (v: Int)
        def `var`: T

    extension (p: (Int, F))
        def lam(b: T): T

    extension (t1: T)
        def inl(tpe: F): T
        def inr(tpe: F): T
        def `match`(inl: T, inr: T): T
        def `and`(t2: T): T
        def _1: T 
        def _2: T
        def apply(a: T): T
        def subst(i: Int, t: T): T

    // Derived

    extension (v: Int)
        def apply(a: T): T = 
            v.`var`(a)

    extension (p: (Int, F))
    
        def `var`: T = 
            p._1.`var`

        def apply(t: T): T = 
            p._1(t)

        def curried: T = 
            p match 
                case (p, Form.Implies(Form.And(a, b), c)) => 
                    (p+1, a).lam(
                        (p+2, b).lam(
                            p((p+1).`var` `and` (p+2).`var`)
                        ))

        def apply_imp(q: T): T =
            p match 
                case (p, Implies(Implies(c, d), b)) =>   
                    p(q((p+1, d).lam(
                        p((p+2, c).lam(
                            (p+1).`var`)))))

        def left: T = 
            p match 
                case (p, Implies(Or(c, d), b)) => 
                    (p+1, c).lam(p((p+1).`var`.inl(d)))

        def right: T = 
            p match 
                case (p, Implies(Or(c, d), b)) => 
                    (p+1, d).lam(p((p+1).`var`.inr(c)))
                 
    
object Term: 

    def apply[F: Form, T: Term.Aux[F]](using t: Term[F, T]): Term[F, T] = t

    type Aux[F] = [t] =>> Term[F, t]

    object Syntax: 
        given [F: Form, T: Aux[F]]: Conversion[Int, T] = _.`var`

        given [F: Form, T: Aux[F]]: Conversion[T, T] = identity
        
        given [F: Form, T: Aux[F], A, B](using A: Conversion[A, T], B: Conversion[B, T]): Conversion[(A, B), T] = 
            case (a, b) => A(a) `and` b

        extension [F: Form, T, A](f: A)(using C: Conversion[A, T], T: Term[F, T])
            def apply(a: T): T = T.apply(f)(a)
            def _1: T = T._1(f)
            def _2: T = T._2(f)
            def inl(tpe: F): T = T.inl(f)(tpe)
            def inr(tpe: F): T = T.inr(f)(tpe)
            def `match`(inl: T, inr: T): T = T.`match`(f)(inl, inr)
        
        