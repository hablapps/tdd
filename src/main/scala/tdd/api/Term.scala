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

