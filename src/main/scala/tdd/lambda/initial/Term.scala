package tdd
package lambda
package initial

import cats.Show
import cats.syntax.show.* 

enum Term: 
    case Var(i: Int) extends Term
    case App(f: Term, a: Term) extends Term
    case Lam(v: (Int, Form), b: Term) extends Term
    case And(t1: Term, t2: Term) extends Term 
    case Prj1(t: Term) extends Term
    case Prj2(t: Term) extends Term
    case InL(t: Term) extends Term
    case InR(t: Term) extends Term
    case Match(t: Term, inl: Term, inr: Term) extends Term 

    def subst(v: Int, t: Term): Term = 
        this match 
            case Var(`v`) => t
            case _: Var => this
            case And(t1, t2) => And(t1.subst(v, t), t2.subst(v, t))
            case Prj1(e) => Prj1(e.subst(v, t))
            case Prj2(e) => Prj2(e.subst(v, t))
            case InL(e) => InL(e.subst(v, t))
            case InR(e) => InR(e.subst(v, t))
            case Match(e, f, g) => Match(e.subst(v, t), f.subst(v, t), g.subst(v, t))
            case App(t1, t2) => App(t1.subst(v, t), t2.subst(v, t))
            case Lam((`v`, _), _) => this 
            case Lam(w, t1) => Lam(w, t1.subst(v, t))

object Term: 

    given cats.Eq[Term] with 
        def eqv(x: Term, y: Term): Boolean =
            x == y

    given lambda.Term[Form, Term] with 

        extension (v: Int)
            def `var`: Term = Var(v)
        
        extension (v: (Int, Form))
            def lam(b: Term): Term = Lam(v, b)

        extension (t1: Term)
            
            def and(t2: Term): Term = And(t1, t2)

            def _1: Term = Prj1(t1)

            def _2: Term = Prj2(t1)

            def inl(tpe: Form): Term = InL(t1)

            def inr(tpe: Form): Term = InR(t1)

            def `match`(inl: Term, inr: Term): Term = 
                Match(t1, inl, inr)

            def apply(t2: Term): Term = App(t1, t2)

            def subst(i: Int, t2: Term): Term = t1.subst(i, t2)

    given Show[Term] with 
        def show(t: Term): String =
            t match
                case Var(i) => s"$i"
                case And(t1, t2) => s"(${t1.show}, ${t2.show})"
                case Prj1(t) => s"${t.show}._1"
                case Prj2(t) => s"${t.show}._2"
                case InL(t) => s"inl(${t.show})"
                case InR(t) => s"inr(${t.show})"
                case Match(t, f, g) => s"${t.show} match (${f.show}, ${g.show})"
                case App(Var(v), t2) => s"$v(${t2.show})"
                case App(t1, t2) => s"(${t1.show})(${t2.show})"
                case Lam(v, t) => s"λ${v._1}: ${v._2.show}. ${t.show}"
