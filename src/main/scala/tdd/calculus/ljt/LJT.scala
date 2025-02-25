package tdd
package calculus
package ljt

import cats.* 
import cats.syntax.all.*
import tdd.util.*
import scala.io.AnsiColor.*

import lj.{Calculus_LJ, LJ}
import Sequent.Prem
import lambda.*
import Form.*

enum LJT[F: Form, T]: 

    // LJ rules

    case Axiom(s: LJ.Axiom[F, T])(using Form[F])
    case LeftFalse(s: LJ.LeftFalse[F, T])(using Form[F])
    case LeftConjunction(s: LJ.LeftConjunction[F, T])(using Form[F])
    case RightConjunction(s: LJ.RightConjunction[F, T])(using Form[F])
    case LeftDisjunction(s: LJ.LeftDisjunction[F, T])(using Form[F])
    case RightDisjunctionL(s: LJ.RightDisjunctionL[F, T])(using Form[F])
    case RightDisjunctionR(s: LJ.RightDisjunctionR[F, T])(using Form[F])
    case RightImplication(s: LJ.RightImplication[F, T])(using Form[F])
    
    // LJT Left implication rules

    case LeftImplication1(
        p: Prem[F], gamma: List[Prem[F]], g: F, 
        t1: T,
        x: Prem[F], t2: T)(using 
        Form[F])

    case LeftImplication2(
        p: Prem[F], gamma: List[Prem[F]], g: F, 
        x: Prem[F], t: T)(using 
        Form[F])

    case LeftImplication3(
        p: Prem[F], gamma: List[Prem[F]], g: F, 
        x: Prem[F], y: Prem[F], t: T)(using 
        Form[F])

    case LeftImplication4(
        p: Prem[F], gamma: List[Prem[F]], g: F, 
        z: Prem[F], t1: T, 
        x: Prem[F], t2: T)(using 
        Form[F])

object LJT: 

    type Aux[F] = [t] =>> LJT[F, t]

    given Calculus[LJT] = Calculus_LJT

    given Rule[LeftImplication1] with 

        def coalg[F: Form]: PartialFunction[Sequent[F], LeftImplication1[F, Sequent[F]]] = 
            case seq@Sequent((p@(_, f@Implies(a@Atom(), b))) :: gamma, g) =>
                val x = (seq.nextVar, b)
                LeftImplication1(
                    p, gamma, g, 
                    Sequent(gamma, a), 
                    x, Sequent(x :: gamma, g))

        def alg[F: Form, T: Term.Aux[F]]: LeftImplication1[F, T] => T = 
            case LeftImplication1(p, gamma, g, t1, x, t2) => 
                t2.subst(x._1, p.apply(t1))
        
    given Rule[LeftImplication2] with 

        def coalg[F: Form]: PartialFunction[Sequent[F], LeftImplication2[F, Sequent[F]]] = 
            case seq@Sequent((p@(_, Implies(And(c, d), b))) :: gamma, g) =>
                val x = (seq.nextVar, c implies (d implies b))
                LeftImplication2(
                    p, gamma, g, 
                    x, Sequent(x :: gamma, g))

        def alg[F: Form, T: Term.Aux[F]]: LeftImplication2[F, T] => T = 
            case LeftImplication2(p, _, _, x, t) => 
                t.subst(x._1, p.curried)
        
    given Rule[LeftImplication3] with 

        def coalg[F: Form]: PartialFunction[Sequent[F], LeftImplication3[F, Sequent[F]]] = 
            case seq@Sequent((p@(_, Implies(Or(c, d), b))) :: gamma, g) =>
                val x = (seq.nextVar, c implies b)
                val y = (x._1 + 1, d implies b)
                LeftImplication3(
                    p, gamma, g, 
                    x, y, Sequent(x :: y :: gamma, g))

        def alg[F: Form, T: Term.Aux[F]]: LeftImplication3[F, T] => T = 
            case LeftImplication3(p, _, _, x, y, t) => 
                t.subst(x._1, p.left)
                    .subst(y._1, p.right)
    
    given Rule[LeftImplication4] with 

        def coalg[F: Form]: PartialFunction[Sequent[F], LeftImplication4[F, Sequent[F]]] = 
            case seq@Sequent((p@(_, Implies(Implies(c, d), b))) :: gamma, g) =>
                val z: Prem[F] = (seq.nextVar, d implies b)
                val x: Prem[F] = (z._1 + 1, b)
                LeftImplication4(
                    p, gamma, g,
                    z, Sequent(z :: gamma, c implies d), 
                    x, Sequent(x :: gamma, g))

        def alg[F: Form, T: Term.Aux[F]]: LeftImplication4[F, T] => T = 
            case LeftImplication4(p, _, _, z, q, x, g) => 
                g.subst(x._1, p.apply_imp(z.lam(q)))
        
    // REUSE FROM LJ 

    given [C[f, t] <: LJ[f, t], R[f, t] <: LJT[f, t]](
        using Iso: Iso[C, R], C: Rule[C]): Rule[R] with 
    
        def coalg[F: Form]: PartialFunction[Sequent[F], R[F, Sequent[F]]] = 
            Rule[C].coalg[F] andThen Iso.from

        def alg[F: Form, T: Term.Aux[F]]: R[F, T] => T = 
            Iso.to andThen Rule[C].alg[F, T]
        
    given Iso[LJ.Axiom, Axiom] = Iso([f, t] => _ ?=> Axiom.apply, [f, t] => _ ?=> _.s)
    given Iso[LJ.LeftFalse, LeftFalse] = Iso([f, t] => _ ?=> LeftFalse.apply, [f, t] => _ ?=> _.s)
    given Iso[LJ.RightDisjunctionL, RightDisjunctionL] = Iso([f, t] => _ ?=> RightDisjunctionL.apply, [f, t] => _ ?=> _.s)
    given Iso[LJ.RightDisjunctionR, RightDisjunctionR] = Iso([f, t] => _ ?=> RightDisjunctionR.apply, [f, t] => _ ?=> _.s)
    given Iso[LJ.LeftDisjunction, LeftDisjunction] = Iso([f, t] => _ ?=> LeftDisjunction.apply, [f, t] => _ ?=> _.s)
    given Iso[LJ.RightImplication, RightImplication] = Iso([f, t] => _ ?=> RightImplication.apply, [f, t] => _ ?=> _.s)
    given Iso[LJ.RightConjunction, RightConjunction] = Iso([f, t] => _ ?=> RightConjunction.apply, [f, t] => _ ?=> _.s)
    given Iso[LJ.LeftConjunction, LeftConjunction] = Iso([f, t] => _ ?=> LeftConjunction.apply, [f, t] => _ ?=> _.s)

