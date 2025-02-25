package tdd
package calculus
package lj

import cats.* 
import cats.syntax.all.*
import tdd.util.*
import scala.io.AnsiColor.*

import lambda.*
import Form.*
import Sequent.Prem
import cats.kernel.instances.order

enum LJ[F: Form, T]: 

    case Axiom(
        p: Prem[F], 
        gamma: List[Prem[F]])(using 
        Form[F])

    case LeftFalse(
        p: Int, 
        gamma: List[Prem[F]], 
        g: F)(using 
        Form[F])

    case LeftImplication(
        p: Prem[F], gamma: List[Prem[F]], g: F, 
        t1: T, 
        x: Prem[F], t2: T)(using 
        Form[F])

    case RightImplication(
        p: Prem[F], gamma: List[Prem[F]], b: F, 
        t: T)(using Form[F])

    case LeftConjunction(
        p: Prem[F], gamma: List[Prem[F]], g: F, 
        x: Prem[F], y: Prem[F], t: T)(using 
        Form[F])

    case RightConjunction(
        gamma: List[Prem[F]], a: F, b: F,  
        t1: T, 
        t2: T)(using 
        Form[F])

    case LeftDisjunction(
        p : Prem[F], gamma: List[Prem[F]], g: F, 
        x: Prem[F], t1: T, 
        y: Prem[F], t2: T)(using 
        Form[F])

    case RightDisjunctionL(
        gamma: List[Prem[F]], a: F, b: F, 
        t: T)(using 
        Form[F])

    case RightDisjunctionR(
        gamma: List[Prem[F]], a: F, b: F, 
        t: T)(using 
        Form[F])
    
object LJ: 

    type Aux[F] = [t] =>> LJ[F, t]

    given Calculus[LJ] = Calculus_LJ

    given Rule[Axiom] with 

        def coalg[F: Form]: PartialFunction[Sequent[F], Axiom[F, Sequent[F]]] = 
            case Sequent(p :: gamma, g) if p._2 == g => 
                Axiom(p, gamma)

        def alg[F: Form, T: Term.Aux[F]]: Axiom[F, T] => T = 
            case Axiom(p, _) => p._1.`var`
        

    given Rule[LeftFalse] with

        def coalg[F: Form]: PartialFunction[Sequent[F], LeftFalse[F, Sequent[F]]] =
            case Sequent((p, False()) :: gamma, g) => 
                LeftFalse(p, gamma, g)

        def alg[F: Form, T: Term.Aux[F]]: LeftFalse[F, T] => T = 
            case LeftFalse(p, _, _) => p.`var`
    

    given Rule[RightImplication] with 

        def coalg[F: Form]: PartialFunction[Sequent[F], RightImplication[F, Sequent[F]]] = 
            case seq@Sequent(gamma, Implies(a, b)) => 
                val p = (seq.nextVar, a)
                RightImplication(
                    p, gamma, b, 
                    Sequent(p :: gamma, b))

        def alg[F: Form, T: Term.Aux[F]]: RightImplication[F, T] => T = 
            case RightImplication(p, _, _, t) => p.lam(t)
            

    given Rule[LeftImplication] with 

        def coalg[F: Form]: PartialFunction[Sequent[F], LeftImplication[F, Sequent[F]]] = 
            case seq@Sequent((p@(_, Implies(a, b))) :: gamma, g) =>
                val x = (seq.nextVar, b)
                LeftImplication(
                    p, gamma, g, 
                    Sequent(p :: gamma, a), 
                    x, Sequent(x :: gamma, g))

        def alg[F: Form, T: Term.Aux[F]]: LeftImplication[F, T] => T = 
            case LeftImplication(p, _, _, t1, x, t2) => 
                t2.subst(x._1, p._1.`var`.apply(t1))
        

    given Rule[RightDisjunctionL] with 

        def coalg[F: Form]: PartialFunction[Sequent[F], RightDisjunctionL[F, Sequent[F]]] = 
            case Sequent(gamma, g@Or(a, b)) => 
                RightDisjunctionL(
                    gamma, a, b,  
                    Sequent(gamma, a))
        
        def alg[F: Form, T: Term.Aux[F]]: RightDisjunctionL[F, T] => T = 
            case RightDisjunctionL(_, _, b, t) => t.inl(b)
            
                
    given Rule[RightDisjunctionR] with 

        def coalg[F: Form]: PartialFunction[Sequent[F], RightDisjunctionR[F, Sequent[F]]] = 
            case Sequent(gamma, Or(a, b)) => 
                RightDisjunctionR(
                    gamma, a, b, 
                    Sequent(gamma, b))

        def alg[F: Form, T: Term.Aux[F]]: RightDisjunctionR[F, T] => T = 
            case RightDisjunctionR(_, a, _, t) => t.inr(a)

                
    given Rule[LeftDisjunction] with 

        def coalg[F: Form]: PartialFunction[Sequent[F], LeftDisjunction[F, Sequent[F]]] = 
            case seq@Sequent((p@(_, Or(a, b))) :: gamma, g) =>
                val x = (seq.nextVar, a)
                val y = (x._1 + 1, b)
                LeftDisjunction(
                    p, gamma, g, 
                    x, Sequent(x :: gamma, g), 
                    y, Sequent(y :: gamma, g))

        def alg[F: Form, T: Term.Aux[F]]: LeftDisjunction[F, T] => T = 
            case LeftDisjunction(p, _, _, x, t1, y, t2) => 
                p.`var`.`match`(x.lam(t1), y.lam(t2))
        
    given Rule[RightConjunction] with 

        def coalg[F: Form]: PartialFunction[Sequent[F], RightConjunction[F, Sequent[F]]] = 
            case Sequent(gamma, And(a, b)) =>
                RightConjunction(
                    gamma, a, b, 
                    Sequent(gamma, a), 
                    Sequent(gamma, b))

        def alg[F: Form, T: Term.Aux[F]]: RightConjunction[F, T] => T = 
            case RightConjunction(_, _, _, t1, t2) => t1 `and` t2

    given Rule[LeftConjunction] with 

        def coalg[F: Form]: PartialFunction[Sequent[F], LeftConjunction[F, Sequent[F]]] = 
            case seq@Sequent((p@(_, And(a, b))) :: gamma, g) => 
                val x = (seq.nextVar, a)
                val y = (x._1 + 1, b)
                LeftConjunction(
                    p, gamma, g,  
                    x, y, Sequent(x :: y :: gamma, g))

        def alg[F: Form, T: Term.Aux[F]]: LeftConjunction[F, T] => T = 
            case LeftConjunction((p, _), _, _, (x, _), (y, _), t) => 
                t.subst(y, p.`var`._2) 
                    .subst(x, p.`var`._1)
        
    