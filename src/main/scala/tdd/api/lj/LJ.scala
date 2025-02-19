package tdd
package api
package lj

import cats.* 
import cats.syntax.all.*
import tdd.util.*
import scala.io.AnsiColor.*

import Form.*

enum LJ[F: Form, T]: 
    case LeftFalse(v: Int)(using Form[F])
    case Axiom(v: (Int, F))(using Form[F])
    case RightImplication(v: (Int, F), deriv: T)(using Form[F])
    case LeftImplication(v1: (Int, F), deriv1: T, v2: (Int, F), deriv2: T)(using Form[F])
    case RightConjunction(deriv1: T, deriv2: T)(using Form[F])
    case LeftConjunction(v: (Int, F), v1: (Int, F), v2: (Int, F), deriv: T)(using Form[F])
    case RightDisjunctionL(b: F, deriv: T)(using Form[F])
    case RightDisjunctionR(a: F, deriv: T)(using Form[F])
    case LeftDisjunction(v: (Int, F), v1: (Int, F), deriv1: T, v2: (Int, F), deriv2: T)(using Form[F])

object LJ: 

    type Aux[F] = [t] =>> LJ[F, t]

    given Rule[Axiom] with 

        def coalg[F: Form]: PartialFunction[Sequent[F], Axiom[F, Sequent[F]]] = 
            case Sequent(v :: _, con) if v._2 == con => 
                Axiom(v)

        def alg[F: Form, T: Term.Aux[F]]: LJ.Axiom[F, T] => T = 
            case LJ.Axiom(v) => v._1.`var`
        

    given Rule[LeftFalse] with

        def coalg[F: Form]: PartialFunction[Sequent[F], LeftFalse[F, Sequent[F]]] =
            case Sequent((v, False()) :: _, _) => 
                LeftFalse(v)

        def alg[F: Form, T: Term.Aux[F]]: LJ.LeftFalse[F, T] => T = 
            case LeftFalse(v) => v.`var`
    

    given Rule[RightImplication] with 

        def coalg[F: Form]: PartialFunction[Sequent[F], RightImplication[F, Sequent[F]]] = 
            case seq@Sequent(ant, Implies(a, b)) => 
                val v = seq.nextVar
                RightImplication(
                    (v, a), 
                    Sequent(seq.ant :+ (v, a), b))

        def alg[F: Form, T: Term.Aux[F]]: LJ.RightImplication[F, T] => T = 
            case RightImplication(v, t) => v.lam(t)
            

    given Rule[LeftImplication] with 

        def coalg[F: Form]: PartialFunction[Sequent[F], LeftImplication[F, Sequent[F]]] = 
            case seq@Sequent((v1, f@Implies(a, b)) :: ant, con) =>
                val v2 = seq.nextVar
                LeftImplication(
                    (v1, f), 
                    Sequent(ant :+ (v1, f), a), 
                    (v2, b), 
                    Sequent(ant :+ (v2, b), con))

        def alg[F: Form, T: Term.Aux[F]]: LJ.LeftImplication[F, T] => T = 
            case LeftImplication(v1, t1, v2, t2) => t2.subst(v2._1, v1._1.`var`.apply(t1))
        

    given Rule[RightDisjunctionL] with 

        def coalg[F: Form]: PartialFunction[Sequent[F], RightDisjunctionL[F, Sequent[F]]] = 
            case Sequent(ant, Or(a, b)) => 
                RightDisjunctionL(b, Sequent(ant, a))
        
        def alg[F: Form, T: Term.Aux[F]]: LJ.RightDisjunctionL[F, T] => T = 
            case RightDisjunctionL(b, t) => t.inl(b)
            
                
    given Rule[RightDisjunctionR] with 

        def coalg[F: Form]: PartialFunction[Sequent[F], RightDisjunctionR[F, Sequent[F]]] = 
            case Sequent(ant, Or(a, b)) => 
                RightDisjunctionR(a, Sequent(ant, b))

        def alg[F: Form, T: Term.Aux[F]]: LJ.RightDisjunctionR[F, T] => T = 
            case RightDisjunctionR(a, t) => t.inr(a)

                
    given Rule[LeftDisjunction] with 

        def coalg[F: Form]: PartialFunction[Sequent[F], LeftDisjunction[F, Sequent[F]]] = 
            case seq@Sequent((v, f@Or(a, b)) :: ant, con) =>
                val v1 = seq.nextVar
                val v2 = v1 + 1
                LeftDisjunction(
                    (v, f), 
                    (v1, a), Sequent(ant :+ (v1, a), con), 
                    (v2, b), Sequent(ant :+ (v2, b), con))

        def alg[F: Form, T: Term.Aux[F]]: LJ.LeftDisjunction[F, T] => T = 
            case LeftDisjunction((v, _), (v1, tpe1), t1, (v2, tpe2), t2) => 
                v.`var`.`match`((v1, tpe1).lam(t1), (v2, tpe2).lam(t2))
        
    given Rule[RightConjunction] with 

        def coalg[F: Form]: PartialFunction[Sequent[F], RightConjunction[F, Sequent[F]]] = 
            case Sequent(ant, And(a, b)) =>
                RightConjunction(
                    Sequent(ant, a), 
                    Sequent(ant, b))

        def alg[F: Form, T: Term.Aux[F]]: LJ.RightConjunction[F, T] => T = 
            case RightConjunction(t1, t2) => t1 `and` t2

    given Rule[LeftConjunction] with 

        def coalg[F: Form]: PartialFunction[Sequent[F], LeftConjunction[F, Sequent[F]]] = 
            case seq@Sequent((v, And(a, b)) :: ant, con) => 
                val v1 = seq.nextVar
                val v2 = v1 + 1
                LeftConjunction(
                    (v, a and b), 
                    (v1, a), 
                    (v2, b), 
                    Sequent(ant :+ (v1, a) :+ (v2, b), con))

        def alg[F: Form, T: Term.Aux[F]]: LJ.LeftConjunction[F, T] => T = 
            case LeftConjunction((v, _), (v1, _), (v2, _), t) => 
                t.subst(v2, v.`var`._2) 
                    .subst(v1, v.`var`._1)


    given [F: Form]: Traverse[Aux[F]] with 
        def traverse[G[_]: Applicative, A, B](fa: LJ[F, A])(f: A => G[B]): G[LJ[F, B]] = 
            fa match
                case Axiom(v) => Axiom(v).pure
                case LeftFalse(v) => LeftFalse(v).pure
                case RightImplication(v, t) => f(t).map(RightImplication(v, _))
                case LeftImplication(v1, t1, v2, t2) => (f(t1), f(t2)).mapN(LeftImplication(v1, _, v2, _))
                case RightConjunction(t1, t2) => (f(t1), f(t2)).mapN(RightConjunction.apply)
                case LeftConjunction(v, v1, v2, t) => f(t).map(LeftConjunction(v, v1, v2, _))
                case RightDisjunctionL(b, t) => f(t).map(RightDisjunctionL(b, _))
                case RightDisjunctionR(a, t) => f(t).map(RightDisjunctionR(a, _))
                case LeftDisjunction(v, v1, t1, v2, t2) => (f(t1), f(t2)).mapN(LeftDisjunction(v, v1, _, v2, _))

        def foldLeft[A, B](fa: LJ[F, A], b: B)(f: (B, A) => B): B = ??? 

        def foldRight[A, B](fa: LJ[F, A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = ??? 

    def alg[F: Form, T](using Term[T, F]): Algebra[LJ.Aux[F], T] = 
        case f: Axiom[F, T] => Rule[Axiom].alg(f)
        case f: LeftFalse[F, T] => Rule[LeftFalse].alg(f)
        case f: RightImplication[F, T] => Rule[RightImplication].alg(f)
        case f: LeftImplication[F, T] => Rule[LeftImplication].alg(f)
        case f: RightConjunction[F, T] => Rule[RightConjunction].alg(f)
        case f: LeftConjunction[F, T] => Rule[LeftConjunction].alg(f)
        case f: RightDisjunctionL[F, T] => Rule[RightDisjunctionL].alg(f)
        case f: RightDisjunctionR[F, T] => Rule[RightDisjunctionR].alg(f)
        case f: LeftDisjunction[F, T] => Rule[LeftDisjunction].alg(f)
        
    given toTree[F: Form: Show]: Algebra[LJ.Aux[F], Tree[Any]] =
        
        case Axiom(v, tpe) => 
            Tree(s"Ax ($v: ${tpe.show})", Nil)

        case LeftFalse(v) => 
            Tree(s"False ($v)", Nil)

        case RightImplication((v, tpe), d) => 
            Tree(s"->R ($v: ${tpe.show})", List(d))

        case LeftImplication((v1, tpe1), d1, (v2, tpe2), d2) => 
            Tree(s"->L ($v1: ${tpe1.show}, $v2: ${tpe2.show})", List(d1, d2))

        case RightConjunction(d1, d2) => 
            Tree(s"& R", List(d1, d2))

        case LeftConjunction((v, tpev), (v1, tpe1), (v2, tpe2), d) => 
            Tree(s"& L ($v: ${tpev.show}, $v1: ${tpe1.show}, $v2: ${tpe2.show})", List(d))

        case RightDisjunctionL(tpe, d) => 
            Tree(s"v RL (${tpe.show})", List(d))

        case RightDisjunctionR(tpe, d) => 
            Tree(s"v RR (${tpe.show})", List(d))

        case LeftDisjunction((v, tpev), (v1, tpe1), d1, (v2, tpe2), d2) => 
            Tree(s"v L ($v: ${tpev.show}, $v1: ${tpe1.show}, $v2: ${tpe2.show})", List(d1, d2))

        
        
