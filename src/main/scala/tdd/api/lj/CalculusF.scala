package tdd
package api
package lj

import cats.* 
import cats.syntax.all.*
import tdd.util.*
import scala.io.AnsiColor.*
import Sequent.*

enum CalculusF[F: Form, T]: 
    case Fail[F: Form, T]() extends  CalculusF[F, T]
    case LeftFalse[F: Form, T](v: Int) extends CalculusF[F, T]
    case Axiom[F: Form, T](v: (Int, F)) extends CalculusF[F, T]
    case RightImplication[F: Form, T](v: (Int, F), deriv: T) extends  CalculusF[F, T]
    case LeftImplication[F: Form, T](v1: (Int, F), deriv1: T, v2: (Int, F), deriv2: T) extends  CalculusF[F, T]
    case RightConjunction[F: Form, T](deriv1: T, deriv2: T) extends  CalculusF[F, T]
    case LeftConjunction[F: Form, T](v: (Int, F), v1: (Int, F), v2: (Int, F), deriv: T) extends  CalculusF[F, T]
    case RightDisjunctionL[F: Form, T](b: F, deriv: T) extends  CalculusF[F, T]
    case RightDisjunctionR[F: Form, T](a: F, deriv: T) extends  CalculusF[F, T]
    case LeftDisjunction[F: Form, T](v: (Int, F), v1: (Int, F), deriv1: T, v2: (Int, F), deriv2: T) extends  CalculusF[F, T]

object CalculusF: 

    type Aux[F] = [t] =>> CalculusF[F, t]

    given [F: Form]: Traverse[Aux[F]] with 
        def traverse[G[_]: Applicative, A, B](fa: CalculusF[F, A])(f: A => G[B]): G[CalculusF[F, B]] = 
            fa match
                case Fail() => Fail().pure
                case LeftFalse(v) => LeftFalse(v).pure
                case Axiom(v) => Axiom(v).pure
                case RightImplication(v, t) => f(t).map(RightImplication(v, _))
                case LeftImplication(v1, t1, v2, t2) => (f(t1), f(t2)).mapN(LeftImplication(v1, _, v2, _))
                case RightConjunction(t1, t2) => (f(t1), f(t2)).mapN(RightConjunction.apply)
                case LeftConjunction(v, v1, v2, t) => f(t).map(LeftConjunction(v, v1, v2, _))
                case RightDisjunctionL(b, t) => f(t).map(RightDisjunctionL(b, _))
                case RightDisjunctionR(a, t) => f(t).map(RightDisjunctionR(a, _))
                case LeftDisjunction(v, v1, t1, v2, t2) => (f(t1), f(t2)).mapN(LeftDisjunction(v, v1, _, v2, _))

        def foldLeft[A, B](fa: CalculusF[F, A], b: B)(f: (B, A) => B): B = ??? 

        def foldRight[A, B](fa: CalculusF[F, A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = ??? 

    def coalg[F: Form]: Coalgebra[Sequent[F], CalculusF.Aux[F]] =
        case GenSeq(seq) => 
            Fail()

        case AxiomSeq(ant, con, v) => 
            Axiom(v)
            
        case LeftFalseSeq(ant, con, v) => 
            LeftFalse(v)
            
        case seq@RightImplSeq(ant, (a, b)) => 
            val v = seq.nextVar
            RightImplication(
                (v, a), 
                GenSeq(api.Sequent(seq.ant :+ (v, a), b)))

        case seq@LeftImplSeq(ant1, (v1, (a, b)), ant2, con) => 
            val v2 = seq.nextVar
            LeftImplication(
                (v1, a implies b), 
                GenSeq(api.Sequent(ant1 ++ ant2 :+ (v1, a implies b), a)), 
                (v2, b), 
                GenSeq(api.Sequent(ant1 ++ (ant2) :+ (v2, b), con)))

        case RightAndSeq(ant, (a, b)) => 
            RightConjunction(
                GenSeq(api.Sequent(ant, a)), 
                GenSeq(api.Sequent(ant, b)))

        case seq@LeftAndSeq(ant1, (v, (a, b)), ant2, con) => 
            val v1 = seq.nextVar
            val v2 = v1 + 1
            LeftConjunction(
                (v, a and b), 
                (v1, a), 
                (v2, b), 
                GenSeq(api.Sequent(ant1 ++ ant2 :+ (v1, a) :+ (v2, b), con)))

        case RightDisjunctionSeqL(ant, (a, b)) => 
            RightDisjunctionL(b, GenSeq(api.Sequent(ant, a)))

        case RightDisjunctionSeqR(ant, (a, b)) => 
            RightDisjunctionR(a, GenSeq(api.Sequent(ant, b)))

        case seq@LeftDisjunctionSeq(ant1, (v, (a, b)), ant2, con) => 
            val v1 = seq.nextVar
            val v2 = v1 + 1
            LeftDisjunction(
                (v, a or b), 
                (v1, a), GenSeq(api.Sequent(ant1 ++ ant2 :+ (v1, a), con)), 
                (v2, b), GenSeq(api.Sequent(ant1 ++ ant2 :+ (v2, b), con)))

        
    def alg[F: Form, T](using Term[T, F]): Algebra[CalculusF.Aux[F], Option[T]] = 
        case Fail() => 
            None
        
        case LeftFalse(v) => 
            Some(v.`var`)

        case Axiom(v) => 
            Some(v._1.`var`)
        
        case RightImplication(v, t) => 
            t.map(v.lam)
        
        case LeftImplication(v1, d1, v2, d2) => 
            for 
                t1 <- d1
                t2 <- d2
            yield t2.subst(v2._1, v1._1.`var`.apply(t1))
        
        case RightConjunction(d1, d2) => 
            for 
                t1 <- d1
                t2 <- d2
            yield t1 `and` t2
            
        case LeftConjunction((v, _), (v1, _), (v2, _), d) => 
            d.map: t => 
                t.subst(v2, v.`var`._2) 
                    .subst(v1, v.`var`._1)

        case RightDisjunctionL(b, t) => 
            t.map(_.inl(b))
        
        case RightDisjunctionR(a, t) => 
            t.map(_.inr(a))

        case LeftDisjunction((v, _), (v1, tpe1), d1, (v2, tpe2), d2) => 
            for 
                t1 <- d1
                t2 <- d2
            yield v.`var`.`match`((v1, tpe1).lam(t1), (v2, tpe2).lam(t2))
        
            
    given toTree[F: Form: Show]: Algebra[CalculusF.Aux[F], Tree[Any]] =
        case Fail() => 
            Tree(s"${RED}X${RESET}", Nil)

        case LeftFalse(v) => 
            Tree(s"False ($v)", Nil)

        case Axiom(v, tpe) => 
            Tree(s"Ax ($v: ${tpe.show})", Nil)

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

        
        
