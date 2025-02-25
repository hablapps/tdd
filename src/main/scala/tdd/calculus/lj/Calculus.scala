package tdd
package calculus
package lj

import cats.*, cats.syntax.all.* 

import util.*
import LJ.* 
import lambda.*

given Calculus_LJ: Calculus[LJ] with

    given IsTraverse[F: Form]: Traverse[LJ.Aux[F]] with 
        def traverse[G[_]: Applicative, A, B](fa: LJ[F, A])(f: A => G[B]): G[LJ[F, B]] = 
            fa match
                case Axiom(p, gamma) => Axiom(p, gamma).pure
                case LeftFalse(v, gamma, g) => LeftFalse(v, gamma, g).pure
                case LeftImplication(p, gamma, g, t1, x, t2) => (f(t1), f(t2)).mapN(LeftImplication(p, gamma, g, _, x, _))
                case RightImplication(p, gamma, b, t) => f(t).map(RightImplication(p, gamma, b, _))
                case LeftConjunction(p, gamma, g, x, y, t) => f(t).map(LeftConjunction(p, gamma, g, x, y, _))
                case RightConjunction(gamma, a, b, t1, t2) => (f(t1), f(t2)).mapN(RightConjunction(gamma, a, b, _, _))
                case LeftDisjunction(p, gamma, g, x, t1, y, t2) => (f(t1), f(t2)).mapN(LeftDisjunction(p, gamma, g, x, _, y, _))
                case RightDisjunctionL(gamma, a, b, t) => f(t).map(RightDisjunctionL(gamma, a, b, _))
                case RightDisjunctionR(gamma, a, b, t) => f(t).map(RightDisjunctionR(gamma, a, b, _))
                
        def foldLeft[A, B](fa: LJ[F, A], b: B)(f: (B, A) => B): B = ??? 

        def foldRight[A, B](fa: LJ[F, A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = ??? 

    def alg[F: Form, T](using Term[F, T]): Algebra[LJ.Aux[F], T] = 
        case f: Axiom[F, T] => Rule[Axiom].alg(f)
        case f: LeftFalse[F, T] => Rule[LeftFalse].alg(f)
        case f: RightImplication[F, T] => Rule[RightImplication].alg(f)
        case f: LeftImplication[F, T] => Rule[LeftImplication].alg(f)
        case f: RightConjunction[F, T] => Rule[RightConjunction].alg(f)
        case f: LeftConjunction[F, T] => Rule[LeftConjunction].alg(f)
        case f: RightDisjunctionL[F, T] => Rule[RightDisjunctionL].alg(f)
        case f: RightDisjunctionR[F, T] => Rule[RightDisjunctionR].alg(f)
        case f: LeftDisjunction[F, T] => Rule[LeftDisjunction].alg(f)

        
    def coalg[F: Form]: Coalgebra[Sequent[F], SearchSpace.SearchF[F, LJ]] = seq =>
        import SearchSpace.apply
        
        val rotations: LazyList[Sequent[F]] = LazyList.from(seq.rotations)

        Rule[LJ.Axiom].coalg(rotations) ++ 
        Rule[LJ.LeftFalse].coalg(rotations) ++
        Rule[LJ.RightImplication].coalg(LazyList(seq)) ++ 
        Rule[LJ.RightDisjunctionL].coalg(LazyList(seq)) ++ 
        Rule[LJ.RightDisjunctionR].coalg(LazyList(seq)) ++ 
        Rule[LJ.RightConjunction].coalg(LazyList(seq)) ++ 
        Rule[LJ.LeftImplication].coalg(rotations) ++ 
        Rule[LJ.LeftConjunction].coalg(rotations) ++ 
        Rule[LJ.LeftDisjunction].coalg(rotations)

    given toTree[F: Form: Show]: Algebra[LJ.Aux[F], Tree[Any]] =

        case Axiom(p, gamma) => 
            Tree(s"${Sequent(p :: gamma, p._2).show} (Ax)", Nil)

        case LeftFalse(p, gamma, g) => 
            Tree(s"${Sequent((p, Form[F].False) :: gamma, g)} (False)", Nil)

        case LeftImplication(p, gamma, g, t1, x, t2) => 
            Tree(s"${Sequent(p :: gamma, g)} (->L)", List(t1, t2))

        case RightImplication(p, gamma, b, t) => 
            Tree(s"${Sequent(gamma, p._2 implies b).show} (->R)", List(t))

        case LeftConjunction(p, gamma, g, x, y, t) => 
            Tree(s"${Sequent(p :: gamma, g).show} (&L)", List(t))

        case RightConjunction(gamma, a, b, t1, t2) => 
            Tree(s"${Sequent(gamma, a and b).show} (&R)", List(t1, t2))

        case LeftDisjunction(p, gamma, g, x, t1, y, t2) => 
            Tree(s"${Sequent(p :: gamma, g).show} (vL)", List(t1, t2))

        case RightDisjunctionL(gamma, a, b, t) => 
            Tree(s"${Sequent(gamma, a or b).show} (vR1)", List(t))

        case RightDisjunctionR(gamma, a, b, t) => 
            Tree(s"${Sequent(gamma, a or b).show} (vR2)", List(t))
