package tdd
package calculus
package ljt

import util.*
import cats.*, cats.syntax.all.*
import LJT.*
import lj.LJ
import Calculus.given
import lambda.*

given Calculus_LJT: Calculus[LJT] with 

    given IsTraverse[F: Form]: Traverse[Aux[F]] with 
        def traverse[G[_]: Applicative, A, B](fa: LJT[F, A])(f: A => G[B]): G[LJT[F, B]] = 
            fa match
                case Axiom(s) => lj.traverse(s)(f).map(Axiom.apply)
                case LeftFalse(s) => lj.traverse(s)(f).map(LeftFalse.apply)
                case RightImplication(s) => lj.traverse(s)(f).map(RightImplication.apply)
                case RightConjunction(s) => lj.traverse(s)(f).map(RightConjunction.apply)
                case LeftConjunction(s) => lj.traverse(s)(f).map(LeftConjunction.apply)
                case RightDisjunctionL(s) => lj.traverse(s)(f).map(RightDisjunctionL.apply)
                case RightDisjunctionR(s) => lj.traverse(s)(f).map(RightDisjunctionR.apply)
                case LeftDisjunction(s) => lj.traverse(s)(f).map(LeftDisjunction.apply)
                case LeftImplication1(p, gamma, g, t1, x, t2) => (f(t1), f(t2)).mapN(LeftImplication1(p, gamma, g, _, x, _))
                case LeftImplication2(p, gamma, g, x, t) => f(t).map(LeftImplication2(p, gamma, g, x, _))
                case LeftImplication3(p, gamma, g, x, y, t) => f(t).map(LeftImplication3(p, gamma, g, x, y, _))
                case LeftImplication4(p, gamma, g, z, t1, x, t2) => (f(t1), f(t2)).mapN(LeftImplication4(p, gamma, g, z, _, x, _))

        def foldLeft[A, B](fa: LJT[F, A], b: B)(f: (B, A) => B): B = ??? 

        def foldRight[A, B](fa: LJT[F, A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = ??? 

    def coalg[F: Form]: Coalgebra[Sequent[F], SearchSpace.SearchF[F, LJT]] = seq =>
        import calculus.SearchSpace.apply                
        val rotations: LazyList[Sequent[F]] = LazyList.from(seq.rotations)

        Rule[LJT.Axiom].coalg(rotations) ++ 
        Rule[LJT.LeftFalse].coalg(rotations) ++
        Rule[LJT.RightImplication].coalg(LazyList(seq)) ++ 
        Rule[LJT.RightDisjunctionL].coalg(LazyList(seq)) ++ 
        Rule[LJT.RightDisjunctionR].coalg(LazyList(seq)) ++ 
        Rule[LJT.RightConjunction].coalg(LazyList(seq)) ++ 
        Rule[LJT.LeftImplication1].coalg(rotations) ++ 
        Rule[LJT.LeftImplication2].coalg(rotations) ++ 
        Rule[LJT.LeftImplication3].coalg(rotations) ++ 
        Rule[LJT.LeftImplication4].coalg(rotations) ++ 
        Rule[LJT.LeftConjunction].coalg(rotations) ++ 
        Rule[LJT.LeftDisjunction].coalg(rotations)

    def alg[F: Form, T](using Term[F, T]): Algebra[LJT.Aux[F], T] = 
        case f: Axiom[F, T] => Rule[Axiom].alg(f)
        case f: LeftFalse[F, T] => Rule[LeftFalse].alg(f)
        case f: RightImplication[F, T] => Rule[RightImplication].alg(f)
        case f: LeftImplication1[F, T] => Rule[LeftImplication1].alg(f)
        case f: LeftImplication2[F, T] => Rule[LeftImplication2].alg(f)
        case f: LeftImplication3[F, T] => Rule[LeftImplication3].alg(f)
        case f: LeftImplication4[F, T] => Rule[LeftImplication4].alg(f)
        case f: RightConjunction[F, T] => Rule[RightConjunction].alg(f)
        case f: LeftConjunction[F, T] => Rule[LeftConjunction].alg(f)
        case f: RightDisjunctionL[F, T] => Rule[RightDisjunctionL].alg(f)
        case f: RightDisjunctionR[F, T] => Rule[RightDisjunctionR].alg(f)
        case f: LeftDisjunction[F, T] => Rule[LeftDisjunction].alg(f)
        
    given toTree[F: Form: Show]: Algebra[LJT.Aux[F], Tree[Any]] =
        
        case Axiom(s) => 
            Algebra[LJ.Aux[F], Tree[Any]](s)

        case LeftFalse(s) => 
            Algebra[LJ.Aux[F], Tree[Any]](s)

        case RightImplication(s) => 
            Algebra[LJ.Aux[F], Tree[Any]](s)

        case RightConjunction(s) => 
            Algebra[LJ.Aux[F], Tree[Any]](s)

        case LeftConjunction(s) => 
            Algebra[LJ.Aux[F], Tree[Any]](s)

        case RightDisjunctionL(s) => 
            Algebra[LJ.Aux[F], Tree[Any]](s)

        case RightDisjunctionR(s) => 
            Algebra[LJ.Aux[F], Tree[Any]](s)

        case LeftDisjunction(s) => 
            Algebra[LJ.Aux[F], Tree[Any]](s)

        case LeftImplication1(p, gamma, g, t1, x, t2) => 
            Tree(s"${Sequent(p :: gamma, g).show} (->L1)", List(t1, t2))

        case LeftImplication2(p, gamma, g, x, t) => 
            Tree(s"${Sequent(p :: gamma, g).show} (->L2)", List(t))

        case LeftImplication3(
                p, gamma, g, 
                x, y, t) => 
            Tree(s"${Sequent(p :: gamma, g).show} (->L3)", List(t))

        case LeftImplication4(
                p, gamma, g, 
                (v1, tpe1), t1, 
                (v2, tpe2), t2) => 
            Tree(s"${Sequent(p :: gamma, g).show} (->L4)", List(t1, t2))

        
