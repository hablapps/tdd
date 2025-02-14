package tdd
package api
package lj

import tdd.util.*
import cats.Show, cats.syntax.show.*
import cats.data.*

enum Sequent[F: Form](val ant: List[(Int, F)], val con: F): 

    case GenSeq[F: Form](seq: api.Sequent[F])
        extends Sequent[F](seq.ant, seq.con)
    
    case AxiomSeq[F: Form](override val ant: List[(Int, F)], override val con: F, val idx: (Int, F)) 
        extends Sequent[F](ant, con)
    
    case LeftFalseSeq[F: Form](override val ant: List[(Int, F)], override val con: F, val idx: Int) 
        extends Sequent[F](ant, con)
    
    case RightImplSeq[F: Form](override val ant: List[(Int, F)], impl: (F, F)) 
        extends Sequent(ant, impl._1 implies impl._2)

    case LeftImplSeq[F: Form](ant1: List[(Int, F)], impl: (Int, (F, F)), ant2: List[(Int, F)], override val con: F) 
        extends Sequent(ant1 ++ ((impl._1, impl._2._1 implies impl._2._1) :: ant2), con)

    case RightAndSeq[F: Form](override val ant: List[(Int, F)], impl: (F, F)) 
        extends Sequent(ant, impl._1 and impl._2)

    case LeftAndSeq[F: Form](ant1: List[(Int, F)], impl: (Int, (F, F)), ant2: List[(Int, F)], override val con: F) 
        extends Sequent(ant1 ++ ((impl._1, impl._2._1 and impl._2._1) :: ant2), con)

    case RightDisjunctionSeqL[F: Form](override val ant: List[(Int, F)], impl: (F, F)) 
        extends Sequent(ant, impl._1 or impl._2)

    case RightDisjunctionSeqR[F: Form](override val ant: List[(Int, F)], impl: (F, F)) 
        extends Sequent(ant, impl._1 or impl._2)

    case LeftDisjunctionSeq[F: Form](ant1: List[(Int, F)], or: (Int, (F, F)), ant2: List[(Int, F)], override val con: F) 
        extends Sequent(ant1 ++ ((or._1, or._2._1 or or._2._1) :: ant2), con)
    
    def nextVar: Int = 
        if ant.isEmpty then 0 else ant.map(_._1).max + 1

    def lastVar: Option[(Int, F)] = 
        ant.lastOption

object Sequent: 

    def toSequent[F: Form](from: Sequent[F]): api.Sequent[F] = 
        api.Sequent(from.ant, from.con)

    def fromSequent[F: Form](gs: api.Sequent[F]): List[Sequent[F]] = 
        Sequent.IsAxiom.unapply(gs).toList ++ 
        Sequent.IsLeftFalse.unapply(gs).toList ++ 
        Sequent.IsRightImpl.unapply(gs).toList ++ 
        Sequent.IsLeftImpl.unapply(gs).toList.flatMap(_.toList) ++
        Sequent.IsLeftAnd.unapply(gs).toList.flatMap(_.toList) ++
        Sequent.IsRightAnd.unapply(gs).toList ++
        Sequent.IsLeftOr.unapply(gs).toList.flatMap(_.toList) ++
        Sequent.IsRightOr.unapply(gs).toList.flatMap(_.toList)

    object IsAxiom: 
        def unapply[F: Form](seq: api.Sequent[F]): Option[AxiomSeq[F]] = 
            val idx = seq.ant.map(_._2).indexOf(seq.con)
            if idx >= 0 then Some(AxiomSeq(seq.ant, seq.con, seq.ant(idx)))
            else None

    object IsLeftFalse: 
        def unapply[F: Form](seq: api.Sequent[F]): Option[LeftFalseSeq[F]] =
            val idxFalse = seq.ant.map(_._2).indexOf(Form[F].False)
            if idxFalse >= 0 then Some(LeftFalseSeq(seq.ant, seq.con, idxFalse))
            else None

    object IsRightImpl: 
        def unapply[F: Form](seq: api.Sequent[F]): Option[RightImplSeq[F]] = 
            seq match 
                case api.Sequent(ant, Form.Implies(a, b)) => Some(RightImplSeq(ant, (a, b)))
                case _ => None

    object IsLeftImpl: 
        def unapply[F: Form](seq: api.Sequent[F]): Option[NonEmptyList[LeftImplSeq[F]]] = 
            seq.ant.zipWithIndex
                .collect:
                    case ((v, f@Form.Implies(a, b)), i) =>
                        (seq.ant.splitAt(i): @unchecked) match 
                            case (l1, _ :: l2) => LeftImplSeq(l1, (v, (a, b)), l2, seq.con)
                .toNEL

    object IsRightAnd: 
        def unapply[F: Form](seq: api.Sequent[F]): Option[RightAndSeq[F]] = 
            seq match 
                case api.Sequent(ant, Form.And(a, b)) => Some(RightAndSeq(ant, (a, b)))
                case _ => None

    object IsLeftAnd: 
        def unapply[F: Form](seq: api.Sequent[F]): Option[NonEmptyList[LeftAndSeq[F]]] = 
            seq.ant.zipWithIndex
                .collect:
                    case ((v, f@Form.And(a, b)), i) =>
                        (seq.ant.splitAt(i): @unchecked) match 
                            case (l1, _ :: l2) => LeftAndSeq(l1, (v, (a, b)), l2, seq.con)
                .toNEL

    object IsRightOr: 
        def unapply[F: Form](seq: api.Sequent[F]): Option[NonEmptyList[Sequent[F]]] = 
            seq match 
                case api.Sequent(ant, Form.Or(a, b)) => 
                    Some(NonEmptyList.of(RightDisjunctionSeqL(ant, (a, b)), RightDisjunctionSeqR(ant, (a, b))))
                case _ => None

    object IsLeftOr: 
        def unapply[F: Form](seq: api.Sequent[F]): Option[NonEmptyList[LeftDisjunctionSeq[F]]] = 
            seq.ant.zipWithIndex
                .collect:
                    case ((v, f@Form.Or(a, b)), i) =>
                        (seq.ant.splitAt(i): @unchecked) match 
                            case (l1, _ :: l2) => LeftDisjunctionSeq(l1, (v, (a, b)), l2, seq.con)
                .toNEL

    
    given [F: Form: Show]: Show[Sequent[F]] with 
        def show(seq: Sequent[F]): String = 
            val seqType = seq match
                case _ : GenSeq[F] => "Gen"
                case _ : AxiomSeq[F] => "Ax"   
                case _ : LeftFalseSeq[F] => "F L"   
                case _ : RightImplSeq[F] => "-> R"
                case _ : LeftImplSeq[F] => "-> L"
                case _ : RightAndSeq[F] => "& R"
                case _ : LeftAndSeq[F] => "& L" 
                case _ : RightDisjunctionSeqL[F] => "v RL"
                case _ : RightDisjunctionSeqR[F] => "v RR"
                case _ : LeftDisjunctionSeq[F] => "v L"
                
            
            seqType + ": " + 
            seq.ant.map{ case (v, f) => s"v$v: ${f.show}"}.mkString(", ") + 
            (if seq.ant.isEmpty then "" else " ") + 
            "⊢ " + 
            seq.con.show
