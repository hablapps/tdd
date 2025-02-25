
package tdd
package calculus 

import cats.data.*
import tdd.util.*
import cats.Show, cats.syntax.show.*
import lambda.*

case class Sequent[F: Form](val ant: List[Sequent.Prem[F]], val con: F): 

    def nextVar: Int = 
        if ant.isEmpty then 0 else ant.map(_._1).max + 1

    def rotations: List[Sequent[F]] = 
        (1 to ant.length-1)
            .foldLeft(ant :: Nil):
                case (acc, _) => (acc.head.tail :+ acc.head.head) :: acc
            .map(Sequent(_, con))

object Sequent: 

    type Prem[F] = (Int, F)

    def initial[F: Form](prop: F): Sequent[F] = 
        Sequent(Nil, prop)

    given [F: Form: Show]: Show[Sequent[F]] with 
        def show(seq: Sequent[F]): String = 
            seq.ant.map{ case (v, f) => s"$v: ${f.show}"}.mkString(", ") + 
            (if seq.ant.isEmpty then "" else " ") + 
            "⊢ " + 
            seq.con.show
