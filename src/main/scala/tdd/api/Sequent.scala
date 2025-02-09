
package tdd.api 

import cats.data.*
import tdd.util.*
import cats.Show, cats.syntax.show.*

case class Sequent[F: Form](val ant: List[(Int, F)], val con: F)

object Sequent: 

    def proof[F: Form](prop: F): Sequent[F] = 
        Sequent(Nil, prop)

    given [F: Form: Show]: Show[Sequent[F]] with 
        def show(seq: Sequent[F]): String = 
            seq.ant.map{ case (v, f) => s"v$v: ${f.show}"}.mkString(", ") + 
            (if seq.ant.isEmpty then "" else " ") + 
            "⊢ " + 
            seq.con.show
