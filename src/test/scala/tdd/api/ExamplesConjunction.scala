package tdd
package api

import org.scalatest._
import funspec._
import matchers._
import org.scalactic.Equality
import cats.Show
import cats.syntax.show.*
import tdd.util.*
import Form.Syntax.{given, *}
import Term.Syntax.{given, *}

class ExamplesConjunction[F: Form: Show, T: Term.Aux[F]: Equality: Show] extends AnyFunSpec with should.Matchers: 

    describe("Conjunctions"): 
        
        it("A & B -> B & A"): 

            (("A" ^ "B") -> ("B" ^ "A")) should BeProvenBy(
                (0, "A" ^ "B").lam(
                    (0._2, 0._1)
                ),
                through = lj.Proof.DepthFirst
            )
            
        it("A & B -> A"): 
            
            (("A" ^ "B") -> "A") should BeProvenBy(
                (0, "A" ^ "B").lam(0._1),
                through = lj.Proof.DepthFirst
            )
