package tdd
package api

import org.scalatest._
import funspec._
import matchers._
import org.scalactic.Equality
import cats.Show
import cats.syntax.show.*
import tdd.util.*

class ExamplesConjunction[F: Form: Show, T: Term.Aux[F]: Equality: Show] extends AnyFunSpec with should.Matchers: 

    describe("Conjunctions"): 
        
        it("A & B -> B & A"): 

            ("A".atom and "B".atom implies ("B".atom and "A".atom)) should BeProvenBy(
                (0, "A".atom and "B".atom).lam(
                    0.`var`._2 `and` 0.`var`._1
                )
            )
            
        it("A & B -> A"): 
            
            (("A".atom and "B".atom) implies "A".atom) should BeProvenBy(
                (0, "A".atom and "B".atom).lam(
                    0.`var`._1
                )
            )
