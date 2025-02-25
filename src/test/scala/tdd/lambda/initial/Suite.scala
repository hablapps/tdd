package tdd
package lambda.initial

import org.scalatest._
import flatspec._
import matchers._

import calculus.*

class Suite[C[_, _]: Calculus] extends calculus.Suite[Form, Term, C]