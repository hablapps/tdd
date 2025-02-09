package tdd
package scala3

import scala.quoted.* 
import api.lj.SearchSpace
import api.lj.Proof
import tdd.util.{given, *}
import cats.syntax.show.*

inline def derivation[A]: A = 
    ${derivationImpl[A]}

def derivationImpl[A: Type](using Q: Quotes): Expr[A] = 
    import quotes.reflect.* 
    
    import ScalaForm.given
    given api.Term[Env[Q.type] => Q.reflect.Symbol => Q.reflect.Term, Q.reflect.TypeRepr] = 
        ScalaTerm.ScalaTermIsTerm
            
    val tpeA: TypeRepr = TypeRepr.of[A]

    val termA1: Option[Term] = 
        Proof.proof(tpeA)
            .map(_(Map())(Symbol.spliceOwner))
            .map(Inlined(None, Nil, _))

    termA1 match 
        case None =>  
            report.errorAndAbort(s"Type ${tpeA.show} is not inhabited")
        case Some(termA) => 
            logger.info(termA.show)
            termA.asExprOf[A]

