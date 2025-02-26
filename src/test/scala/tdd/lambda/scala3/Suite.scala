package tdd
package lambda
package scala3

import org.scalatest._
import flatspec._
import matchers._
import org.scalactic.Equality

import scala.quoted.* 
import scala.quoted.staging.* 

import calculus.*

class Suite[C[_, _]: Calculus] extends AnyFlatSpec:

    given Compiler = Compiler.make(this.getClass.getClassLoader)

    staging.run: (Q: Quotes) ?=>
        import quotes.reflect.* 
        
        type ScalaTerm = Env[Q.type] => Symbol => Term

        import ScalaForm.given
        import ScalaTerm.given
        
        given lambda.Term[TypeRepr, ScalaTerm] = ScalaTerm.ScalaTermIsTerm

        given Equality[ScalaTerm] with
            def areEqual(a: ScalaTerm, b: Any): Boolean = 
                (b: @unchecked) match 
                    case b: ScalaTerm => 
                        scala.util.Try(
                            a(Map())(Symbol.spliceOwner).asExprOf[Any] `matches`
                            b(Map())(Symbol.spliceOwner).asExprOf[Any]
                        ).getOrElse(false)
    
        given cats.Show[ScalaTerm] with 
            def show(t: ScalaTerm): String = 
                t(Map())(Q.reflect.Symbol.spliceOwner).show
                
        calculus.Suite[TypeRepr, ScalaTerm, C].execute()
        
        '{()}
