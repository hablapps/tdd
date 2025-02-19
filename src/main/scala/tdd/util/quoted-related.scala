package tdd.util

import cats.* 

import scala.quoted.* 

given TypeRepr_Show(using Q: Quotes): Show[Q.reflect.TypeRepr] = 
    import quotes.reflect.* 

    new Show[TypeRepr]:
        def show(t: TypeRepr): String = 
            t.asType match 
                case '[a => b] => "(" + show(TypeRepr.of[a]) + " => " + show(TypeRepr.of[b]) + ")"
                case _ => t.show
