package tdd.util

import cats.* 

import scala.quoted.* 

extension [T: Type](using Quotes)(o: Option[Expr[T]])
    def sequence: Expr[Option[T]] = 
        o match 
            case None => '{None}
            case Some(a) => '{Some($a)}

given TypeRepr_Show(using Q: Quotes): Show[Q.reflect.TypeRepr] = 
    import quotes.reflect.* 

    new Show[TypeRepr]:
        def show(t: TypeRepr): String = 
            t.asType match 
                case '[a => b] => "(" + show(TypeRepr.of[a]) + " => " + show(TypeRepr.of[b]) + ")"
                case _ => t.show
