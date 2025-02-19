package tdd
package scala3

import scala.quoted.* 
import cats.Show

object ScalaForm: 

    given Show_TypeRepr(using Q: Quotes): Show[Q.reflect.TypeRepr] = 
        import quotes.reflect.*

        new Show[TypeRepr]: 
            def show(t: TypeRepr): String =
                t.show
        
    given SF(using Q: Quotes): api.Form[Q.reflect.TypeRepr] = 
        import quotes.reflect.* 

        new api.Form[TypeRepr]: 

            val False: TypeRepr = TypeRepr.of[Nothing]

            var atoms: Map[String, TypeRepr] = Map() 

            extension (p: String)
                def atom: TypeRepr = 
                    if atoms.contains(p) then atoms(p)
                    else 
                        val clsSymbol = Symbol.newClass(Symbol.spliceOwner, p, List(TypeRepr.of[Object]), _ => List(), None)
                        val classDef: ClassDef = ClassDef(clsSymbol, List(TypeTree.of[Object]), List())
                        val tpe = TypeTree.ref(clsSymbol).tpe
                        atoms += (p -> tpe)
                        tpe

            extension (a: TypeRepr)
                infix def implies(b: TypeRepr): TypeRepr = 
                    (a.asType, b.asType) match 
                        case ('[t1], '[t2]) => 
                            TypeRepr.of[t1 => t2]

                infix def and(b: TypeRepr): TypeRepr = 
                    (a.asType, b.asType) match 
                        case ('[t1], '[t2]) => 
                            TypeRepr.of[Tuple2[t1, t2]]

                infix def or(b: TypeRepr): TypeRepr = 
                    (a.asType, b.asType) match 
                        case ('[t1], '[t2]) => 
                            TypeRepr.of[Either[t1, t2]]

                def fold[W](
                        `false`: W, 
                        atom: String => W, 
                        implies: (TypeRepr, TypeRepr) => W,
                        and: (TypeRepr, TypeRepr) => W,
                        or: (TypeRepr, TypeRepr) => W): W = 
                    a.asType match 
                        case '[Nothing] => `false`
                        case '[(a1 *: a2 *: EmptyTuple) => b] => implies(TypeRepr.of[(a1, a2)], TypeRepr.of[b])
                        case '[a => b] => implies(TypeRepr.of[a], TypeRepr.of[b])
                        case '[(a, b)] => and(TypeRepr.of[a], TypeRepr.of[b])
                        case '[Either[a, b]] => or(TypeRepr.of[a], TypeRepr.of[b])
                        case '[a] => a match 
                            case TypeRef(tpe, name) => atom(name)
                            case _ => report.errorAndAbort(s"Type ${a.show} not supported")