package tdd
package scala3

import scala.quoted.* 
import util.given
import cats.Show

object ScalaTerm: 

    import ScalaForm.SF
    
    given ScalaTermIsTerm(using Q: Quotes): api.Term[Env[Q.type] => Q.reflect.Symbol => Q.reflect.Term, Q.reflect.TypeRepr] = 
        
        import Q.reflect.*

        type ScalaTerm = Env[Q.type] => Symbol => Term
        
        new api.Term[ScalaTerm, TypeRepr]:

            extension (v: Int)
                def `var`: ScalaTerm = 
                    env => owner => 
                        val t = Ref(env(v))
                        //logger.info(s"In var: ${t.show}: ${t.tpe.widen.show}")
                        t

            extension (v: (Int, TypeRepr))
                def lam(body: ScalaTerm): ScalaTerm = 
                    env => owner => 
                        val newV: Symbol = Env.newVar(v)
                                                    
                        val bodyEval: Term = body(env + (v._1 -> newV))(Symbol.noSymbol)
                        
                        val lamMethodType: MethodType = MethodType(List(newV.name))(
                            _ => List(v._2), 
                            _ => bodyEval.tpe.widen)
                            
                        val t = Lambda(
                            owner, 
                            lamMethodType,
                            (meth, params) => 
                                Env.substRefs(newV, params(0).asInstanceOf[Term])(bodyEval.changeOwner(meth)))

                        //logger.info(s"In lam: ${t.show}: ${t.tpe.show}")
                        t

            extension (t: ScalaTerm)

                def inl(tpe: TypeRepr): ScalaTerm = 
                    env => owner => 
                        val t1t: Term = t(env)(owner)
                        (t1t.tpe.widen.asType, tpe.widen.asType) match 
                            case ('[a], '[b]) => 
                                val t = '{Left[a, b](${t1t.asExprOf[a]}): Either[a, b]}.asTerm
                                //logger.info(s"In or: ${t.show}: ${t.tpe.show}")
                                t
                def inr(tpe: TypeRepr): ScalaTerm = 
                    env => owner => 
                        val t1t: Term = t(env)(owner)
                        (tpe.widen.asType, t1t.tpe.widen.asType) match 
                            case ('[a], '[b]) => 
                                val t = '{Right[a, b](${t1t.asExprOf[b]}): Either[a, b]}.asTerm
                                //logger.info(s"In or: ${t.show}: ${t.tpe.show}")
                                t

                def `match`(inl: ScalaTerm, inr: ScalaTerm): ScalaTerm = 
                    env => owner => 
                        val t1t: Term = t(env)(owner)
                        val inlf: Term = inl(env)(owner)
                        val inrf: Term = inr(env)(owner)
                        
                        val r = t1t.tpe.widen.asType match 
                            case '[Either[a, b]] => 
                                inlf.tpe.widen.asType match
                                    case '[`a` => c] =>
                                        inrf.tpe.widen.asType match 
                                            case '[`b` => `c`] => 
                                                '{${t1t.asExprOf[Either[a, b]]}
                                                    .fold(${inlf.asExprOf[a => c]}, ${inrf.asExprOf[b => c]})
                                                }.asTerm
                        //logger.info(s"In match: ${r.show}: ${r.tpe.show}")
                        r  

                infix def and(t2: ScalaTerm): ScalaTerm = 
                    env => owner => 
                        val t1t: Term = t(env)(owner)
                        val t2t: Term = t2(env)(owner)
                        (t1t.tpe.widen.asType, t2t.tpe.widen.asType) match 
                            case ('[tpea], '[tpeb]) => 
                                val t = '{(${t1t.asExprOf[tpea]}, ${t2t.asExprOf[tpeb]})}.asTerm
                                //logger.info(s"In and: ${t.show}: ${t.tpe.show}")
                                t

                def _1: ScalaTerm = 
                    env => owner => 
                        val t1t: Term = t(env)(owner)
                        t1t.tpe.asType match 
                            case '[(tpea, tpeb)] =>
                                val t = '{${t1t.asExprOf[(tpea, tpeb)]}._1: tpea}.asTerm
                                //logger.info(s"In _1: ${t.show}: ${t.tpe.show}")
                                t
                
                def _2: ScalaTerm = 
                    env => owner => 
                        val t1t: Term = t(env)(owner)
                        t1t.tpe.asType match 
                            case '[(tpea, tpeb)] => 
                                val t = '{${t1t.asExprOf[(tpea, tpeb)]}._2: tpeb}.asTerm
                                //logger.info(s"In _2: ${t.show}: ${t.tpe.show}")
                                t

                infix def apply(t2: ScalaTerm): ScalaTerm = 
                    env => owner => 
                        val tf: Term = t(env)(owner)
                        val ta: Term = t2(env)(owner)
                        ta.tpe.asType match 
                            case '[tpea] => 
                                tf.tpe.asType match 
                                    case '[`tpea` => b] => 
                                        val t = '{${tf.asExprOf[tpea => b]}(${ta.asExprOf[tpea]})}.asTerm
                                        //logger.info(s"In app: ${t.show}: ${t.tpe.show}")
                                        t

                def subst(v: Int, by: ScalaTerm): ScalaTerm = 
                    env => owner => 
                        val termBy = by(env)(owner)
                        val newS = Env.newVar((v, termBy.tpe))
                        val term = t(env  + (v -> newS))(owner)
                        val st = Env.substRefs(newS, termBy)(term)
                        //logger.info(s"In ${term.show} subst $v by ${termBy.show}: ${st.show}: ${st.tpe.show}")
                        st    

