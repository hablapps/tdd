package tdd
package initial

enum Form: 
    case False
    case Atom(p: String) extends Form
    case Implies(a: Form, c: Form) extends Form
    case And(a: Form, c: Form) extends Form
    case Or(a: Form, c: Form) extends Form

object Form: 

    given api.Form[Form] with 

        val False: Form = Form.False

        extension (s: String)
            def atom = Atom(s)

        extension (f: Form)
            infix def implies(b: Form) = Form.Implies(f, b)

            infix def and(b: Form) = Form.And(f, b)

            infix def or(b: Form) = Form.Or(f, b)

            def fold[W](
                    `false`: W, 
                    atom: String => W, 
                    implies: (Form, Form) => W,
                    and: (Form, Form) => W,
                    or: (Form, Form) => W): W = 
                f match 
                    case Form.False => `false`
                    case Atom(n) => atom(n)
                    case Form.Implies(a, b) => implies(a, b)
                    case And(a, b) => and(a, b)
                    case Or(a, b) => or(a, b)

    given cats.Show[Form] with 
        def show(f: Form): String = 
            f.toString(false)
            
        extension (f: Form)
            def toString(paren: Boolean): String = 
                f match 
                    case False => "F"
                    case Atom(p) => p
                    case Implies(a, b) => 
                        val str = s"${a.toString(true)} -> ${b.toString(false)}"
                        if paren then s"($str)" else str
                    case And(a, b) => 
                        s"${a.toString(true)} & ${b.toString(true)}"
                    case Or(a, b) => 
                        val str = s"${a.toString(paren)} v ${b.toString(paren)}"
                        if paren then s"($str)" else str
