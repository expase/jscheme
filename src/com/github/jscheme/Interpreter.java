package com.github.jscheme;




public class Interpreter  extends SchemeUtils {

    public final static Environment globalEnv = new Environment();


    static {
        ComputeProcedure.registe(globalEnv);
        CompareProcedure.registe(globalEnv);
        PrintProcedure.registe(globalEnv);
    }
    public Object eval(Object x) {
        return eval(x, globalEnv);
    }

    public Object eval(Object x,Environment env) {
        while(true) {
            if( x== null) break;
            if(x instanceof String) {
                return env.lookup((String)x);
            } else if(x instanceof Double) {
                return  x;
            } else if (x instanceof StringAtom) {
                return ((StringAtom)x).value;
            }else {
                Object fn = first(x);
                Object args = rest(x);
                if("define".equals(fn)) {
                    if (first(args) instanceof Pair)
                        return env.define((String)first(first(args)),
                                eval(cons("lambda", cons(rest(first(args)), rest(args))), env));
                    else return env.define((String)first(args), eval(second(args), env));

                } else if (fn == "if") {         // IF
                    x = (truth(eval(first(args), env))) ? second(args) : third(args);
                } else if ("lambda".equals(fn)) {
                    return new Closure(first(args), rest(args), env);
                } else if ("call/cc".equals(fn)) {
                    RuntimeException cc = new RuntimeException();
                    Continuation proc = new Continuation(cc);
                    try {
                        return ((Procedure)first(args)).apply(this, cons(proc, null));
                    } catch (RuntimeException e) {
                        if (e == cc) return proc.value;
                        else throw e;
                    }

                }else {
                    fn = eval(fn, env);
                    if(fn instanceof Closure) { // (CLOSURE CALL)
                        Closure f = (Closure) fn;
                        x = f.body;
                        env = new Environment(f.parms, evalList(args, env), f.env);
                    } else if (fn instanceof Procedure) {                            // (OTHER PROCEDURE CALL)
                        return ((Procedure) fn).apply(this, evalList(args, env));
                    }
                }
            }
        }
        return null;

    }

    Pair evalList(Object x, Environment env) {
        if(x != null && !(x instanceof Pair)) error("Illegal arg list " + x);
        return x == null ? null : cons(eval(first(x), env), evalList(rest(x), env));

    }

    public static Object third(Object x) {
        return first(rest(rest(x)));
    }

    public static boolean truth(Object x) {
        return x != Boolean.FALSE;
    }


    public static void main(String[] args) {
        Parser parser = new Parser("(define (inc x) (+ x 1)) (display (inc 2))");
        Interpreter interpreter = new Interpreter();
        while(true) {
            Object expr = parser.read();
            if(expr == null) break;
            interpreter.eval(expr);

        }

    }


}
