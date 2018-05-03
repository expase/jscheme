package com.github.jscheme;

/**
 * < > <= >= = operator support
 */
public class CompareProcedure extends Procedure {

    private char op;
    public CompareProcedure(char op) {
        this.op = op;
    }

    public static void registe(Environment env) {
        env.define("<", new CompareProcedure('<'));
        env.define(">", new CompareProcedure('>'));
        env.define("=", new CompareProcedure('='));
        env.define("<=", new CompareProcedure('L'));
        env.define(">=", new ComputeProcedure('G'));
    }
    @Override
    Object apply(Interpreter interpreter, Object args) {
        while (rest(args) instanceof Pair) {
            double x = num(first(args));
            args = rest(args);
            double y = num(first(args));
            switch (op) {
                case '>':
                    if (!(x > y)) return FALSE;
                    break;
                case '<':
                    if (!(x < y)) return FALSE;
                    break;
                case '=':
                    if (!(x == y)) return FALSE;
                    break;
                case 'L':
                    if (!(x <= y)) return FALSE;
                    break;
                case 'G':
                    if (!(x >= y)) return FALSE;
                    break;
                default:
                    error("internal error: unrecognized op: " + op);
                    break;
            }
        }
        return TRUE;
    }

    public static double num(Object x) {
        if (x instanceof Number) return ((Number) x).doubleValue();
        else return num(error("expected a number, got: " + x));
    }

}
