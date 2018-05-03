package com.github.jscheme;

/**
 * + - * / operator function support
 */
public class ComputeProcedure extends Procedure {
    private char op;
    public ComputeProcedure(char op) {
        this.op = op;
    }
    @Override
    Double apply(Interpreter interpreter, Object args) {
        Object x = first(args);

        switch(op) {
            case '+':
                return numCompute(args, '+', 0.0);
            case '-':
                return numCompute(rest(args), '-', (double)x);
            case '*':
                return numCompute(args, '*', 1.0);
            case '/':
                return numCompute(rest(args), '/', (double)x);
            default:
                return error("internal error: unrecognized op: " + op);
        }

    }

    public static void registe(Environment env) {
        env.define("+", new ComputeProcedure('+'));
        env.define("-", new ComputeProcedure('-'));
        env.define("*", new ComputeProcedure('*'));
        env.define("/", new ComputeProcedure('/'));
    }

    public static Double numCompute(Object args, char op, double result) {
        if (args == null) {
            switch (op) {
                case '-':
                    return 0 - result;
                case '/':
                    return 1 / result;
                default:
                    return result;
            }
        } else {
            while (args instanceof Pair) {
                double x = (double)first(args);
                args = rest(args);
                switch (op) {
                    case '+':
                        result += x;
                        break;
                    case '-':
                        result -= x;
                        break;
                    case '*':
                        result *= x;
                        break;
                    case '/':
                        result /= x;
                        break;
                    default:
                        error("internal error: unrecognized op: " + op);
                        break;
                }
            }
            return result;
        }
    }

}
