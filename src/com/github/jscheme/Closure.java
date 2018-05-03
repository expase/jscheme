package com.github.jscheme;

/**
 * a lambda function call
 */
public class Closure extends Procedure {
    public Object parms;
    public Object body;
    public Environment env;

    public Closure(Object parms,Object body, Environment env) {
        this.parms = parms;
        this.body = first(body);
        this.env = env;
    }

    @Override
    Object apply(Interpreter interpreter, Object args) {
        return interpreter.eval(body, new Environment(parms, args, env));
    }

    public String toString() {
        return "Closure(" + body + ")";
    }
}
