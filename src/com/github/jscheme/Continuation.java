package com.github.jscheme;

public class Continuation extends Procedure {
    RuntimeException cc = null;
    public Object value = null;

    public Continuation(RuntimeException cc) {
        this.cc = cc;
    }

    public Object apply(Interpreter interpreter, Object args) {
        value = first(args);
        throw cc;
    }
}
