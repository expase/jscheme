package com.github.jscheme;

public abstract  class Procedure extends SchemeUtils {
    abstract Object apply(Interpreter interpreter, Object args);
}
