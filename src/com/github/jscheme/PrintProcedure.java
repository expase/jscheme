package com.github.jscheme;

import java.io.PrintWriter;

public class PrintProcedure extends Procedure {
    PrintWriter output = new PrintWriter(System.out, true);

    public static void registe(Environment env) {
        env.define("display", new PrintProcedure());
        env.define("println", new PrintProcedure());
    }

    @Override
    Object apply(Interpreter interpreter, Object args) {
        Object x = first(args);
        output.println(x);
        return null;
    }
}
