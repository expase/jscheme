package com.github.jscheme;



import java.util.HashMap;
import java.util.Map;

public class Environment extends SchemeUtils {

    private Environment outer;
    private Map<String, Object> env = new HashMap<>();

    public Environment() {

    }

    public Environment(Object params,Object args, Environment outer) {
        if(params != null) {
            while(params instanceof Pair) {
                String name = (String)first(params);
                Object val = first(args);
                env.put(name, val);
                params = rest(params);
                args = rest(args);

            }
        }
        this.outer = outer;
    }

    public Object lookup(String symbol) {
        if(env.containsKey(symbol)) {
            return env.get(symbol);
        }
        if(outer != null) return outer.lookup(symbol);
        else return error(String.format("Unbound variable: %s", symbol));
    }

    public static Object error(String message) {
        System.err.println("**** ERROR: " + message);
        throw new RuntimeException(message);
    }

    public Object define(String symbol,Object x) {
        env.put(symbol, x);
        return x;

    }

}
