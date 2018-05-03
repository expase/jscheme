package com.github.jscheme;




public class SchemeUtils {
    /**
     * Same as Boolean.TRUE. *
     */
    public static final Boolean TRUE = Boolean.TRUE;
    /**
     * Same as Boolean.FALSE. *
     */
    public static final Boolean FALSE = Boolean.FALSE;

    public  static <T>  T error(String message) {
        System.err.println(message);
        throw new RuntimeException(message);
    }

    public static Object first(Object x) {
        return x instanceof Pair ? ((Pair)x).first : null;
    }

    public static Object rest(Object x) {
        return x instanceof Pair ? ((Pair)x).rest : null;
    }

    public static Pair cons(Object first,Object rest) {
        return new Pair(first, rest);
    }

    public static Object second(Object x) {
        return first(rest(x));
    }




}
