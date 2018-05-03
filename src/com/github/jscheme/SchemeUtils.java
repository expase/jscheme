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

    static void stringify(Object x, StringBuffer buf) {
        if (x == null)
            buf.append("()");
        else if (x instanceof Double) {
            double d = ((Double) x).doubleValue();
            if (Math.round(d) == d) buf.append((long) d);
            else buf.append(d);
        } else if (x instanceof Character) {
            buf.append(x);
        } else if (x instanceof Pair) {
            ((Pair) x).stringifyPair(buf);
        } else if (x instanceof char[]) {
            char[] chars = (char[]) x;
            for (int i = 0; i < chars.length; i++) {
                buf.append(chars[i]);
            }
        } else if (x instanceof Object[]) {
            Object[] v = (Object[]) x;
            buf.append("#(");
            for (int i = 0; i < v.length; i++) {
                stringify(v[i],  buf);
                if (i != v.length - 1) buf.append(' ');
            }
            buf.append(')');
        } else if (x == TRUE) {
            buf.append("#t");
        } else if (x == FALSE) {
            buf.append("#f");
        } else {
            buf.append(x);
        }
    }

    static String stringify(Object x) {
        StringBuffer buf = new StringBuffer();
        stringify(x, buf);
        return buf.toString();
    }

}
