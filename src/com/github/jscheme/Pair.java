package com.github.jscheme;

public class Pair {
    public Object first;
    public Object rest;

    public Pair(Object first,Object rest) {
        this.first = first;
        this.rest = rest;
    }


    public String toString() {
        StringBuffer buf = new StringBuffer();
        stringifyPair(buf);
        return buf.toString();
    }

    void stringifyPair(StringBuffer buf) {
        buf.append('(');
        stringify(first,  buf);
        Object tail = rest;
        while (tail instanceof Pair) {
            buf.append(' ');
            stringify(((Pair) tail).first,  buf);
            tail = ((Pair) tail).rest;
        }
        if (tail != null) {
            buf.append(" . ");
            stringify(tail,  buf);
        }
        buf.append(')');
    }

    static void stringify(Object x,  StringBuffer buf) {
        if(x == null) {
            buf.append("()");
        } else if (x instanceof Pair) {
            ((Pair)x).stringifyPair(buf);
        } else {
            buf.append(x.toString());
        }
    }

}
