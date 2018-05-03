package com.github.jscheme;

import java.util.ArrayList;
import java.util.List;

import static com.github.jscheme.SchemeUtils.error;

import static com.github.jscheme.TokenType.*;

public class Parser {

    private Scanner scanner;
    private List<Token> lookahead = new ArrayList<>();
    private int p = 0;

    public Parser(String source) {
        this(new Scanner(source));
    }

    public Parser(Scanner scanner) {
        this.scanner = scanner;
        sync(1);
    }
    public Object read() {
        Token token = peek();

        if(match(LEFT_PAREN)) {
            return readTail();
        } else if (match(TRUE)) {
            return Boolean.TRUE;
        } else if (match( FALSE)) {
            return Boolean.FALSE;
        }else if (match(NUMBER)) {
            return token.literal;
        } else if (match( STRING)) {
            return new StringAtom(token.lexeme);
        } else if (match( SYMBOL)) {
            return token.lexeme;
        } else if (token.tokenType == EOF) {
            return null;
        }
        throw new RuntimeException("unknown token " + token);
    }

    public Object readTail() {

        if(match( RIGHT_PAREN)) {
            return null;
        } else if (match(EOF)) {
            return error("EOF during read.");
        }else {
            return cons(read(), readTail());
        }
    }


    public Token peek() {
        return LT(1);
    }

    private Token LT(int i) {
        sync(i);
        return lookahead.get(p + i - 1);
    }

    private TokenType LA(int i) {
        return LT(i).tokenType;
    }

    private void sync(int i) {
        if ((p + i - 1) > (lookahead.size() - 1)) {
            int n = (p + i - 1) - (lookahead.size() - 1);
            fill(n);
        }
    }

    private void fill(int n) {
        for(int i = 0; i < n; i++){
            lookahead.add(scanner.nextToken());
        }
    }

    private boolean match(TokenType tokenType) {
        if (LA(1) == tokenType) {
            consume();
            return true;
        }
        return false;
    }


    private void consume(){
        p++;
        if ( p == lookahead.size()) {
            p = 0;
            lookahead.clear();
        }
        sync(1);

    }

    public Token peekNext() {
        return LT(2);
    }

    public Token peek(int index) {
        return LT(index);
    }

    public boolean isAtEnd() {
        return peek().isEof();
    }

    private Pair cons(Object first, Object rest) {
        return new Pair(first, rest);
    }

    public static void main(String[] args) {

        Parser parser = new Parser("(lambda x (* x x))");
        while(true) {
            Object expr = parser.read();
            if(expr == null) break;
            System.out.println(SchemeUtils.first(expr));
            System.out.println(SchemeUtils.rest((SchemeUtils.rest(expr))));

        }


    }

}
