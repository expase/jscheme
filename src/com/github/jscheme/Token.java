package com.github.jscheme;

public class Token {
    public final TokenType tokenType;
    public final int line;
    public final int col;
    public final String lexeme;
    public final Object literal;
    public Token(TokenType tokenType,int line,int col,String lexeme,Object literal) {
        this.tokenType = tokenType;
        this.line = line;
        this.col = col;
        this.lexeme = lexeme;
        this.literal = literal;
    }

    public boolean isEof() {
        return tokenType.equals(TokenType.EOF);
    }

    public String toString() {
        return lexeme + "[" + tokenType + "]";

    }
}
