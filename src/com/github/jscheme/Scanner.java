package com.github.jscheme;

public class Scanner extends SchemeUtils {


    private final char EOF = (char)-1;
    private int start = 0;
    private int current = 0;
    private int line = 1;
    private int col = 1;
    private final String source;
    public Scanner(String source) {
        this.source = source;
    }

    private boolean isEof() {
        return current >= source.length();
    }

    private char advance() {
        current++;
        char c = source.charAt(current - 1);
        if (c == '\n') {
            line++;
            col = 0;
        } else {
            col++;
        }
        return c;
    }
    private char peek() {
        return isEof() ? EOF : source.charAt(current);
    }

    public char peekNext() {
        if (current + 1 >= source.length()) return EOF;
        return source.charAt(current + 1);
    }

    public Token nextToken() {
        while(!isEof()) {
            start = current;
            char c = advance();
            switch(c) {
                case ' ':
                case '\t':
                case '\r':
                case '\n': ws(); continue;
                case ';': comment(); return nextToken();
                case '\"': return string();
                case '\'': return createToken(TokenType.QUOTE);
                case '(': return createToken(TokenType.LEFT_PAREN);
                case ')': return createToken(TokenType.RIGHT_PAREN);
                case '#': {
                    switch(peek()) {
                        case 't':
                        case 'T':
                            advance();
                            return createToken(TokenType.TRUE, "true", TRUE);
                        case 'f':
                        case 'F':
                            advance();
                            return createToken(TokenType.FALSE, "false", FALSE);
                        default:
                            return nextToken();
                    }
                }
                default:
                    if( (c == '-' && isDigit(peekNext())) || isDigit(c)) {
                        return number();
                    } else  {
                        return symbol();
                    }
            }
        }

        return new Token(TokenType.EOF, line, col, null, null);

    }

    private Token symbol() {

        while (!Character.isWhitespace( peek()) && peek() != EOF &&
                peek() != '(' && peek() != ')' && peek() != '\'' && peek() != ';'
                && peek() != '"' && peek() != ',' && peek() != '`') advance();

        String text = source.substring(start, current);

        return createToken(TokenType.SYMBOL, text, text);

    }

    private Token number() {
        if(peek() == '-') advance(); //consume negative '-'

        while (isDigit(peek())) advance();

        if (peek() == '.' && isDigit(peekNext())) {
            // Consume the "."
            advance();

            while (isDigit(peek())) advance();
        }

        String numberStr = source.substring(start, current);

        return createToken(TokenType.NUMBER,  numberStr, Double.parseDouble(numberStr));
    }
    private Token string() {
        while(peek() != '"' && !isEof()) advance();
        if(isEof()) {
            error(line , "Unterminated string.");
            return null;
        }
        advance(); //consume the character "
        String str = source.substring(start + 1, current - 1);
        return createToken(TokenType.STRING, str, str);
    }

    private Token createToken(TokenType tokenType) {
        return new Token(tokenType, line, col, source.substring(start, current), null);
    }

    private Token createToken(TokenType tokenType, String lexeme, Object value) {
        return new Token(tokenType, line, col, lexeme, value);
    }

    private void ws() {
        while((peek() == ' ' || peek() == '\t' || peek() == '\r' || peek() == '\n')
                && !isEof()) advance();
    }

    private void comment() {
        while(peek() != '\n' && !isEof()) advance();
    }

    private boolean match(char expected) {
        if (isEof()) return false;
        if (source.charAt(current) != expected) return false;

        current++;
        return true;
    }

    private boolean isAlpha(char c) {
        return (c >= 'a' && c <= 'z') ||
                (c >= 'A' && c <= 'Z') ||
                c == '_';
    }

    private boolean isAlphaNumeric(char c) {
        return isAlpha(c) || isDigit(c);
    }

    private boolean isDigit(char c) {
        return c >= '0' && c <= '9';
    }

    static void error(int line, String message) {
        report(line, "", message);
    }

    static private void report(int line, String where, String message) {
        System.err.println(
                "[line " + line + "] Error" + where + ": " + message);

    }

    public static void main(String[] args) {
        Scanner scanner = new Scanner("(define (square x) (* x x))\n(define x 1.0)");
        while(true) {
            Token token = scanner.nextToken();
            if(token.isEof()) break;
            System.out.println(token.toString());
        }
    }
}
