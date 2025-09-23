use string_interner::StringInterner;
use volette::compiler::lexer::Lexer;

const CONTENTS: &str = r#"fn add(u32)(a: u32, b: u32): u32 {
    return a + b;
}

struct Point {
    x: f64,
    // comment
y: f64,
}

fn dist(p1: Point, p2: Point): f64 {
    /*let dx = p1.x - p2.x;
let dy = p1.y - p2.y; */
    return (dx * dx + dy * dy) ** 0.5;
}

const SIZE = 10;

fn fill(arr: [u8; SIZE], val: u8): Nil {
    let i = 0;
    loop {
        if i >= SIZE {
            break;
        }
        arr[i] = val;
        i = i + 1;
    }
    Nil
}

fn fact(n: u64): u64 {
    if n <= 1 {
        return 1;
    }
    return n * fact(n - 1);
}

pub fn max(a: i32, b: i32): i32 {
    if a > b {
        return a;
    }
    return b;
}"#;

#[test]
fn test_lexer() {
    let mut interner = StringInterner::new();
    let s = interner.get_or_intern("test.vt");
    let mut lexer = Lexer::new(&mut interner, s);
    lexer.tokenize(CONTENTS.chars().collect());
    let expected_tokens = vec![
        ("Keyword(Fn)", 1, (1, 2)),
        ("Identifier(add)", 1, (4, 6)),
        ("Punctuation(OpenParen)", 1, (7, 7)),
        ("TypeLiteral(U32)", 1, (8, 10)),
        ("Punctuation(CloseParen)", 1, (11, 11)),
        ("Punctuation(OpenParen)", 1, (12, 12)),
        ("Identifier(a)", 1, (13, 13)),
        ("Punctuation(Colon)", 1, (14, 14)),
        ("TypeLiteral(U32)", 1, (16, 18)),
        ("Punctuation(Comma)", 1, (19, 19)),
        ("Identifier(b)", 1, (21, 21)),
        ("Punctuation(Colon)", 1, (22, 22)),
        ("TypeLiteral(U32)", 1, (24, 26)),
        ("Punctuation(CloseParen)", 1, (27, 27)),
        ("Punctuation(Colon)", 1, (28, 28)),
        ("TypeLiteral(U32)", 1, (30, 32)),
        ("Punctuation(OpenBrace)", 1, (34, 34)),
        ("Keyword(Return)", 2, (5, 10)),
        ("Identifier(a)", 2, (12, 12)),
        ("Punctuation(Plus)", 2, (14, 14)),
        ("Identifier(b)", 2, (16, 16)),
        ("Punctuation(Semicolon)", 2, (17, 17)),
        ("Punctuation(CloseBrace)", 3, (1, 1)),
        ("Keyword(Struct)", 5, (1, 6)),
        ("Identifier(Point)", 5, (8, 12)),
        ("Punctuation(OpenBrace)", 5, (14, 14)),
        ("Identifier(x)", 6, (5, 5)),
        ("Punctuation(Colon)", 6, (6, 6)),
        ("TypeLiteral(F64)", 6, (8, 10)),
        ("Punctuation(Comma)", 6, (11, 11)),
        ("Identifier(y)", 8, (1, 1)),
        ("Punctuation(Colon)", 8, (2, 2)),
        ("TypeLiteral(F64)", 8, (4, 6)),
        ("Punctuation(Comma)", 8, (7, 7)),
        ("Punctuation(CloseBrace)", 9, (1, 1)),
        ("Keyword(Fn)", 11, (1, 2)),
        ("Identifier(dist)", 11, (4, 7)),
        ("Punctuation(OpenParen)", 11, (8, 8)),
        ("Identifier(p1)", 11, (9, 10)),
        ("Punctuation(Colon)", 11, (11, 11)),
        ("Identifier(Point)", 11, (13, 17)),
        ("Punctuation(Comma)", 11, (18, 18)),
        ("Identifier(p2)", 11, (20, 21)),
        ("Punctuation(Colon)", 11, (22, 22)),
        ("Identifier(Point)", 11, (24, 28)),
        ("Punctuation(CloseParen)", 11, (29, 29)),
        ("Punctuation(Colon)", 11, (30, 30)),
        ("TypeLiteral(F64)", 11, (32, 34)),
        ("Punctuation(OpenBrace)", 11, (36, 36)),
        ("Keyword(Return)", 14, (5, 10)),
        ("Punctuation(OpenParen)", 14, (12, 12)),
        ("Identifier(dx)", 14, (13, 14)),
        ("Punctuation(Star)", 14, (16, 16)),
        ("Identifier(dx)", 14, (18, 19)),
        ("Punctuation(Plus)", 14, (21, 21)),
        ("Identifier(dy)", 14, (23, 24)),
        ("Punctuation(Star)", 14, (26, 26)),
        ("Identifier(dy)", 14, (28, 29)),
        ("Punctuation(CloseParen)", 14, (30, 30)),
        ("Punctuation(StarStar)", 14, (32, 33)),
        ("FloatLiteral(0.5)", 14, (35, 37)),
        ("Punctuation(Semicolon)", 14, (38, 38)),
        ("Punctuation(CloseBrace)", 15, (1, 1)),
        ("Keyword(Const)", 17, (1, 5)),
        ("Identifier(SIZE)", 17, (7, 10)),
        ("Punctuation(Eq)", 17, (12, 12)),
        ("IntLiteral(10)", 17, (14, 15)),
        ("Punctuation(Semicolon)", 17, (16, 16)),
        ("Keyword(Fn)", 19, (1, 2)),
        ("Identifier(fill)", 19, (4, 7)),
        ("Punctuation(OpenParen)", 19, (8, 8)),
        ("Identifier(arr)", 19, (9, 11)),
        ("Punctuation(Colon)", 19, (12, 12)),
        ("Punctuation(OpenBracket)", 19, (14, 14)),
        ("TypeLiteral(U8)", 19, (15, 16)),
        ("Punctuation(Semicolon)", 19, (17, 17)),
        ("Identifier(SIZE)", 19, (19, 22)),
        ("Punctuation(CloseBracket)", 19, (23, 23)),
        ("Punctuation(Comma)", 19, (24, 24)),
        ("Identifier(val)", 19, (26, 28)),
        ("Punctuation(Colon)", 19, (29, 29)),
        ("TypeLiteral(U8)", 19, (31, 32)),
        ("Punctuation(CloseParen)", 19, (33, 33)),
        ("Punctuation(Colon)", 19, (34, 34)),
        ("TypeLiteral(Nil)", 19, (36, 38)),
        ("Punctuation(OpenBrace)", 19, (40, 40)),
        ("Keyword(Let)", 20, (5, 7)),
        ("Identifier(i)", 20, (9, 9)),
        ("Punctuation(Eq)", 20, (11, 11)),
        ("IntLiteral(0)", 20, (13, 13)),
        ("Punctuation(Semicolon)", 20, (14, 14)),
        ("Keyword(Loop)", 21, (5, 8)),
        ("Punctuation(OpenBrace)", 21, (10, 10)),
        ("Keyword(If)", 22, (9, 10)),
        ("Identifier(i)", 22, (12, 12)),
        ("Punctuation(GreaterThanOrEq)", 22, (14, 15)),
        ("Identifier(SIZE)", 22, (17, 20)),
        ("Punctuation(OpenBrace)", 22, (22, 22)),
        ("Keyword(Break)", 23, (13, 17)),
        ("Punctuation(Semicolon)", 23, (18, 18)),
        ("Punctuation(CloseBrace)", 24, (9, 9)),
        ("Identifier(arr)", 25, (9, 11)),
        ("Punctuation(OpenBracket)", 25, (12, 12)),
        ("Identifier(i)", 25, (13, 13)),
        ("Punctuation(CloseBracket)", 25, (14, 14)),
        ("Punctuation(Eq)", 25, (16, 16)),
        ("Identifier(val)", 25, (18, 20)),
        ("Punctuation(Semicolon)", 25, (21, 21)),
        ("Identifier(i)", 26, (9, 9)),
        ("Punctuation(Eq)", 26, (11, 11)),
        ("Identifier(i)", 26, (13, 13)),
        ("Punctuation(Plus)", 26, (15, 15)),
        ("IntLiteral(1)", 26, (17, 17)),
        ("Punctuation(Semicolon)", 26, (18, 18)),
        ("Punctuation(CloseBrace)", 27, (5, 5)),
        ("TypeLiteral(Nil)", 28, (5, 7)),
        ("Punctuation(CloseBrace)", 29, (1, 1)),
        ("Keyword(Fn)", 31, (1, 2)),
        ("Identifier(fact)", 31, (4, 7)),
        ("Punctuation(OpenParen)", 31, (8, 8)),
        ("Identifier(n)", 31, (9, 9)),
        ("Punctuation(Colon)", 31, (10, 10)),
        ("TypeLiteral(U64)", 31, (12, 14)),
        ("Punctuation(CloseParen)", 31, (15, 15)),
        ("Punctuation(Colon)", 31, (16, 16)),
        ("TypeLiteral(U64)", 31, (18, 20)),
        ("Punctuation(OpenBrace)", 31, (22, 22)),
        ("Keyword(If)", 32, (5, 6)),
        ("Identifier(n)", 32, (8, 8)),
        ("Punctuation(LessThanOrEq)", 32, (10, 11)),
        ("IntLiteral(1)", 32, (13, 13)),
        ("Punctuation(OpenBrace)", 32, (15, 15)),
        ("Keyword(Return)", 33, (9, 14)),
        ("IntLiteral(1)", 33, (16, 16)),
        ("Punctuation(Semicolon)", 33, (17, 17)),
        ("Punctuation(CloseBrace)", 34, (5, 5)),
        ("Keyword(Return)", 35, (5, 10)),
        ("Identifier(n)", 35, (12, 12)),
        ("Punctuation(Star)", 35, (14, 14)),
        ("Identifier(fact)", 35, (16, 19)),
        ("Punctuation(OpenParen)", 35, (20, 20)),
        ("Identifier(n)", 35, (21, 21)),
        ("Punctuation(Minus)", 35, (23, 23)),
        ("IntLiteral(1)", 35, (25, 25)),
        ("Punctuation(CloseParen)", 35, (26, 26)),
        ("Punctuation(Semicolon)", 35, (27, 27)),
        ("Punctuation(CloseBrace)", 36, (1, 1)),
        ("Keyword(Pub)", 38, (1, 3)),
        ("Keyword(Fn)", 38, (5, 6)),
        ("Identifier(max)", 38, (8, 10)),
        ("Punctuation(OpenParen)", 38, (11, 11)),
        ("Identifier(a)", 38, (12, 12)),
        ("Punctuation(Colon)", 38, (13, 13)),
        ("TypeLiteral(I32)", 38, (15, 17)),
        ("Punctuation(Comma)", 38, (18, 18)),
        ("Identifier(b)", 38, (20, 20)),
        ("Punctuation(Colon)", 38, (21, 21)),
        ("TypeLiteral(I32)", 38, (23, 25)),
        ("Punctuation(CloseParen)", 38, (26, 26)),
        ("Punctuation(Colon)", 38, (27, 27)),
        ("TypeLiteral(I32)", 38, (29, 31)),
        ("Punctuation(OpenBrace)", 38, (33, 33)),
        ("Keyword(If)", 39, (5, 6)),
        ("Identifier(a)", 39, (8, 8)),
        ("Punctuation(GreaterThan)", 39, (10, 10)),
        ("Identifier(b)", 39, (12, 12)),
        ("Punctuation(OpenBrace)", 39, (14, 14)),
        ("Keyword(Return)", 40, (9, 14)),
        ("Identifier(a)", 40, (16, 16)),
        ("Punctuation(Semicolon)", 40, (17, 17)),
        ("Punctuation(CloseBrace)", 41, (5, 5)),
        ("Keyword(Return)", 42, (5, 10)),
        ("Identifier(b)", 42, (12, 12)),
        ("Punctuation(Semicolon)", 42, (13, 13)),
        ("Punctuation(CloseBrace)", 43, (1, 1)),
        // ("Eof", 43, (1, 1)),
    ]
    .iter()
    .map(|(kind, line, (start, end))| (kind.to_string(), line.to_owned(), (start.to_owned(), end.to_owned())))
    .collect::<Vec<_>>();

    let tokens = lexer.format_tokens();

    assert_eq!(tokens.len() - 1, expected_tokens.len());
    for (i, token) in tokens.iter().skip(1).enumerate() {
        assert_eq!(token, &expected_tokens[i]);
    }
}
