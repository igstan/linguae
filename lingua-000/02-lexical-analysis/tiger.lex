structure T = Token

type pos = int
type lexresult = T.token

val lineNum = Lexer.lineNum
val linePos = Lexer.linePos
val strBuff = Lexer.stringBuffer
val commentNesting = Lexer.commentNesting

fun eof () =
  let
    val p = hd (!linePos)
    val n = !commentNesting
    val _ = commentNesting := 0
  in
    if n = 0 then
      T.EOF (p, p)
    else
      raise Lexer.Error (p, "unclosed comment")
  end

%%
%s COMMENT STRING ESCAPE;

digits = [0-9]+;
id     = [a-z][a-zA-Z0-9_]*;
ws     = ("\012"|[\t\ ])*;
cr     = "\013";
lf     = "\010";
eol    = ({cr}{lf}|{lf}|{cr});

%%
<INITIAL>{ws}             => ( continue () );
<INITIAL>{eol}            => ( lineNum := !lineNum + 1; linePos := yypos :: !linePos; continue () );
<INITIAL>type             => ( T.TYPE(yypos, yypos + size yytext) );
<INITIAL>var              => ( T.VAR(yypos, yypos + size yytext) );
<INITIAL>function         => ( T.FUNCTION(yypos, yypos + size yytext) );
<INITIAL>break            => ( T.BREAK(yypos, yypos + size yytext) );
<INITIAL>of               => ( T.OF(yypos, yypos + size yytext) );
<INITIAL>end              => ( T.END(yypos, yypos + size yytext) );
<INITIAL>in               => ( T.IN(yypos, yypos + size yytext) );
<INITIAL>nil              => ( T.NIL(yypos, yypos + size yytext) );
<INITIAL>let              => ( T.LET(yypos, yypos + size yytext) );
<INITIAL>do               => ( T.DO(yypos, yypos + size yytext) );
<INITIAL>to               => ( T.TO(yypos, yypos + size yytext) );
<INITIAL>for              => ( T.FOR(yypos, yypos + size yytext) );
<INITIAL>while            => ( T.WHILE(yypos, yypos + size yytext) );
<INITIAL>else             => ( T.ELSE(yypos, yypos + size yytext) );
<INITIAL>then             => ( T.THEN(yypos, yypos + size yytext) );
<INITIAL>if               => ( T.IF(yypos, yypos + size yytext) );
<INITIAL>array            => ( T.ARRAY(yypos, yypos + size yytext) );
<INITIAL>assign           => ( T.ASSIGN(yypos, yypos + size yytext) );
<INITIAL>or               => ( T.OR(yypos, yypos + size yytext) );
<INITIAL>and              => ( T.AND(yypos, yypos + size yytext) );
<INITIAL>">="             => ( T.GE(yypos, yypos + size yytext) );
<INITIAL>">"              => ( T.GT(yypos, yypos + size yytext) );
<INITIAL>"<="             => ( T.LE(yypos, yypos + size yytext) );
<INITIAL>"<"              => ( T.LT(yypos, yypos + size yytext) );
<INITIAL>"<>"             => ( T.NEQ(yypos, yypos + size yytext) );
<INITIAL>"="              => ( T.EQ(yypos, yypos + size yytext) );
<INITIAL>"/"              => ( T.DIVIDE(yypos, yypos + size yytext) );
<INITIAL>"*"              => ( T.TIMES(yypos, yypos + size yytext) );
<INITIAL>"-"              => ( T.MINUS(yypos, yypos + size yytext) );
<INITIAL>"+"              => ( T.PLUS(yypos, yypos + size yytext) );
<INITIAL>"."              => ( T.DOT(yypos, yypos + size yytext) );
<INITIAL>"}"              => ( T.RBRACE(yypos, yypos + size yytext) );
<INITIAL>"{"              => ( T.LBRACE(yypos, yypos + size yytext) );
<INITIAL>"]"              => ( T.RBRACK(yypos, yypos + size yytext) );
<INITIAL>"["              => ( T.LBRACK(yypos, yypos + size yytext) );
<INITIAL>")"              => ( T.RPAREN(yypos, yypos + size yytext) );
<INITIAL>"("              => ( T.LPAREN(yypos, yypos + size yytext) );
<INITIAL>";"              => ( T.SEMICOLON(yypos, yypos + size yytext) );
<INITIAL>":"              => ( T.COLON(yypos, yypos + size yytext) );
<INITIAL>","              => ( T.COMMA(yypos, yypos + size yytext) );
<INITIAL>{digits}         => ( T.INT(Option.valOf (Int.fromString yytext), yypos, yypos + size yytext) );
<INITIAL>{id}             => ( T.ID(yytext, yypos, yypos + size yytext) );
<INITIAL>"\""             => ( YYBEGIN STRING; strBuff := ""; continue () );
<INITIAL>"/*"             => ( YYBEGIN COMMENT; commentNesting := !commentNesting + 1; continue () );
<INITIAL>.                => ( Lexer.error yypos ("illegal character " ^ yytext); continue () );

<STRING>\\                => ( YYBEGIN ESCAPE; strBuff := !strBuff ^ yytext; continue () );
<STRING>"\""              => ( YYBEGIN INITIAL; T.STRING(!strBuff, yypos, yypos + size yytext) );
<STRING>.                 => ( strBuff := !strBuff ^ yytext; continue () );

<ESCAPE>n                 => ( YYBEGIN STRING; strBuff := (!strBuff) ^ yytext; continue () );
<ESCAPE>t                 => ( YYBEGIN STRING; strBuff := (!strBuff) ^ yytext; continue () );
<ESCAPE>^[a-zA-Z]         => ( YYBEGIN STRING; strBuff := (!strBuff) ^ yytext; continue () );
<ESCAPE>"\""              => ( YYBEGIN STRING; strBuff := (!strBuff) ^ yytext; continue () );
<ESCAPE>[\n\t\f\v\r]*\\   => ( YYBEGIN STRING; strBuff := (!strBuff) ^ yytext; continue () );
<ESCAPE>.                 => ( Lexer.error yypos ("illegal character " ^ yytext); continue () );

<COMMENT>"/*"             => ( commentNesting := !commentNesting + 1; continue () );
<COMMENT>"*/"             => ( commentNesting := !commentNesting - 1; if !commentNesting = 0 then YYBEGIN INITIAL else (); continue () );
<COMMENT>\n               => ( continue () );
<COMMENT>.                => ( continue () );
