type pos = int
type lexresult = Tokens.token

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
fun err(p1,p2) = ErrorMsg.error p1

val commentDepth = ref 0
fun updateCommentDepth delta = commentDepth := !commentDepth + delta

val stringStart = ref 0
val currentString = ref ""
fun appendCurrentString s = (currentString := !currentString ^ s)

fun stringToInt s = valOf(Int.fromString s)

fun newLine pos = (lineNum := !lineNum + 1; linePos := pos :: !linePos)

fun translateControl s = str (chr (ord (String.sub (s, 1)) - 64))

fun eof() =
    let val pos = hd(!linePos) 
    in 
        if !commentDepth > 0
        then ErrorMsg.error pos ("unclosed comment of depth" ^ Int.toString (!commentDepth))
        else ();
        Tokens.EOF(pos,pos)
    end

%% 

digits=[0-9]+;
letter=[a-zA-Z];
format=[ \t\f];
%s COMMENT STRING ESCAPE FORMAT;

%%

<INITIAL>\n         => (newLine yypos; continue()); 
<INITIAL>while      => (Tokens.WHILE(yypos,yypos+5));
<INITIAL>for        => (Tokens.FOR(yypos,yypos+3));
<INITIAL>to         => (Tokens.TO(yypos,yypos+2));
<INITIAL>break      => (Tokens.BREAK(yypos,yypos+5));
<INITIAL>let        => (Tokens.LET(yypos,yypos+3));
<INITIAL>in         => (Tokens.IN(yypos,yypos+2));
<INITIAL>end        => (Tokens.END(yypos,yypos+3));
<INITIAL>function   => (Tokens.FUNCTION(yypos,yypos+8));
<INITIAL>var  	    => (Tokens.VAR(yypos,yypos+3));
<INITIAL>type       => (Tokens.TYPE(yypos,yypos+4));
<INITIAL>array      => (Tokens.ARRAY(yypos,yypos+5));
<INITIAL>if         => (Tokens.IF(yypos,yypos+2));
<INITIAL>then       => (Tokens.THEN(yypos,yypos+4));
<INITIAL>else       => (Tokens.ELSE(yypos,yypos+4));
<INITIAL>do         => (Tokens.DO(yypos,yypos+2));
<INITIAL>of         => (Tokens.OF(yypos,yypos+2));
<INITIAL>nil        => (Tokens.NIL(yypos,yypos+3));

<INITIAL>","        => (Tokens.COMMA(yypos,yypos+1));
<INITIAL>";"        => (Tokens.SEMICOLON(yypos,yypos+1));
<INITIAL>":"        => (Tokens.COLON(yypos,yypos+1));
<INITIAL>"("        => (Tokens.LPAREN(yypos,yypos+1));
<INITIAL>")"        => (Tokens.RPAREN(yypos,yypos+1));
<INITIAL>"["        => (Tokens.LBRACK(yypos,yypos+1));
<INITIAL>"]"        => (Tokens.RBRACK(yypos,yypos+1));
<INITIAL>"{"        => (Tokens.LBRACE(yypos,yypos+1));
<INITIAL>"}"        => (Tokens.RBRACE(yypos,yypos+1));
<INITIAL>"."        => (Tokens.DOT(yypos,yypos+1));
<INITIAL>"+"        => (Tokens.PLUS(yypos,yypos+1));
<INITIAL>"-"        => (Tokens.MINUS(yypos,yypos+1));
<INITIAL>"*"        => (Tokens.TIMES(yypos,yypos+1));
<INITIAL>"/"        => (Tokens.DIVIDE(yypos,yypos+1));
<INITIAL>"="        => (Tokens.EQ(yypos,yypos+1));
<INITIAL>"<>"       => (Tokens.NEQ(yypos,yypos+2));
<INITIAL>"<"        => (Tokens.LT(yypos,yypos+1));
<INITIAL>"<="       => (Tokens.LE(yypos,yypos+2));
<INITIAL>">"        => (Tokens.GT(yypos,yypos+1));
<INITIAL>"<="       => (Tokens.GE(yypos,yypos+2));
<INITIAL>"&"        => (Tokens.AND(yypos,yypos+1));
<INITIAL>"|"        => (Tokens.OR(yypos,yypos+1));
<INITIAL>":="       => (Tokens.ASSIGN(yypos,yypos+2));

<INITIAL>{digits}   => (Tokens.INT(stringToInt yytext, yypos, yypos + size yytext));
<INITIAL>{letter}+[{letter}{digits}_]*
                    => (Tokens.ID(yytext, yypos, yypos + size yytext));
<INITIAL>" "|\t     => (continue());

<INITIAL>"/*"       => (YYBEGIN COMMENT; updateCommentDepth 1; continue());
<INITIAL>\"         => (YYBEGIN STRING; currentString := ""; stringStart := yypos; continue());
<INITIAL>.          => (ErrorMsg.error yypos ("illegal character " ^ yytext); continue());

<COMMENT>"/*"       => (updateCommentDepth 1; continue());
<COMMENT>"*/"       => (updateCommentDepth ~1; if !commentDepth = 0 then YYBEGIN INITIAL else (); continue());
<COMMENT>\n         => (newLine yypos; continue());
<COMMENT>.          => (continue());

<STRING>\"          => (YYBEGIN INITIAL; Tokens.STRING(!currentString, !stringStart, yypos+1));
<STRING>\\          => (YYBEGIN ESCAPE; continue());
<STRING>\n          => (ErrorMsg.error yypos ("unclosed string"); newLine yypos; YYBEGIN INITIAL; continue());
<STRING>.           => (appendCurrentString yytext; continue());

<ESCAPE>n           => (appendCurrentString "\n"; YYBEGIN STRING; continue());
<ESCAPE>t           => (appendCurrentString "\t"; YYBEGIN STRING; continue());
<ESCAPE>\\\^[@A-Z\[\\\]\^_]
                    => (appendCurrentString (translateControl yytext); continue());
<ESCAPE>\ddd        => (appendCurrentString (str (chr (stringToInt yytext))); YYBEGIN STRING; continue());
<ESCAPE>\"|\\       => (appendCurrentString yytext; YYBEGIN STRING; continue());
<ESCAPE>\n          => (newLine yypos; YYBEGIN FORMAT; continue());
<ESCAPE>{format}    => (YYBEGIN FORMAT; continue());
<ESCAPE>.           => (err (yypos-1, yypos+1)
                        ("illegal escape character \\" ^ yytext
                         ^ (if yytext = "^"
                            then " (may be illegal control character)"
                            else ""));
                        YYBEGIN STRING; continue());

<FORMAT>\n          => (newLine yypos; continue());
<FORMAT>{format}    => (continue());
<FORMAT>"\\"        => (YYBEGIN STRING; continue());
<FORMAT>.           => (ErrorMsg.error yypos ("illegal format character " ^ yytext); continue());
