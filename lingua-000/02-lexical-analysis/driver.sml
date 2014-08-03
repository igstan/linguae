structure Parse =
struct
  fun parse filename =
    let
      val file = TextIO.openIn filename
      fun get _ = TextIO.input file
      val lexer = Mlex.makeLexer get
      fun lex () =
        let
          val t = lexer()
        in
          print (Token.toString t);
          print "\n";
          case t of
            Token.EOF(_, _) => ()
          | _               => lex ()
        end
    in
      lex ();
      TextIO.closeIn file
    end
end
