signature LEXER =
sig
  exception Error of int * string

  val stringBuffer : string ref
  val commentNesting : int ref
  val anyErrors : bool ref
  val fileName : string ref
  val lineNum : int ref
  val linePos : int list ref
  val sourceStream : TextIO.instream ref
  val error : int -> string -> unit
  val impossible : string -> unit
  val reset : unit -> unit
end

structure Lexer : LEXER =
struct
  exception Error of int * string

  val stringBuffer = ref ""
  val commentNesting = ref 0
  val anyErrors = ref false
  val fileName = ref ""
  val lineNum = ref 1
  val linePos = ref [1]
  val sourceStream = ref TextIO.stdIn

  fun reset () = (
    stringBuffer := "";
    commentNesting := 0;
    anyErrors := false;
    fileName := "";
    lineNum := 1;
    linePos := [1];
    sourceStream := TextIO.stdIn
  )

  fun error pos (msg: string) =
    let
      fun look (a :: rest, n) =
        if a < pos then
          app print [":", Int.toString n, ".", Int.toString (pos - a)]
        else
          look (rest, n - 1)
      | look _ = print "0.0"
    in
      anyErrors := true;
      print (!fileName);
      look(!linePos, !lineNum);
      print ":";
      print msg;
      print "\n"
    end

  fun impossible msg = (
    app print ["Error: Compiler bug: ",msg,"\n"];
    TextIO.flushOut TextIO.stdOut
  )
end
