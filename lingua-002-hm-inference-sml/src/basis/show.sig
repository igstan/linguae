signature SHOW =
  sig
    type 'a show

    val println : string -> unit
    val int : int show
    val bool : bool show
    val char : char show
    val real : real show
    val word : word show
    val unit : unit show
    val string : string show
    val list : 'a show -> 'a list show
    val array : 'a show -> 'a array show
    val vector : 'a show -> 'a vector show
    val option : 'a show -> 'a option show
    val tuple2 : 'a show * 'b show -> ('a * 'b) show
    val tuple3 : 'a show * 'b show * 'c show -> ('a * 'b * 'c) show
    val tuple4 : 'a show * 'b show * 'c show * 'd show -> ('a * 'b * 'c * 'd) show
    val tuple5 : 'a show * 'b show * 'c show * 'd show * 'e show -> ('a * 'b * 'c * 'd * 'e) show
    val tuple6 : 'a show * 'b show * 'c show * 'd show * 'e show * 'f show -> ('a * 'b * 'c * 'd * 'e * 'f) show
    val tuple7 : 'a show * 'b show * 'c show * 'd show * 'e show * 'f show * 'g show -> ('a * 'b * 'c * 'd * 'e * 'f * 'g) show
  end
