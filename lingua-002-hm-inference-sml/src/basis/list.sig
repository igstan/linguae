signature LIST =
  sig
    include LIST

    (**
     * An alias for the built-in `hd` function.
     *)
    val head : 'a list -> 'a

    (**
     * An alias for the built-in `tl` function.
     *)
    val tail : 'a list -> 'a list

    (**
     * A curried version of the `::` data constructor.
     *)
    val cons : 'a -> 'a list -> 'a list

    (**
     * Appends an element to the end of the list.
     *)
    val snoc : 'a -> 'a list -> 'a list

    (**
     * All the list elements until the predicate returns `false`.
     *
     * ```sml
     * - Lists.takeWhile (fn a => a < 4) [1,2,3,4,5,6];
     * val it = [1,2,3] : int list
     * ```
     *)
    val takeWhile : ('a -> bool) -> 'a list -> 'a list

    (**
     * All the list elements after which the predicate returns `true`.
     *
     * ```sml
     * - Lists.dropWhile (fn a => a < 4) [1,2,3,4,5,6];
     * val it = [4,5,6] : int list
     * ```
     *)
    val dropWhile : ('a -> bool) -> 'a list -> 'a list

    (**
     * The first element of a list, if at all.
     *
     * ```sml
     * - Lists.headOption ([] : int list);
     * val it = NONE : int option
     * - Lists.headOption [1,2,3];
     * val it = SOME 1 : int option
     * ```
     *)
    val headOption : 'a list -> 'a option

    (**
     * All but first elements of a list, if any.
     *
     * ```sml
     * - Lists.tailOption ([] : int list);
     * val it = NONE : int list option
     * - Lists.tailOption [1,2,3];
     * val it = SOME [2,3] : int list option
     * ```
     *)
    val tailOption : 'a list -> 'a list option

    (**
     * A pair of init and last, if any.
     *
     * ```sml
     * - Lists.initOption ([] : int list);
     * val it = NONE : int list option
     * - Lists.initOption [1,2,3];
     * val it = SOME ([1,2], 3) : int list option
     * ```
     *)
    val initLastOption : 'a list -> ('a list * 'a) option

    (**
     * All but last elements of a list, if any.
     *
     * ```sml
     * - Lists.initOption ([] : int list);
     * val it = NONE : int list option
     * - Lists.initOption [1,2,3];
     * val it = SOME [1,2] : int list option
     * ```
     *)
    val initOption : 'a list -> 'a list option

    (**
     * The last element of a list, if at all.
     *
     * ```sml
     * - Lists.lastOption ([] : int list);
     * val it = NONE : int option
     * - Lists.lastOption [1,2,3];
     * val it = SOME 3 : int option
     * ```
     *)
    val lastOption : 'a list -> 'a option

    (**
     * Interleaves `i` in between successive elements of `l`.
     *
     * ```sml
     * - Lists.interleave 0 [];
     * val it = [] : int list
     * - Lists.interleave 0 [1,2,3];
     * val it = [1,0,2,0,3] : int list
     * ```
     *)
    val interleave : 'a -> 'a list -> 'a list

    (**
     * Map and fold left at the same time.
     *
     * ```sml
     * - Lists.mapFoldl (fn (x, acc) => (x, x :: acc)) [] [1,2,3];
     * val it = ([1,2,3],[3,2,1]) : int list * int list
     * ```
     *)
    val mapFoldl : (('a * 'c) -> ('b * 'c)) -> 'c -> 'a list -> ('b list * 'c)

    (**
     * Map and fold right at the same time.
     *
     * ```sml
     * - Lists.mapFoldr (fn (x, acc) => (x, x :: acc)) [] [1,2,3];
     * val it = ([1,2,3],[1,2,3]) : int list * int list
     * ```
     *)
    val mapFoldr : (('a * 'c) -> ('b * 'c)) -> 'c -> 'a list -> ('b list * 'c)
  end
