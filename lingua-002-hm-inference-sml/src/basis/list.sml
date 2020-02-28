structure List : LIST =
struct
  open List

  val head = hd
  val tail = tl

  fun cons x xs = x :: xs

  fun snoc x xs = xs @ [x]

  fun takeWhile predicate xs =
    let
      fun recur xs result =
        case xs of
          [] => rev result
        | x :: xs => if predicate x then recur xs (x :: result) else rev result
    in
      recur xs []
    end

  fun dropWhile predicate xs =
    case xs of
      [] => []
    | x :: rest => if predicate x then dropWhile predicate rest else xs

  fun headOption xs = case xs of [] => NONE | x :: _  => SOME x

  fun tailOption xs = case xs of [] => NONE | _ :: xs => SOME xs

  fun initLastOption xs =
    let
      fun recur xs result =
        case xs of
          [] => NONE
        | x :: [] => SOME (rev result, x)
        | x :: xs => recur xs (x :: result)
    in
      recur xs []
    end

  fun initOption xs =
    Option.map #1 (initLastOption xs)

  fun lastOption xs =
    case xs of
      [] => NONE
    | x :: [] => SOME x
    | x :: xs => lastOption xs

  fun interleave i l =
    let
      fun recur [] acc = rev acc
        | recur (x :: []) acc = recur [] (x :: acc)
        | recur (x :: xs) acc = recur xs (i :: x :: acc)
    in
      recur l []
    end

  local
    open Tuples

    fun prepend xs x = x :: xs
    fun folder mapFolder (x, (result, acc)) =
      Tuple2.map1 (prepend result) (mapFolder (x, acc))
  in
    fun mapFoldl mapFolder seed xs =
      Tuple2.map1 rev (foldl (folder mapFolder) ([], seed) xs)

    fun mapFoldr mapFolder seed xs =
      foldr (folder mapFolder) ([], seed) xs
  end
end
