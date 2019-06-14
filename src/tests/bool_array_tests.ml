open Base
module B = Dataframe.For_testing_only.Bool_array

let print_ba ba =
  let str1 =
    List.init (B.length ba) ~f:(fun i -> if B.get ba i then '1' else '0')
    |> String.of_char_list
  in
  let str2 =
    let acc = Array.create 'X' ~len:(B.length ba) in
    B.iteri ba ~f:(fun i b -> acc.(i) <- (if b then '1' else '0'));
    Array.to_list acc |> String.of_char_list
  in
  if String.(str1 <> str2) then Stdio.printf "Incoherent strings: %s %s\n%!" str1 str2;
  Stdio.printf "%d %d %s\n%!" (B.length ba) (B.num_set ba) str1

let%expect_test _ =
  let ba = B.create true ~len:42 in
  print_ba ba;
  [%expect {| 42 42 111111111111111111111111111111111111111111 |}];
  let ba =
    B.mapi ba ~f:(fun i b ->
        assert b;
        i % 2 = 0)
  in
  print_ba ba;
  [%expect {| 42 21 101010101010101010101010101010101010101010 |}];
  let ba = B.mapi ba ~f:(fun i b -> b && i % 3 = 0) in
  print_ba ba;
  [%expect {| 42 7 100000100000100000100000100000100000100000 |}];
  let indexes =
    B.indexes ba ~value:true
    |> Array.map ~f:Int.to_string
    |> String.concat_array ~sep:" "
  in
  Stdio.print_endline indexes;
  [%expect {| 0 6 12 18 24 30 36 |}];
  let indexes =
    B.indexes ba ~value:false
    |> Array.map ~f:Int.to_string
    |> String.concat_array ~sep:" "
  in
  Stdio.print_endline indexes;
  [%expect
    {| 1 2 3 4 5 7 8 9 10 11 13 14 15 16 17 19 20 21 22 23 25 26 27 28 29 31 32 33 34 35 37 38 39 40 41 |}]

let%expect_test _ =
  let from_indexes indexes ~len =
    let ba = B.Mutable.create false ~len in
    List.iter indexes ~f:(fun index -> B.Mutable.set ba index true);
    let ba = B.Mutable.finish ba in
    print_ba ba;
    let indexes' = B.indexes ba ~value:true |> Array.to_list in
    let indexes =
      List.dedup_and_sort indexes ~compare:Int.compare
      |> List.map ~f:Int.to_string
      |> String.concat ~sep:" "
    in
    let indexes' = List.map indexes' ~f:Int.to_string |> String.concat ~sep:" " in
    if String.(indexes <> indexes') then Stdio.printf "%s <> %s\n%!" indexes indexes';
    let flipped = B.Mutable.create true ~len in
    B.iteri ba ~f:(fun i b -> if b then B.Mutable.set flipped i false);
    let flipped = B.Mutable.finish flipped in
    print_ba flipped
  in
  from_indexes [] ~len:0;
  [%expect {|
    0 0
    0 0 |}];
  from_indexes [] ~len:4;
  [%expect {|
    4 0 0000
    4 4 1111 |}];
  from_indexes [ 0; 1; 2; 3 ] ~len:4;
  [%expect {|
    4 4 1111
    4 0 0000 |}];
  from_indexes [ 3; 2; 1 ] ~len:4;
  [%expect {|
    4 3 0111
    4 1 1000 |}];
  from_indexes [ 3; 2; 1; 3; 2; 1 ] ~len:4;
  [%expect {|
    4 3 0111
    4 1 1000 |}]

let%expect_test _ =
  let test bool_array =
    let ba = B.Mutable.create false ~len:(Array.length bool_array) in
    Array.iteri bool_array ~f:(B.Mutable.set ba);
    let ba = B.Mutable.finish ba in
    let bool_array' = Array.create false ~len:(Array.length bool_array) in
    B.iteri ba ~f:(Array.set bool_array');
    let to_str x =
      Array.map x ~f:(fun b -> if b then "1" else "0") |> String.concat_array
    in
    let str = to_str bool_array in
    let str' = to_str bool_array' in
    if String.( <> ) str str' then Stdio.printf "%s <> %s\n%!" str str'
  in
  for len = 0 to 18 do
    for cfg = 0 to (1 lsl len) - 1 do
      let bool_array = Array.create false ~len in
      for i = 0 to len - 1 do
        bool_array.(i) <- cfg land (1 lsl i) <> 0
      done;
      test bool_array
    done
  done;
  [%expect {||}]
