
let
  # Drop the number of elements from the start of the list.
  #
  # > drop 3 [1 2 3 4 5 6 7]
  # [4 5 6 7]
  drop = i: xs:
    builtins.genList (n: builtins.elemAt xs (n + i)) (builtins.length xs - i);
in

{ arrayExtend = f: xs: builtins.genList (n: f (drop n xs)) (builtins.length xs);
}
