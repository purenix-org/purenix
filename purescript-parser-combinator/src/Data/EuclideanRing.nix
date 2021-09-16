
let
  abs = x: if x < 0 then -x else x;

  # taken from nixpkgs/lib/trivial.nix
  mod = base: int: base - (int * (builtins.div base int));
in

{ maxInt = 9223372036854775807;
, minInt = -9223372036854775807;

  # TODO: What is this function?
, intDegree = x: abs x;

, intMod = x: y:
    if y == 0 then
      0
    else
      let
        yy = abs y;
      in (mod (mod x yy) + yy) yy)

, numDiv = x: y: x / y;
}
