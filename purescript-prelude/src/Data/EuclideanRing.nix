
let
  abs = x: if x < 0 then -x else x;

  # taken from nixpkgs/lib/trivial.nix
  mod = base: int: base - (int * (builtins.div base int));
in

{
  intDegree = x: abs x;

  intDiv = x: y: if y == 0 then 0 else x / y;

  intMod = x: y:
    if y == 0 then
      0
    else
      let
        yy = abs y;
      in mod ((mod x yy) + yy) yy;

  numDiv = x: y:
    # Add 0.0 to x to make sure that it is a float and floating division will
    # be used, even if the user accidentally passes in two integers.
    (x + 0.0) / y;
}
