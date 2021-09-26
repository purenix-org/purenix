
let
  myEq = a: b: a == b;
in
{ eqBooleanImpl = myEq;

  eqIntImpl = myEq;

  eqNumberImpl = myEq;

  eqCharImpl = myEq;

  eqStringImpl = myEq;

  eqArrayImpl = eq: xs: ys:
    # if the arrrays are different lengths, then they are different
    if builtins.length xs != builtins.length ys then
      false
    else
      let
        len = builtins.length xs;

        go = n:
          # if we've gone through the whole list and never found elements that
          # are non-equal, then the lists are equal
          if n >= len then
            true
          else
            if eq (builtins.elemAt xs n) (builtins.elemAt ys n) then
              # if these elements are equal, then go on to the next one
              go (n + 1)
            else
              # if these elements are not equal, then the lists are not equal
              false;
      in
      go 0;
}
