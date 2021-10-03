
let
  myOrd = lt: eq: gt: a: b:
    if a < b
      then lt
      else
        if a == b then eq else gt;

  # TODO: A while loop incrementing the index of the array and using
  # builtins.elemAt may be faster than builtins.head and builtins.tail.
  arrOrd = compare: arrA: arrB:
    if arrA == [] then
      if arrB == [] then
        0
      else
        1
    else
      if arrB == [] then
        -1
      else
        let
          rel = compare (builtins.head arrA) (builtins.head arrB);
         in
           if rel == 0 then
             arrOrd compare (builtins.tail arrA) (builtins.tail arrB)
           else
             rel;

  # In PureScript, true > false.  But in Nix, we can't directly compare
  # booleans, so we need this function.
  ordBooleanImpl = lt: eq: gt: a: b:
    if a && !b then
      gt
    else
      if !a && b then
        lt
      else
        eq;
in

{ inherit ordBooleanImpl;
  ordIntImpl = myOrd;
  ordNumberImpl = myOrd;
  ordStringImpl = myOrd;
  ordCharImpl = myOrd;
  ordArrayImpl = arrOrd;
}
