
# foreign import ordIntImpl :: Ordering -> Ordering -> Ordering -> Int -> Int -> Ordering

# foreign import ordNumberImpl :: Ordering -> Ordering -> Ordering -> Number -> Number -> Ordering

# foreign import ordStringImpl :: Ordering -> Ordering -> Ordering -> String -> String -> Ordering

# foreign import ordCharImpl :: Ordering -> Ordering -> Ordering -> Char -> Char -> Ordering

# foreign import ordArrayImpl :: forall a. (a -> a -> Int) -> Array a -> Array a -> Int

let
  myOrd = lt: eq: gt: a: b:
    if a < b
      then lt
      else
        if a == b then eq else gt;

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
in

{ ordIntImpl = myOrd;
  ordNumberImpl = myOrd;
  ordStringImpl = myOrd;
  ordCharImpl = myOrd;
  ordArrayImpl = arrOrd;
}
