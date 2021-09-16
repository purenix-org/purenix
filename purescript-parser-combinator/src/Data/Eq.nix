
let
  myEq = a: b: a == b;
in
{ eqBooleanImpl = myEq;
, eqIntImpl = myEq;
, eqNumberImpl = myEq;
, eqCharImpl = myEq;
, eqStringImpl = myEq;
, eqArrayImpl = myEq;
}
