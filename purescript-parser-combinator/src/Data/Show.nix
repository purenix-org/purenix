
{ showIntImpl = builtins.toString;

# TODO: This should remove extra zeros from the end of a number
  showNumberImpl = builtins.toString;

# TODO: This should probably put single quotes around a character.
  showCharImpl = builtins.toString;

# TODO: This should probably add double quotes around a String.
  showStringImpl = builtins.toString;

  cons = a: arr: [ a ] ++ arr;

  join = builtins.concatStringsSep;
}
