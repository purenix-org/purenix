
{ showIntImpl = builtins.toString;

  # TODO: This should remove extra zeros from the end of a number
  showNumberImpl = builtins.toString;

  # TODO: There are some characters that are shown specially.  See JS implementation.
  showCharImpl = c: "'" + c + "'";

  # TODO: There are some characters that need to be shown specially.  See JS implementation.
  showStringImpl = str: "\"" + str + "\"";

  showArrayImpl = show: arr:
    "[" + builtins.concatStringsSep "," (map show arr) + "]";

  cons = a: arr: [ a ] ++ arr;

  intercalate = builtins.concatStringsSep;
}
