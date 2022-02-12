{
  inherit (builtins) throw;
  run = tests:
    let
      results = builtins.foldl' (acc: case: case acc) { oks = [ ]; ngs = [ ]; } tests;
      str = builtins.concatStringsSep "\n" ([ "Some tests failed.." "-- SUCCESSES: --" ] ++ results.oks ++ [ "-- FAILURES: --" ] ++ results.ngs);
    in
    if builtins.length results.ngs == 0 then "All tests passed!" else builtins.throw str;
  failure = str: { oks, ngs }: { inherit oks; ngs = ngs ++ [ str ]; };
  success = str: { oks, ngs }: { inherit ngs; oks = oks ++ [ str ]; };
}
