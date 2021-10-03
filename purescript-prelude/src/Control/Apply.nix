
{ arrayApply = fs: xs: builtins.concatMap (f: map f xs) fs; }
