
{
  topInt = 9223372036854775807;
  bottomInt = -9223372036854775807;

  # TODO: Actually figure out what these are:
  # exports.topChar = String.fromCharCode(65535);
  # exports.bottomChar = String.fromCharCode(0);
  topChar = "z";
  bottomChar = "A";

  # TODO: This isn't correct.  From playing around with this on the repl, it
  # doesn't look like floats are bounded in Nix.
  # exports.topNumber = Number.POSITIVE_INFINITY;
  # exports.bottomNumber = Number.NEGATIVE_INFINITY;
  topNumber = 9223372036854775807.0;
  buttomNumber = -9223372036854775807.0;
}


