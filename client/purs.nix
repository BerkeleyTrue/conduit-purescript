{
  name = "client";
  srcs = [ "src" ];
  dependencies = [
    "console"
    "effect"
    "prelude"
    "halogen"
    "halogen-hooks"
    "halogen-helix"
  ];
}
