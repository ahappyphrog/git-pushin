let () =
  try Git_pushin.Gui.run () 
  with _ ->
    print_endline ("GUI unavailable, falling back to CLI");
    Git_pushin.Cli.run ()