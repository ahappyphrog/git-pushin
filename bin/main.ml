let cli_requested =
  Array.to_list Sys.argv |> List.exists (fun arg -> arg = "--cli")

let gui_requested =
  Array.to_list Sys.argv |> List.exists (fun arg -> arg = "--gui")

let run_gui_with_fallback () =
  try Git_pushin.Gui.run ()
  with Graphics.Graphic_failure msg ->
    prerr_endline ("GUI unavailable, falling back to CLI: " ^ msg);
    Git_pushin.Cli.run ()

let () =
  if cli_requested then Git_pushin.Cli.run ()
  else if gui_requested then Git_pushin.Gui.run ()
  else run_gui_with_fallback ()
