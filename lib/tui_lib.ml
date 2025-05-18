let screen_clear = "\033[2J"
let cursor_home = "\033[H"
let cursor_hide = "\033[?25l"
let cursor_show = "\033[?25h"

let setup_terminal () =
  print_string screen_clear;
  print_string cursor_home;
  print_string cursor_hide;
  flush stdout
