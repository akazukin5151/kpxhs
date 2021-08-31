{-# LANGUAGE OverloadedStrings #-}

module Defaults where

help :: String
help = "kpxhs - Interactive Keepass database TUI viewer\n\
        \  Usage\n\
        \    kpxhs                  Start the program\n\
        \    kpxhs [-h | --help]    Show this help\n\
        \    kpxhs --write-config   Write the default configs to ~/.config/kpxhs/\n\n\
        \  TUI keybindings (in general)\n\
        \    Esc                    Quit\n\
        \    Tab                    Cycle focus\n\
        \    Enter                  Show entry details\n\
        \    u                      Copy username\n\
        \    p                      Copy password\n\
        \  Navigation\n\
        \    j, s                   Move down\n\
        \    k, w                   Move up\n\
        \    g                      Move to top\n\
        \    G                      Move to bottom\n\
        \    q                      Page up\n\
        \    e                      Page down"
