{-# LANGUAGE OverloadedStrings #-}

module Constants where

version :: Double
version = 1.7

help :: String
help = "kpxhs - Interactive Keepass database TUI viewer\n\
        \  Usage\n\
        \    kpxhs                   Start the program\n\
        \    kpxhs [-v | --version]  Print the version number\n\
        \    kpxhs [-h | --help]     Show this help\n\
        \    kpxhs --write-config    Write the default configs to ~/.config/kpxhs/\n\n\
        \  TUI keybindings (in general)\n\
        \    Esc                     Quit, back (elsewhere)\n\
        \    q                       Quit, back (in browser)\n\
        \    Tab                     Cycle focus\n\
        \    Enter                   Show entry details\n\
        \    u                       Copy username\n\
        \    p                       Copy password\n\n\
        \  Navigation ([n] means optional digit)\n\
        \    [n]j, [n]s              Move down n items (default: 1)\n\
        \    [n]k, [n]w              Move up n items   (default: 1)\n\
        \    g                       Move to top\n\
        \    G                       Move to bottom\n\
        \    q                       Page up\n\
        \    e                       Page down"
