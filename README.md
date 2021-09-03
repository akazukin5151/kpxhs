# kpxhs

[![commits since](https://img.shields.io/github/commits-since/twenty5151/kpxhs/latest)](https://GitHub.com/twenty5151/kpxhs/commit/)

[Keepass](https://keepass.info/) database interactive TUI viewer based on `keepassxc-cli`

![unlock](pics/unlock.png)

![browser1](pics/browser1.png)

![countdown](pics/countdown.png)

![footer](pics/responsive_footer.png)

![entry](pics/entry.png)

![searching](pics/searching.png)

![browser2](pics/browser2.png)

![clear_clip](pics/clear_clip.png)

# Features
- Configurable database and keyfile path
- Browse entries and navigate directories
- View entry details
- Copy entry username and password fields to clipboard
- Clear clipboard on exit
- Clear clipboard after a configurable timeout
- Responsive messages in the footer

# Why
- Keyboard friendly way of accessing your passwords
- Fast(er)
    - No need to type your password for every command, unlike `keepassxc-cli`
        - Password is cached for the entire session (make sure to close it after you're done)
    - Entries details are cached
- Browser plugin doesn't work consistently for me
- View only (for now?) because I access passwords much more often than I add or edit them. In rare cases when I have to, using the mouse and GUI isn't such a hassle.
- Learn Haskell
- I use arch btw

\* If you want non-interactive (for scripting etc), just use keepassxc-cli directly


# Usage requirements
You need to install [keepassxc](https://github.com/keepassxreboot/keepassxc/) with `keepassxc-cli` and have it available in PATH. The few most recent versions should work, but if there are incompatibilities in future versions it will be noted here.

# Installing

## Install using pre-compiled binary

Just go to the [releases](https://github.com/twenty5151/kpxhs/releases/) page and grab the latest binary for your OS. Only UNIX (linux and macos) is supported. Binaries are compiled and uploaded using Github actions

## Build from source using [Stack](https://docs.haskellstack.org/en/stable/README/)

No superuser permissions are needed (unless if you need to install stack itself)

1. `git clone https://github.com/twenty5151/kpxhs`
2. `cd kpxhs`
3. `stack build` (compile)
4. `stack install` (move binary to `~/.local/bin/`)

## Manual

The manual is not installed automatically. You can do it manually using `cp docs/kpxhs.1 ~/.local/share/man/man1/kpxhs.1`. Alternatively, view the rendered markdown online in https://github.com/twenty5151/kpxhs/blob/master/docs/kpxhs.1.md

# Configure (optional)

- Hint: run `kpxhs --write-config` to generate the default config for further editing
- You can fill your database path in using a config file, so you only need to enter your password upon running. This is how keepassxc works by default. The paths can be any string
    - Default: `""`
    - `Just "xxx"` will fill in the database path with the string "xxx"
    - `Nothing` will fall back to the default
- You can also have `kpxhs` auto-fill a path to the keyfile
    - Default: `""`
    - `Just "xxx"` will fill in the keyfile path with the string "xxx"
    - `Nothing` will fall back to the default
- You can also change the clipboard clear timeout (the number of seconds before the clipboard is cleared)
    - Default: `Seconds 10`
    - `Just (Seconds t)` will clear the clipboard `t` seconds after copying
    - `Just DoNotClear` will disable automatic clipboard clearing
    - `Nothing` will fall back to the default
- See also [test/example_config.hs](test/example_config.hs)

Write something like this in `~/.config/kpxhs/config.hs`:

```hs
Config { timeout = Just (Seconds 10)
       , dbPath = Just "/home/me/kpxhs/test/kpxhs_test.kdbx"
       , keyfilePath = Just "/home/me/kpxhs/test/keyfile.key"
       }
```

- Must be a valid Haskell expression
- Construct the `Config` record (don't worry about String vs Text)

```hs
-- Timeout is isomorphic to Maybe; Used for clarity
data Timeout = Seconds Int | DoNotClear

data Config = Config { timeout     :: Maybe Timeout
                     , dbPath      :: Maybe Text
                     , keyfilePath :: Maybe Text
                     }
```

- See [Theming.md](Theming.md) for rationale and details on "Haskell expression"
- To be clear, while it has to be a valid Haskell expression, it is indeed not a valid Haskell file/module. It is not supposed to be compiled, but interpreted at launch.
- Why not use Dhall or Aeson? Aeson doubled the size of the binary while Dhall tripled (!) it

## Theming

See [Theming.md](Theming.md)

# Usage

```sh
$ kpxhs -h
kpxhs - Interactive Keepass database TUI viewer
  Usage
    kpxhs                   Start the program
    kpxhs [-v | --version]  Print the version number
    kpxhs [-h | --help]     Show this help
    kpxhs --write-config    Write the default configs to ~/.config/kpxhs/

  TUI keybindings (in general)
    Esc                     Quit, back (elsewhere)
    q                       Quit, back (in browser)
    Tab                     Cycle focus
    Enter                   Show entry details
    u                       Copy username
    p                       Copy password

  Navigation ([n] means optional digit)
    [n]j, [n]s              Move down n items (default: 1)
    [n]k, [n]w              Move up n items   (default: 1)
    g                       Move to top
    G                       Move to bottom
    q                       Page up
    e                       Page down
```

See the [man page](./docs/kpxhs.1.md)

# License

GPLv3 or later
