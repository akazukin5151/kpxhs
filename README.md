# kpxhs

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
- A keepass client with `keepassxc-cli` in PATH
    - I recommend [keepassxc](https://github.com/keepassxreboot/keepassxc/)

# Installing

## Install using pre-compiled binary

Just go to the [releases](https://github.com/twenty5151/kpxhs/releases/) page and grab the latest binary for your OS. Only UNIX (linux and macos) is supported. Binaries are compiled and uploaded using Github actions

## Build from source using [Stack](https://docs.haskellstack.org/en/stable/README/)

1. `git clone https://github.com/twenty5151/kpxhs`
2. `cd kpxhs`
3. `stack build` (compile)
4. `stack install` (move binary to `~/.local/bin/`)

# Configure (optional)

- You can fill your database path in using a config file, so you only need to enter your password upon running. This is how keepassxc works by default. The paths can be any string
    - Default: `Just ""`
    - `Just "xxx"` will fill in the database path with the string "xxx"
    - `Nothing` will fall back to the default
- You can also have `kpxhs` auto-fill a path to the keyfile
    - Default: `Just ""`
    - `Just "xxx"` will fill in the keyfile path with the string "xxx"
    - `Nothing` will fall back to the default
- You can also change the clipboard clear timeout (the number of seconds before the clipboard is cleared)
    - Default: `Just (Seconds 10)`
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
- Constructs the following record (don't worry about String vs Text)

```hs
data Timeout = Seconds Int | DoNotClear

data Config = Config { timeout     :: Maybe Timeout
                     , dbPath      :: Maybe Text
                     , keyfilePath :: Maybe Text
                     }
```

- See [Theming.md](Theming.md) for rationale and details on "Haskell expression"
- Why not use Dhall or Aeson? Aeson doubled the size of the binary while Dhall tripled (!) it

## Theming

See [Theming.md](Theming.md)

# Usage

```sh
$ kpxhs -h
kpxhs - Interactive Keepass database TUI viewer
  Usage
    kpxhs                  Start the program
    kpxhs [-h | --help]    Show this help

  TUI keybindings (in general)
    Esc                    Quit
    Tab                    Cycle focus
    Enter                  Show entry details
    u                      Copy username
    p                      Copy password
  Navigation
    j, s                   Move down
    k, w                   Move up
    g                      Move to top
    G                      Move to bottom
    q                      Page up
    e                      Page down
```

## Keybindings

- Initial screen:
    - `Esc`: quit
    - `Tab`: switch focus between database path, password, and keyfile path fields
    - `Enter`: attempt unlock
- Browser view:
    - `Esc`: quit
    - If search bar is focused:
        - Just type and the list will be filtered in real-time
        - `Tab`: focus the list
    - If an entry in the list is focused:
        - `u`: copy username
        - `p`: copy the password
        - `Enter`: view entry details
        - `Tab` to focus search
        - Use `j`, `k`, `w`, and `s` to navigate
- Inside entry details:
    - `u`: copy username
    - `p`: copy the password
    - `Esc`: return to browser view

## Example usage

1. `kpxhs`
2. `YOUR_PASSWORD<Enter>` (Assuming database path stored in config and no keyfile)
3. (Focus is on search bar by default) `git` (List filtered to items with "git" in title)
4. `Tab` (Focus to list)
5. `j` (Focus one entry below)
6. `p` (Copy password)
7. `Esc` (quit)
8. (Focus is on clear clipboard and exit by default) `Enter` (clear clipboard and exit)

# License

GPLv3 or later
