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

- You can fill your database path in using a config file, so you only need to enter your password upon running. This is how keepassxc works by default.
    - Default: ""
- You can also have `kpxhs` auto-fill a path to the keyfile
    - Default: ""
- You can also change the clipboard clear timeout (the number of seconds before the clipboard is cleared)
    - Default: 10 (seconds)
    - Any value less than or equal to 0 will disable automatic clipboard clearing
- See also `test/example_config`

1. `mkdir ~/.config/kpxhs`
2. `nvim ~/.config/kpxhs/config`
3. Write something like this:

```
timeout = 10
db_path = /home/me/kpxhs/test/kpxhs_test.kdbx
keyfile_path = /home/me/kpxhs/test/keyfile.key
```

- Order of key-value pairs does not matter
- The keys must be exact and verbatim
- Equal sign is required, but whitespace around the sign is insignificant
- The timeout must be an integer
    - Any value less than or equal to 0 will disable automatic clipboard clearing (but any *invalid* integer will cause the program to fallback to its default, which is 10 seconds)
- The two paths can be any string, but any number of surrounding whitespace is stripped
- Any other (invalid) text is ignored, so they act like comments
- Why not use Dhall or Aeson? Aeson doubled the size of the binary while Dhall tripled (!) it

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
