% kpxhs(1) Version 1.8 | kpxhs manual

# NAME

**kpxhs** - Interactive Keepass database TUI viewer

# SYNOPSIS

**kpxhs** [\-h | \--help] [\-v | \--version] [\--write-config]

# DESCRIPTION

Interactive [Keepass](https://keepass.info/) database TUI viewer. The database and keyfile path can be configured to auto-fill on launch. Entry details can be viewed and their username and password fields can be copied to clipboard. On exit, if the clipboard is "dirty" then it will offer to clear it. The clipboard can also be cleared after a configurable timeout, along with a progress bar that counts down to clear.

# OPTIONS

`h, help`

: Prints the help message and exit. Ignores number of dashes.

`v, version`

: Prints the version number and exit. Ignores number of dashes.

`write-config`

: Writes the default config.hs and theme.hs files to ~/.config/kpxhs/. Ignores number of dashes.

# KEYBINDINGS

`q`
: Go up a directory, or attempt to quit if in root directory.

`Esc`
: In entry details view, go back to browser

`Tab, Shift-Tab`
: Cycle focus between elements (eg: list and search bar; login fields; exit dialog)

`Enter`
: Show entry details of selected entry. Attempts login if locked

`u`
: Copy username of selected/current entry

`p`
: Copy password of selected/current entry

`[n]j, [n]s`
: Move down the list by \`n\` items, default is 1. \`n\` is an optional series of digits (0-9), that terminates when \`j\` or \`s\` is pressed. This resembles the vim motion commands. Leading zeros are stripped. For example, \`4j\` will move four items down and \`10j\` will move ten items down. Use the relative line numbers to help; the number shown is the exact number needed for the digit \`n\`

`[n]k, [n]w`
: Move up the list by \`n\` items, default is 1. \`n\` is an optional series of digits (0-9), that terminates when \`k\` or \`w\` is pressed. Leading zeros are stripped. This resembles the vim motion commands. For example, \`4k\` will move four items down and \`10k\` will move ten items down. Use the relative line numbers to help; the number shown is the exact number needed for the digit \`n\`

`g`
: Move to the top of the list

`G`
: Move to the bottom of the list

The following table shows a summary of the keybindings and their effects in each mode

|Key  | Browser           | Search       | Entry details| Login      | Exit dialog
|-----|-------------------|--------------|--------------|------------|------------
|q    | Go up dir or quit | -            | -            | -          | -
|Esc  | Clear command     | Quit         | Back         | Quit       | -
|Tab  | Focus Search      | Focus Browser| -            | Cycle Focus| Cycle Focus
|Enter| Show details      | -            | -            | Unlock     | -
|j    | Move down         | -            | -            | -          | -
|k    | Move up           | -            | -            | -          | -
|u    | Copy username     | -            | -            | -          | -
|p    | Copy password     | -            | -            | -          | -
|g    | Go to top         | -            | -            | -          | -
|G    | Go to bottom      | -            | -            | -          | -


# EXAMPLE USAGE

1. \`kpxhs\`
2. \`YOUR_PASSWORD<Enter>\` (Assuming database path stored in config and no keyfile)
3. (Focus is on search bar by default) \`git\` (List filtered to items with "git" in title)
4. \`Tab\` (Focus to list)
5. \`j\` (Focus one entry below)
6. \`p\` (Copy password)
7. \`Esc\` (quit)
8. (Focus is on clear clipboard and exit by default) \`Enter\` (clear clipboard and exit)

# CONFIGURATION

## INTRODUCTION

You can set the database and keyfile fields to be auto-filled with any path, so you only need to enter your password on launch. You can also customize the automatic clipboard clearing: change the number of seconds to wait before clearing the clipboard; or disable the feature altogether.

Hint: run \`kpxhs --write-config\` to generate the default config and theme for further editing

## SETTINGS

The config file is located in \`~/.config/kpxhs/config.hs\`. Make sure it is encoded as UTF-8. Write something like:

```hs
Config { timeout = Just (Seconds 10)
       , dbPath = Just "/home/me/kpxhs/test/kpxhs_test.kdbx"
       , keyfilePath = Just "/home/me/kpxhs/test/keyfile.key"
       }
```

**It must be a valid Haskell expression** of a record with three fields: timeout, dbPath, and keyfilePath. All three are Maybe types - they are optional and you can always omit specifying them by writing \`Nothing\`. Do not delete a field however, as it will result in an invalid config.

The paths can be any UTF-8 string; no validation is performed on them.

### timeout

After copying a username or password, *kpxhs* can automatically clear the clipboard after a set number of seconds. There are three valid values for the timeout field:

`Just (Seconds t)`
: Set the number of seconds to wait for \`t\` seconds after the copy for clearing the clipboard. The number of seconds must be an integer.

`Just DoNotClear`
: Disable automatic clipboard clearing.

`Nothing`
: Fall back to the default, which is **10 seconds**

### dbPath

*kpxhs* can auto-fill the database path with a given string. There are two valid values:

`Just "xxx"`
: Fill in the database path with the string "xxx", without quotes.

`Nothing`
: Fall back to the default, which is the empty string ""

### keyfilePath

*kpxhs* can auto-fill the keyfile path with a given string. There are two valid values:

`Just "xxx"`
: Fill in the keyfile path with the string "xxx", without quotes.

`Nothing`
: Fall back to the default, which is the empty string ""


## THEMING

The theme file is located in \`~/.config/kpxhs/theme.hs\`. Make sure it is encoded as UTF-8. You should probably edit the default theme instead of writing from scratch, because if you write from scratch, all the colors in the default theme are lost.

This is the default theme if you don't provide any:

```hs
