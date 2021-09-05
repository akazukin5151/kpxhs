% kpxhs(1) Version 1.7 | kpxhs manual

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
[ (["edit"],                          Val { fg = Black,  bg = White,  styles = [] })
, (["edit","focused"],                Val { fg = White,  bg = Blue,   styles = [] })
, (["dialog"],                        Val { fg = White,  bg = Blue,   styles = [] })
, (["button"],                        Val { fg = Black,  bg = White,  styles = [] })
, (["button","selected"],             Val { fg = Def,    bg = Yellow, styles = [] })
, (["progressComplete"],              Val { fg = White,  bg = Blue,   styles = [] })
, (["kpxhs","key"],                   Val { fg = Def,    bg = White,  styles = [] })
, (["kpxhs","label"],                 Val { fg = Black,  bg = Def,    styles = [] })
, (["kpxhs","line_number"],           Val { fg = Yellow, bg = Def,    styles = [] })
, (["kpxhs","line_number","focused"], Val { fg = Red,    bg = Def,    styles = [Bold]})
, (["kpxhs","list_border"],           Val { fg = Black,  bg = Def,    styles = [] })
, (["kpxhs","list_border","focused"], Val { fg = Blue,   bg = Def,    styles = [] })
, (["kpxhs","directory"],             Val { fg = Black,  bg = Def,    styles = [Bold]})
, (["kpxhs","directory","focused"],   Val { fg = Red,    bg = Def,    styles = [Bold]})
, (["kpxhs","go_up"],                 Val { fg = Green
                                          , bg = Def
                                          , styles = [Bold, Italic]
                                          })
, (["kpxhs","go_up","focused"],       Val { fg = Blue
                                          , bg = Def
                                          , styles = [Bold, Italic]
                                          })
, (["kpxhs","entry"],                 Val { fg = Black,  bg = Def,    styles = [] })
, (["kpxhs","entry","focused"],       Val { fg = Red,    bg = Def,    styles = [] })
]
```

**The theme file must be a valid Haskell expression**. It is a list-of-2-tuples; for every tuple, the first item is a list-of-strings representing the attribute name, and the second item is the attribute value. The attribute value is represented as a record with three fields: fg, bg, and styles. The fg and bg fields only accept certain color names. styles is a list-of-styles, and also only accept certain style names.

### Attribute names

`["xxx", "yyy"]`
: Represents an attribute name "xxx.yyy". Must have at least one item.

There are a few special attribute names exclusive to *kpxhs*. They are appropriately namespaced with \`"kpxhs"\`.

`["kpxhs", "key"]`
: The key being bound (eg, "Esc")

`["kpxhs", "label"]`
: The label bound (eg, "exit")

In other words, the footer shows a nano-like grid of keys and their action. For example, "Esc exit" to indicate that pressing the Esc key will exit. \`kpxhs.key\` would style the "Esc" text and \`kpxhs.label\` would style the "exit" text

`["kpxhs", "line_number"]`
: The relative line numbers on the left side of the list

`["kpxhs", "list_border"]`
: The list/browser border when it is not focused (ie, focus is on search bar). Only foreground color is used.

`["kpxhs", "list_border", "focused"]`
: The list/browser border when it is focused (ie, focus is on list). Only foreground color is used.

`["kpxhs", "directory"]`
: A directory that is not currently selected

`["kpxhs", "directory", "focused"]`
: A directory that is currently selected

`["kpxhs", "directory"]`
: An entry that is not currently selected

`["kpxhs", "directory", "focused"]`
: An entry that is currently selected

Apart from those, you can use any other attribute name of elements used in the program. Here are the Brick docs for the attribute names of the elements used in *kpxhs*:

- [List widget](https://hackage.haskell.org/package/brick-0.64/docs/Brick-Widgets-List.html#g:7)
- [Exit dialog](https://hackage.haskell.org/package/brick-0.64/docs/Brick-Widgets-Dialog.html#g:4)
- [Login dialog](https://hackage.haskell.org/package/brick-0.64/docs/Brick-Widgets-Edit.html#g:7)
- [Progress bar](https://hackage.haskell.org/package/brick-0.64/docs/Brick-Widgets-ProgressBar.html#g:1)
- [Borders](https://hackage.haskell.org/package/brick-0.64/docs/Brick-Widgets-Border.html#g:5)

### Attribute values

The record has three fields:

`fg`
: Set the foreground color. See **Colors**

`bg`
: Set the background color. See **Colors**

`styles`
: Set the given styles. See **Styles**

### Colors


`Black, Red, Green, Yellow, Blue, Magenta, Cyan, White, BrightBlack, BrightRed, BrightGreen, BrightYellow, BrightBlue, BrightMagenta, BrightCyan, BrightWhite`
: Set the color to one of those 16 colors. Their exact values are configured through your terminal

`Def`
: Use the default color for that element. Essentially means a color is not specified

`RGB r g b`
: Use an RGB color given by the three integers, from 0 to 255 inclusive. Note that Brick doesn't support the entire rgb palette, so some colors can throw an error. *kpxhs* allows it to be thrown, because some attributes might be a hassle to navigate to, so aborting the program will let the user know their color is invalid as early as possible.

### Styles

`Standout, Underline, ReverseVideo, Blink, Dim, Bold, Italic, Strikethrough`
: Formats the text with the given style

If you don't want to specify a style, leave the list empty.

### Theme examples

0. Set the text of \`kpxhs.key\` to bold
```hs
, (["kpxhs","key"],       Val { fg = Def,   bg = Def,  styles = [Bold] } )
```

1. Set the background color of \`kpxhs.key\` to red
```hs
, (["kpxhs","key"],       Val { fg = Def,   bg = Red,  styles = [] } )
```

2. Set the background color of \`kpxhs.key\` to red and make it bold

```hs
, (["kpxhs","key"],       Val { fg = Def,   bg = Red,  styles = [Bold] } )
```

3. Set the background color of \`kpxhs.key\` to red and make it bold-italic

```hs
, (["kpxhs","key"],       Val { fg = Def,   bg = Red,  styles = [Bold, Italic] } )
```

4. Set the background color of \`kpxhs.key\` to red, the foreground color to RGB(51, 187, 204) and make it bold-italic

```hs
, (["kpxhs","key"],       Val { fg = RGB 51 187 204,   bg = Red,  styles = [Bold, Italic] } )
```

## CONFIGURATION NOTES

The only contents of the config and theme files is the single expression; assignments, imports, statements, and comments are not allowed. You cannot use \`$\` to replace parenthesis, because no arbitrary functions are evaluated. Whitespace and newline rules follow normal Haskell rules for expressions. The config and theme files are not valid Haskell modules that can be compiled; they are interpreted at launch.

Any records must match their specified number of fields; omission or addition of any fields will result in an invalid config, and the default will be used instead.

Type constructors must be written verbatim with no changes in capitalization. They include: \`Just\`, \`Nothing\`, \`Seconds\`, \`DoNotClear\`, \`Val\`, all the color names (eg, \`Red\`), and all the style names (eg, \`Bold\`)


# ENVIRONMENT

Requires [keepassxc](https://github.com/keepassxreboot/keepassxc/) installed with \`keepassxc-cli\` in PATH.

# FILES

`Configuration`

: \`~/.config/kpxhs/config.hs\`


`Theme`

: \`~/.config/kpxhs/theme.hs\`

# BUGS

The issue tracker and repo is in: <https://github.com/twenty5151/kpxhs>

# LICENSE

GPLv3 or later

# SEE ALSO

keepassxc-cli(1)
