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

`Tab, Ctrl-Tab`
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

`q`
: Move 5 items up the list

`e`
: Move 5 items down the list

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

## GENERAL

The only contents of the file is the single expression; assignments, imports, statements, and comments are not allowed. You cannot use \`$\` to replace parenthesis, because no arbitrary functions are evaluated. Whitespace and newline rules follow normal Haskell rules for expressions. The config and theme files are not valid Haskell modules that can be compiled; they are interpreted at launch.

The records must match their specified number of fields; omission or addition of any fields will result in an invalid config, and the default will be used instead.

Type constructors must be written verbatim with no changes in capitalization. They include: \`Just\`, \`Nothing\`, \`Seconds\`, \`DoNotClear\`, all the color names (eg, \`Red\`), and all the style names (eg, \`Bold\`)

Hint: run \`kpxhs --write-config\` to generate the default config and theme for further editing

## SETTINGS

You can fill your database path in using a config file, so you only need to enter your password upon running. This is how keepassxc works by default. The paths can be any UTF-8 string; no validation is performed on them.

The config file is located in \`~/.config/kpxhs/config.hs\`. Make sure it is encoded as UTF-8. Write something like:

```hs
Config { timeout = Just (Seconds 10)
       , dbPath = Just "/home/me/kpxhs/test/kpxhs_test.kdbx"
       , keyfilePath = Just "/home/me/kpxhs/test/keyfile.key"
       }
```

**It must be a valid Haskell expression** of a record with three fields: timeout, dbPath, and keyfilePath. All three are Maybe types - they are optional and you can always omit specifying them by writing \`Nothing\`. Do not delete a field however, as it will result in an invalid config.

### timeout

After copying a username or password, *kpxhs* can automatically clear the clipboard after a set number of seconds. There are three valid values for the timeout field:

`Just (Seconds t)`
: Set the number of seconds to wait before clearing the clipboard to \`t\` seconds. The number of seconds must be an integer.

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
[ (Name ["list","selected"],   Val { fg = Red,   bg = Def,    styles = [] } )
, (Name ["edit"],              Val { fg = Black, bg = White,  styles = [] } )
, (Name ["edit","focused"],    Val { fg = White, bg = Blue,   styles = [] } )
, (Name ["dialog"],            Val { fg = White, bg = Blue,   styles = [] } )
, (Name ["button"],            Val { fg = Black, bg = White,  styles = [] } )
, (Name ["button","selected"], Val { fg = Def,   bg = Yellow, styles = [] } )
, (Name ["kpxhs","key"],       Val { fg = Def,   bg = White,  styles = [] } )
, (Name ["kpxhs","label"],     Val { fg = Black, bg = Def,    styles = [] } )
, (Name ["progressComplete"],  Val { fg = White, bg = Blue,   styles = [] } )
]
```

**The theme file must be a valid Haskell expression**. It is a list-of-2-tuples, where the first item is an attribute name made of a list-of-strings, and the second item is a record with three fields: fg, bg, and styles. fg and bg are of type Color, while styles is a list-of-styles

### Attribute names

`Name xs`
: Constructs an attribute name using the list-of-strings xs.

There are also two special attribute names exclusive to *kpxhs*. They are appropriately namespaced with \`"kpxhs"\`.

`Name ["kpxhs", "key"]`
: The style of the key being bound (eg, "Esc")

`Name ["kpxhs", "label"]`
: The style of the label bound (eg, "exit")

In other words, the footer shows a nano-like grid of keys and their action. For example, "Esc exit" to indicate that pressing the Esc key will exit. \`kpxhs.key\` would style the "Esc" text and \`kpxhs.label\` would style the "exit" text

Apart from those two, you can use any other attribute name of elements used in the program. Here are the Brick docs for the attribute names of the elements used in *kpxhs*:

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
: Uses the 16 colors configured through your terminal

`Def`
: Use the default color for that element

`RGB r g b`
: Use an RGB color given by the three integers, from 0 to 255 inclusive. Note that it doesn't support the entire rgb palette, so some colors can throw an error. *kpxhs* allows it to be thrown, because some attributes might be a hassle to navigate to, so aborting the program will let the user know their color is invalid as early as possible.

### Styles

`Standout, Underline, ReverseVideo, Blink, Dim, Bold, Italic, Strikethrough`
: Formats the text with the given style

If you don't want to specify a style, leave the list empty.

### Theme examples

0. Set the text of \`kpxhs.key\` to bold
```hs
, (Name ["kpxhs","key"],       Val { fg = Def,   bg = Def,  styles = [Bold] } )
```

1. Set the background color of \`kpxhs.key\` to red
```hs
, (Name ["kpxhs","key"],       Val { fg = Def,   bg = Red,  styles = [] } )
```

2. Set the background color of \`kpxhs.key\` to red and make it bold

```hs
, (Name ["kpxhs","key"],       Val { fg = Def,   bg = Red,  styles = [Bold] } )
```

3. Set the background color of \`kpxhs.key\` to red and make it bold-italic

```hs
, (Name ["kpxhs","key"],       Val { fg = Def,   bg = Red,  styles = [Bold, Italic] } )
```

4. Set the background color of \`kpxhs.key\` to red, the foreground color to RGB(51, 187, 204) and make it bold-italic

```hs
, (Name ["kpxhs","key"],       Val { fg = RGB 51 187 204,   bg = Red,  styles = [Bold, Italic] } )
```


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
