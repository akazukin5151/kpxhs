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
: The relative line numbers on the left side of the list, for entries that are not selected/in focus

`["kpxhs", "line_number", "focused"]`
: The relative line numbers on the left side of the list for the currently selected entry

`["kpxhs", "list_border"]`
: The list/browser border when it is not focused (ie, focus is on search bar). Only foreground color is used.

`["kpxhs", "list_border", "focused"]`
: The list/browser border when it is focused (ie, focus is on list). Only foreground color is used.

`["kpxhs", "directory"]`
: A directory that is not currently selected

`["kpxhs", "directory", "focused"]`
: A directory that is currently selected

`["kpxhs", "go_up"]`
: The "\-- (Go up directory) \--" text when it is not focused/selected

`["kpxhs", "go_up", "focused"]`
: The "\-- (Go up directory) \--" text when it is focused/selected

`["kpxhs", "entry"]`
: An entry that is not currently selected

`["kpxhs", "entry", "focused"]`
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
