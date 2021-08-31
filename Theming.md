# Theming

## Contents

1. [Introduction](#Introduction)
2. [Reasons for design decisions](#Reasons-for-design-decisions)
    - [Security](#Security)
3. [How it works]
4. [Writing the theme file](#Writing-the-theme-file)
    - [Attribute names](#Attribute-names)
    - [Attribute values](#Attribute-values)
    - [Colors](#Colors)
    - [Styles](#Styles)
5. [Examples](#Examples)

## Introduction

- The theme file is located in `~/.config/kpxhs/theme.hs`
- Refer to [test/default_theme.hs](test/default_theme.hs) for reference; it is also the default theme if you don't provide any
- You should probably edit the default theme instead of writing from scratch, because if you write from scratch, all the colors in the default theme are lost.
- You can theme almost every element because the entire attribute map is exposed
- Jump straight to [Writing the theme file](#Writing-the-theme-file) and [Examples](#Examples) to start writing your config; everything else is for other developers

## Reasons for design decisions

- The colors/theme is usually specified (hardcoded) like:

```hs
A.attrMap V.defAttr [ (attrName1, attr1), (attrName2, attr2) ]
```
[See also: Brick docs on](https://hackage.haskell.org/package/brick-0.64/docs/Brick-AttrMap.html) `AttrMap`

- The idea is to move that list-of-tuples into a file to be read and evaluated at launch
- The goals are **flexibility of theming, minimal internal processing, and to avoid extra dependencies or use of another configuration language**
- If the theme file is a valid Haskell expression, it can just be `read`, avoiding building a DSL, writing a parser, or using Aeson/Dhall which doubles/triples the binary size respectively. The expression can be passed into `A.attrMap V.defAttr`, maintaining full flexibility.

- [Brick.Themes](https://hackage.haskell.org/package/brick-0.64/docs/Brick-Themes.html) are not used here because it's not flexible enough. For example, a default must be given to the function `newTheme`

### Security

While it may seem insecure to evaluate a raw Haskell file, it cannot contain any functions, and it has to type check as the type `UserFacingTheme`. That means there's no way to cheat and successfully pass in something that's not `UserFacingTheme`. For example, writing `unsafePerformIO (writeFile "log" "boom")` does not work because two functions are used here. No matter where `unsafePerformIO` is placed in the list-of-tuples, it can't be evaluated. There's no mechanism in the kpxhs source to evaluate arbitrary functions, only {fore, back}ground colors and text styles. Colors must type check as `Color`, and text styles type check as `Style`. *As long as Haskell's read function does not evaluate and execute functions, it is secure*

It is Turing incomplete because `read` is Turing incomplete. This means parsing and evaluation is guaranteed to terminate.

Is it possible for an update to expose a vulnerability? Yes, either by maliciously or accidentally. But as with any FOSS software, you can always review the source code or at least the changes yourself. It doesn't auto-update, and is not in any package repositories where an update can sneak in, so every update has to be installed manually. If you find a security flaw, please do report it.

## Writing the theme file

- **The theme file must be a valid Haskell expression**
- Imagine that the theme file contains the following code:
    - No qualified imports, imagine that all constructor are in scope

```hs
import Data.Word (Word8)

newtype Name = Name [String]
    deriving (Show, Read)

type UserFacingTheme = [(Name, Val)]

data Val =
  Val { fg     :: Color
      , bg     :: Color
      , styles :: [Style]
      }
      deriving (Show, Read)

-- List of valid colors
data Color = Black
           | Red
           | Green
           | Yellow
           | Blue
           | Magenta
           | Cyan
           | White
           | BrightBlack
           | BrightRed
           | BrightGreen
           | BrightYellow
           | BrightBlue
           | BrightMagenta
           | BrightCyan
           | BrightWhite
           | RGB Word8 Word8 Word8
           | Def
           deriving (Show, Read)

-- List of valid styles
data Style = Standout
           | Underline
           | ReverseVideo
           | Blink
           | Dim
           | Bold
           | Italic
           | Strikethrough
           deriving (Show, Read)
```

- **The type of the expression is:** `UserFacingTheme`
- The only contents of the file is the list-of-tuples (**no assignments**, nothing else)
- You cannot use `$` to replace parenthesis, because no functions are evaluated, the entire file is passed to the Haskell `read` function
- Whitespace and newline rules follow normal Haskell rules for expressions
- To be clear, the config and theme files are not valid Haskell modules that can be compiled.

### Attribute names

There are two special attribute names exclusive to `kpxhs`. They are appropriately namespaced with `"kpxhs"`.

- `Name ["kpxhs", "key"]`: The style of the key being bound (eg, "Esc")
- `Name ["kpxhs", "label"]`: The style of the label bound (eg, "exit")

In other words, the footer shows a nano-like grid of keys and their action. For example, "Esc exit" to indicate that pressing the Esc key will exit. `kpxhs.key` would style the "Esc" text and `kpxhs.label` would style the "exit" text

Apart from those two, you can use any other attribute name of elements used in the program.
    - [Attributes for the List widget](https://hackage.haskell.org/package/brick-0.64/docs/Brick-Widgets-List.html#g:7)
    - [Attributes for the Exit dialog](https://hackage.haskell.org/package/brick-0.64/docs/Brick-Widgets-Dialog.html#g:4)
    - [Attributes for the Login dialog](https://hackage.haskell.org/package/brick-0.64/docs/Brick-Widgets-Edit.html#g:7)
    - [Attributes for the Progress bar](https://hackage.haskell.org/package/brick-0.64/docs/Brick-Widgets-ProgressBar.html#g:1)
    - [Attributes for borders](https://hackage.haskell.org/package/brick-0.64/docs/Brick-Widgets-Border.html#g:5)

[See also: Brick docs on](https://hackage.haskell.org/package/brick-0.64/docs/Brick-AttrMap.html#t:AttrName) `AttrName`


### Attribute values

- All you have to do is to construct the `Val` record
- `fg`: set the foreground color
- `bg`: set the background color
- `styles`: set the following styles (See [Styles](#Styles))

### Colors

- Use either the color name constructors or `RGB` constructor
- The color names such as `Red`, `Blue`, etc, take no extra arguments
- The `RGB` constructor takes three `Word8`s, each from 0 to 255 inclusive.
    - Internally, `kpxhs` converts the rgb values into `Color240`
    - The `RGB` constructor is exposed instead `Color240` because `Color240` is extremely weird
    - Note that it doesn't support the entire rgb palette, so some colors can throw an error. `kpxhs` allows it to be thrown, because some attributes might be a hassle to navigate to, so aborting the program will let the user know their color is invalid as early as possible.
    - Use integer literals for `Word8`
- If you don't want to specify a color, use the `Def` constructor to leave it as default


### Styles

- Text styles includes bold, italic, underline, reverse, etc
- All style constructor takes no arguments
- If you don't want to specify a style, leave the list empty

## Examples

0. Set the text of `kpxhs.key` to bold
```hs
, (Name ["kpxhs","key"],       Val { fg = Def,   bg = Def,  styles = [Bold] } )
```

1. Set the background color of `kpxhs.key` to red
```hs
, (Name ["kpxhs","key"],       Val { fg = Def,   bg = Red,  styles = [] } )
```

2. Set the background color of `kpxhs.key` to red and make it bold

```hs
, (Name ["kpxhs","key"],       Val { fg = Def,   bg = Red,  styles = [Bold] } )
```

3. Set the background color of `kpxhs.key` to red and make it bold-italic

```hs
, (Name ["kpxhs","key"],       Val { fg = Def,   bg = Red,  styles = [Bold, Italic] } )
```

4. Set the background color of `kpxhs.key` to red, the foreground color to RGB(51, 187, 204) and make it bold-italic

```hs
, (Name ["kpxhs","key"],       Val { fg = RGB 51 187 204,   bg = Red,  styles = [Bold, Italic] } )
```
