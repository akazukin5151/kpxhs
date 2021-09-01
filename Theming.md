# Theming

## Contents

1. [Introduction](#Introduction)
2. [Reasons for design decisions](#Reasons-for-design-decisions)
    - [Security](#Security)
3. [Writing the theme file](#Writing-the-theme-file)
    - [Attribute names](#Attribute-names)
    - [Attribute values](#Attribute-values)
    - [Colors](#Colors)
    - [Styles](#Styles)
4. [Examples](#Examples)

## Introduction

- The theme file is located in `~/.config/kpxhs/theme.hs`
- Make sure it is encoded as UTF-8
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
- There are several advantages in using a Haskell expression as a config and theme file:
    - It can just be `read` natively, no parsing needed, no need to have Haskell installed
    - No need to build a DSL
    - Aeson and Dhall doubled and tripled the binary size respectively
    - Dhall might require the user to install dhall binaries or tooling
    - Anything that `A.attrMap V.defAttr` accepts is valid, maintaining full flexibility.
    - [Brick.Themes](https://hackage.haskell.org/package/brick-0.64/docs/Brick-Themes.html) are not flexible enough. For example, a default must be given to the function `newTheme`. Themes must provide a default attribute or get the exception "serializeCustomColor does not support KeepCurrent". It distinguishes between a customization and a theme but I want everything to be customizable.

- There are however a few disadvantages:
    - The config and theme files are fragile and failure intolerant. Although Haskell syntax is relatively lenient for expressions, any error will cause a fallback to the default, even if just one small part cannot be parsed.
    - If parsing fails, it doesn't let the user know where the error is
    - Requires some knowledge of Haskell to be fully confident in editing.
    - Enums are used extensively anyway (the list of valid colors and styles), so having a dhall-like tooling and type checking is better than nothing.
        - But essentially only applies to Dhall, because JSON and YAML cannot type check your enum variants anyway. How many linux terminal utilities use Dhall for their configs? A vast majority of them relies on textually listing out the valid values anyway.
        - Actually, there is one prominent Haskell program on linux that uses Haskell for configuration - XMonad. And it actually uses a proper Haskell module, so it's not like it's unprecended.

### Security

While it may seem insecure to evaluate a raw Haskell file, it cannot contain any functions, and it has to type check as the type `UserFacingTheme`. That means there's no way to cheat and successfully pass in something that's not `UserFacingTheme`. For example, writing `unsafePerformIO (writeFile "log" "boom")` does not work because two functions are used here. No matter where `unsafePerformIO` is placed in the list-of-tuples, it can't be evaluated. There's no mechanism in the kpxhs source to evaluate arbitrary functions, only {fore, back}ground colors and text styles. Colors must type check as `Color`, and text styles type check as `Style`. *As long as Haskell's read function does not evaluate and execute functions, it is secure*

It is Turing incomplete because `read` is Turing incomplete. This means parsing and evaluation is guaranteed to terminate.

Is it possible for an update to expose a vulnerability? Yes, either by maliciously or accidentally. But as with any FOSS software, you can always review the source code or at least the changes yourself. It doesn't auto-update, and is not in any package repositories where an update can sneak in, so every update has to be installed manually. If you find a security flaw, please do report it.

PS: no superuser permissions are required for the *binary installation* as well as usage. Installing stack probably requires it, but not compilation (`stack build` *and* `stack install`) either.

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
