# Theming

## Contents

1. [Introduction](#Introduction)
2. [Reasons for design decisions](#Reasons-for-design-decisions)
    - [Security](#Security)
3. [How it works](#How-it-works)
4. [Writing the theme file](#Writing-the-theme-file)
    - [Attributes](#Attributes)
    - [Colors](#Colors)
    - [Styles](#Styles)
5. [Examples](#Examples)

## Introduction

- The theme file is located in `~/.config/kpxhs/theme.hs`
- Refer to [test/default_theme.hs](test/default_theme.hs) for reference; it is also the default theme if you don't provide any
- You should probably edit the default theme instead of writing from scratch, because if you write from scratch, all the colors in the default theme are lost.
- You can theme almost every element because the entire attribute map is exposed
- Jump straight to [Writing the theme file](#Writing-the-theme-file) and [Examples](#Examples) to start writing your config; everything else is for other developers
- Refer to the Brick docs for detailed information on what exactly you can theme and how to
    - [Brick.AttrMap](https://hackage.haskell.org/package/brick-0.64/docs/Brick-AttrMap.html)
    - [Attributes for the List widget](https://hackage.haskell.org/package/brick-0.64/docs/Brick-Widgets-List.html#g:7)
    - [Attributes for the Exit dialog](https://hackage.haskell.org/package/brick-0.64/docs/Brick-Widgets-Dialog.html#g:4)
    - [Attributes for the Login dialog](https://hackage.haskell.org/package/brick-0.64/docs/Brick-Widgets-Edit.html#g:7)
    - [Attributes for the Progress bar](https://hackage.haskell.org/package/brick-0.64/docs/Brick-Widgets-ProgressBar.html#g:1)
    - [Attributes for borders](https://hackage.haskell.org/package/brick-0.64/docs/Brick-Widgets-Border.html#g:5)
    - `AttrName` [docs](https://hackage.haskell.org/package/brick-0.64/docs/Brick-AttrMap.html#t:AttrName)

## Reasons for design decisions

- The colors/theme is usually specified (hardcoded) like:

```hs
A.attrMap V.defAttr [ (attrName1, attr1), (attrName2, attr2) ]
```

- The idea is to move that list-of-tuples into a file to be read and evaluated at launch
- The goals are **flexibility of theming, minimal internal processing, and to avoid extra dependencies or use of another configuration language**
- If the theme file is a valid Haskell expression, it can just be `read`, avoiding building a DSL, writing a parser, or using Aeson/Dhall which doubles/triples the binary size respectively. The expression can be passed into `A.attrMap V.defAttr`, maintaining full flexibility.

- [Brick.Themes](https://hackage.haskell.org/package/brick-0.64/docs/Brick-Themes.html) are not used here because it doesn't seem to allow full unlimited customization.

### Security

While it may seem insecure to evaluate a raw Haskell file, it cannot contain any functions, and it has to type check as the type `ThemeAux`. That means there's no way to cheat and successfully pass in something that's not `ThemeAux`. For example, writing `unsafePerformIO (writeFile "log" "boom")` does not work because two functions are used here. No matter where `unsafePerformIO` is placed in the list-of-tuples, it can't be evaluated. There's no mechanism in the kpxhs source to evaluate arbitrary functions, only {fore, back}ground colors and text styles. Those colors must type check as `ColorAux` to be `read`; text styles type check as `StyleAux`. *As long as Haskell's read function does not evaluate and execute functions, it is secure*

It is Turing incomplete because `read` is Turing incomplete. This means parsing and evaluation is guaranteed to terminate.

Is it possible for an update to expose a vulnerability? Yes, either by maliciously or accidentally. But as with any FOSS software, you can always review the source code or at least the changes yourself. The eval functions are very short. It doesn't auto-update, and is not in any package repositories where an update can sneak in, so every update has to be installed manually. If you find a security flaw, please do report it.

## How it works

- The attribute names are already `Read`, so the `read` function works out-of-the-box, albeit with a more verbose constructor
- The attributes are trickier as the functions `fg`, `bg`, and `on` are used to convert colors into {fore, back}ground color attributes. They are turned into constructors for the theme file: `Fg`, `Bg`, and `On`
- The colors are also `Read`, but auxiliary type constructors have to be used (eg, `Red`) instead of their convenience functions (eg, `red`) because of two reasons: functions cannot be evaluated, and `Color240` is weird so RGB conversion is baked in
- In Brick, the styles are applied with the `withStyle` function, which takes an attribute (which is `Read`) and a style. `withStyle` is turned into the constructor `WithStyle`, which derives `Read`. Auxiliary type constructors are used too (eg, `WithStyle (...) Bold`) to make writing the file easier. 

- PS: The eval function is very cute

```hs
eval :: AttrAux -> Attr
eval (Fg c)          = fg (evalColor c)
eval (Bg c)          = bg (evalColor c)
eval (On f b)        = evalColor f `on` evalColor b
eval (WithStyle a s) = withStyle (eval a) (evalStyle s)
```
    
## Writing the theme file

- **The theme file must be a valid Haskell expression**
- Imagine that the theme file contains the following code:
    - No qualified imports, imagine that all constructor are in scope

```hs
import Brick        (AttrName)
import Data.Word    (Word8)
import Graphics.Vty (Color, Style)

type ThemeAux = [(AttrName, AttrAux)]

data AttrAux = Fg ColorAux
             | Bg ColorAux
             | On ColorAux ColorAux
             | WithStyle AttrAux StyleAux
             deriving (Show, Read)

data ColorAux = Black
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
              deriving (Show, Read)

data StyleAux = Standout
              | Underline
              | ReverseVideo
              | Blink
              | Dim
              | Bold
              | Italic
              | Strikethrough
              deriving (Show, Read)
```

- **The type of the expression is:** `ThemeAux`
- The only contents of the file is the list-of-tuples (**no assignments**, nothing else)
- You cannot use `$` to replace parenthesis, because no functions are evaluated, the entire file is passed to the Haskell `read` function
- Whitespace and newline rules follow normal Haskell rules for expressions
- To be clear, the config and theme files are not valid Haskell modules that can be compiled.

### Attributes

- Note that the fg and bg colors will override each other; use `On` to set both
- `Fg Color`: set the foreground color only
- `Bg Color`: set the background color only
- `On Color Color`: set both foreground and background colors
    - Resembles the `on` function in Brick, which is written as an infix function
    - Imagine applying the constructor in infix
    - `On color1 color2` ==> ```color1 `On` color2```
- `WithStyle AttrAux Style`: set the style on given attribute. See [Styles](#Styles)

There are two special attribute names exclusive to `kpxhs`. They are appropriately namespaced with `"kpxhs"`.

- `AttrName ["kpxhs", "key"]`: The style of the key being bound (eg, "Esc")
- `AttrName ["kpxhs", "label"]`: The style of the label bound (eg, "exit")

In other words, the footer shows a nano-like grid of keys and their action. For example, "Esc exit" to indicate that pressing the Esc key will exit. `kpxhs.key` would style the "Esc" text and `kpxhs.label` would style the "exit" text

### Colors

- Use either the color name constructors or `RGB` constructor
- The color names such as `Red`, `Blue`, etc, take no extra arguments
- `RGB` takes three `Word8`, each from 0 to 255 inclusive.
    - Internally, `kpxhs` converts the rgb values into `Color240`
    - The `RGB` constructor is exposed instead `Color240` because `Color240` is extremely weird
    - Note that it doesn't support the entire rgb palette, so some colors can throw an error. `kpxhs` allows it to be thrown, because some attributes might be a hassle to navigate to, so aborting the program will let the user know their color is invalid as early as possible.
    - Use integer literals for `Word8`


### Styles

- Text styles includes bold, italic, underline, reverse, etc
- The `WithStyle` constructor mirrors the `withStyle` function, which takes an attribute and applies a style to it
- The `WithStyle` constructor takes one other constructor; they are the variants of StyleAux
- Can be nested arbitrarily, but of course has to terminate with a non-recursive variant
- See the examples

## Examples

1. Set the background color of `kpxhs.key` to red
```hs
, (AttrName ["kpxhs", "key"],      Bg Red)
```

2. Set the background color of `kpxhs.key` to red and make it bold

```hs
, (AttrName ["kpxhs", "key"],      WithStyle (Bg Red) Bold)
```

3. Set the background color of `kpxhs.key` to red and make it bold-italic

```hs
, (AttrName ["kpxhs", "key"],      WithStyle (WithStyle (Bg Red) Bold) Italic)
```

4. Set the background color of `kpxhs.key` to red, the foreground color to RGB(51, 187, 204) and make it bold-italic

```hs
, (AttrName ["kpxhs", "key"],      WithStyle (WithStyle (On (RGB 51 187 204) Red) Bold) Italic)
```
