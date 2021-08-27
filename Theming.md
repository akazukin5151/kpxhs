# Theming

## Contents

1. Introduction
2. Reasons for design decisions
    - Security
3. How it works
4. Writing the theme file
    - Attributes
    - Colors

## Introduction

- The theme file is in `~/.config/kpxhs/theme.hs`
- Refer to [test/default_theme.hs](test/default_theme.hs) for reference; it is also the default theme if you don't provide any
- You should probably edit the default theme instead of writing from scratch, because if you write from scratch, all the colors in the default theme are lost.
- You can theme almost every element because the entire attribute map is exposed
- Refer to the Brick docs for detailed information on what exactly you can theme and how to
    - [Brick.AttrMap](https://hackage.haskell.org/package/brick-0.64/docs/Brick-AttrMap.html)
    - [Attributes for the List widget](https://hackage.haskell.org/package/brick-0.64/docs/Brick-Widgets-List.html#g:7)
    - [Attributes for the Exit dialog](https://hackage.haskell.org/package/brick-0.64/docs/Brick-Widgets-Dialog.html#g:4)
    - [Attributes for the Login dialog](https://hackage.haskell.org/package/brick-0.64/docs/Brick-Widgets-Edit.html#g:7)
    - [Attributes for the Progress bar](https://hackage.haskell.org/package/brick-0.64/docs/Brick-Widgets-ProgressBar.html#g:1)
    - [Attributes for borders](https://hackage.haskell.org/package/brick-0.64/docs/Brick-Widgets-Border.html#g:5)
    - [ISO Color codes](https://hackage.haskell.org/package/vty-5.33/docs/Graphics-Vty-Attributes-Color.html)
        - NOTE: the ISOColor numbers are zero-indexed, but the hackage docs show the bullet points starting at 1. Double check [the source code](https://hackage.haskell.org/package/vty-5.33/docs/src/Graphics.Vty.Attributes.Color.html#Color)
    - [ANSI Color codes](https://en.wikipedia.org/wiki/ANSI_escape_code#8-bit) (scroll down a bit)
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

While it may seem insecure to evaluate a raw Haskell file, it cannot contain any functions, and it has to type check as the type `ThemeAux`. That means there's no way to cheat and successfully pass in something that's not `ThemeAux`. For example, writing `unsafePerformIO (writeFile "log" "boom")` does not work because two functions are used here. No matter where `unsafePerformIO` is placed in the list-of-tuples, it can't be evaluated. There's no mechanism in the kpxhs source to evaluate arbitrary functions, only {fore, back}ground colors. And those colors must type check as `Color` to be `read`. *As long as Haskell's read function does not evaluate and execute functions, it is secure*

It is Turing incomplete because `read` is Turing incomplete. This means parsing and evaluation is guaranteed to terminate.

Is it possible for an update to expose a vulnerability? Yes, either by maliciously or accidentally. But as with any FOSS software, you can always review the source code or at least the changes yourself. It doesn't auto-update, and is not in any package repositories where an update can sneak in, so every update has to be installed manually. If you find a security flaw, please do report it.

## How it works

- The attribute names are already `Read`, so the `read` function works out-of-the-box, albeit with a more verbose constructor
- The attributes are trickier as the functions `fg`, `bg`, and `on` are used to convert colors into {fore, back}ground color attributes. They are turned into constructors for the theme file: `Fg`, `Bg`, and `On`
- The colors are also `Read`, but the constructors have to be used (eg, `ISOColor 1`) instead of their convenience functions (eg, `red`)

- PS: The eval function is very cute

```hs
eval :: AttrAux -> Attr
eval (Fg c)   = fg c
eval (Bg c)   = bg c
eval (On f b) = f `on` b
```
    
## Writing the theme file

- **The theme file must be a valid Haskell expression**
- Imagine that the theme file contains the following code:
    - No qualified imports, imagine that all constructor are in scope

```hs
import Brick        (AttrName)
import Graphics.Vty (Color)

type ThemeAux = [(AttrName, AttrAux)]

data AttrAux = Fg Color | Bg Color | On Color Color
  deriving (Show, Read)
```

- **The type of the expression is:** `ThemeAux`
- The only contents of the file is the list-of-tuples (**no assignments**, nothing else)
- You cannot use `$` to replace parenthesis, because no functions are evaluated, the entire file is passed to the Haskell `read` function
- Whitespace and newline rules follow normal Haskell rules for expressions

### Attributes

- `Fg Color`: set the foreground color
- `Bg Color`: set the background color
- `On Color Color`: set both foreground and background colors
    - Resembles the `on` function in Brick, which is written as an infix function
    - Imagine applying the constructor in infix
    - `On color1 color2` ==> ```color1 `On` color2```

There are two special attribute names exclusive to `kpxhs`. They are appropriately namespaced with `"kpxhs"`.

- `AttrName ["kpxhs", "key"]`: The style of the key being bound (eg, "Esc")
- `AttrName ["kpxhs", "label"]`: The style of the label bound (eg, "exit")

In other words, the footer shows a nano-like grid of keys and their action. For example, "Esc exit" to indicate that pressing the Esc key will exit. `kpxhs.key` would style the "Esc" text and `kpxhs.label` would style the "exit" type

### Colors

- Use either the `ISOColor` or `Color240` constructor
- Both of them take a `Word8`, so you can write an integer literal
- `ISOColor` takes integers from 0 to 15 inclusive. The exact colors depend on your terminal configuration, but they are essentially "simplified" colors like "black"
    - [Color codes](https://hackage.haskell.org/package/vty-5.33/docs/Graphics-Vty-Attributes-Color.html)
- `Color240` takes integers from 0 to 255 inclusive.
    - [ANSI Color codes](https://en.wikipedia.org/wiki/ANSI_escape_code#8-bit) (scroll down a bit)
    - It's not the full RGB color palette, but Brick doesn't support that anyway
