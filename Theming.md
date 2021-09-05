# Theming

This file is now documentation for other developers, in particular it justifies design decisions, evaluates its pros and cons, and discusses security measures.

Read the user guide in the manual here: https://github.com/twenty5151/kpxhs/blob/master/docs/out/kpxhs.1.md

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

## Security

While it may seem insecure to evaluate a raw Haskell file, it cannot contain any functions, and it has to type check as the type `UserFacingTheme`. That means there's no way to cheat and successfully pass in something that's not `UserFacingTheme`. For example, writing `unsafePerformIO (writeFile "log" "boom")` does not work because two functions are used here. No matter where `unsafePerformIO` is placed in the list-of-tuples, it can't be evaluated. There's no mechanism in the kpxhs source to evaluate arbitrary functions, only {fore, back}ground colors and text styles. Colors must type check as `Color`, and text styles type check as `Style`. *As long as Haskell's read function does not evaluate and execute functions, it is secure*

It is Turing incomplete because `read` is Turing incomplete. This means parsing and evaluation is guaranteed to terminate.

Is it possible for an update to expose a vulnerability? Yes, either by maliciously or accidentally. But as with any FOSS software, you can always review the source code or at least the changes yourself. It doesn't auto-update, and is not in any package repositories where an update can sneak in, so every update has to be installed manually. If you find a security flaw, please do report it.

PS: no superuser permissions are required for the *binary installation* as well as usage. Installing stack probably requires it, but not compilation (`stack build` *and* `stack install`).
