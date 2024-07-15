# elm-sourcemaps

A fork of [elm/compiler](https://github.com/elm/compiler) that adds support for sourcemaps (from the [gren/compiler PR #205](https://github.com/gren-lang/compiler/pull/205)).

[![Screencast](https://github.com/Janiczek/elm-sourcemaps/raw/main/screenshot.png)](https://github.com/Janiczek/elm-sourcemaps/raw/main/screencast.mp4)

## Build

```
stack build
stack install
```

## Usage

```
elm-sourcemaps make --sourcemaps --output=elm.js src/Main.elm
```

This will compile Elm just like the official binary would, but it will also add the sourcemap to the JS file.
