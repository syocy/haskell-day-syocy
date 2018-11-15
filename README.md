# haskell-day-syocy

SpeakerDeck:

- [並列並行言語Haskell](https://speakerdeck.com/syocy/bing-lie-bing-xing-yan-yu-haskell)
- [Dhall: Haskellの新たなキラーアプリ](https://speakerdeck.com/syocy/dhall-haskellfalsexin-tanakiraapuri)

PDFs are uploaded in [Releases](https://github.com/syocy/haskell-day-syocy/releases).

## Build

[stack](https://docs.haskellstack.org/en/stable/README/)
is required.

1. Install stack.
2. `stack setup`

### Build slides

Prerequisites: texlive, [llmk](https://github.com/wtsnjp/llmk), [dhall-to-yaml](https://github.com/dhall-lang/dhall-lang/wiki/Getting-started%3A-Generate-JSON-or-YAML), [x14y24pxHeadUpDaisy font](http://www17.plala.or.jp/xxxxxxx/00ff/)

```shell
./shake.hs twice
```

If you are not Mac user, you should remove option brackets of luatexja-preset in .tex files.

`\usepackage[hiragino-pro]{luatexja-preset}`  
to  
`\usepackage{luatexja-preset}`

### Doctest src/

```shell
stack test
```

### Profile app/Main.hs

Prerequisites: [ThreadScope](https://wiki.haskell.org/ThreadScope)

```shell
stack build --executable-profiling
stack exec haskell-day-syocy-exe --rts-options "-l -N4 -qn2 -A8M" -- 5 20
threadscope haskell-day-syocy-exe.eventlog &
```
