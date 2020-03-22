# julia-formatter

Emacs package for formatting Julia code using [JuliaFormatter.jl](https://github.com/domluna/JuliaFormatter.jl)

This package is inspired by [kdheepak/JuliaFormatter.vim](https://github.com/kdheepak/JuliaFormatter.vim).

## Screenshot

![demo](https://user-images.githubusercontent.com/5780297/77244432-0b41b980-6c58-11ea-950e-5891328688ff.gif)

## Features

This package provides two functions:

* `julia-format-region` : formats the code in the region
* `julia-format-buffer` : formats the code on the entire buffer

(No key bindings by default.)

## Install

```shell
# In the directory appended to the load-path
git clone https://github.com/ki-chi/julia-formatter.git
```

## Usage

I recommend that you should use this package with [julia-emacs](https://github.com/JuliaEditorSupport/julia-emacs).

```lisp
;; Example
(require 'julia-mode)
(require 'julia-formatter)
(add-hook 'julia-mode-hook '(lambda() (julia-formatter-server-start)))
```

If you use use-package,

```lisp
(use-package julia-mode)
(use-package julia-formatter
    :hook (julia-mode . (lambda() (julia-formatter-server-start))))
```

## License

MIT
