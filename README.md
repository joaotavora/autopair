**Autopair** is an extension to the Emacs text editor that
 automatically pairs braces and quotes:

  - Opening braces/quotes are autopaired;
  - Closing braces/quotes are autoskipped;
  - Backspacing an opening brace/quote autodeletes its pair.
  - Newline between newly-opened brace pairs open an extra indented line.

Autopair works well across all Emacs major-modes, deduces from the
language's syntax table which characters to pair, skip or delete. It
should work even with extensions that redefine such keys. It also
works with [YASnippet][yasnippet], another package I maintain.

**Important: in Emacs 24.4 you can try `electric-pair-mode` as an alternative to autopair. See [below][2]**

# Installation and basic use:

To try it out, download the
[latest version](https://raw.github.com/capitaomorte/autopair/master/autopair.el)
add to your `.emacs`

```el
(add-to-list 'load-path "/path/to/autopair") ;; comment if autopair.el is in standard load path 
(require 'autopair)
(autopair-global-mode) ;; enable autopair in all buffers
```

# Alternatives to autopair

I developed autopair to work just like
[Textmate](http://macromates.com/) or better, be minimally intrusive
to my existing hacks and need very little customization. You might
prefer it to the following:

- https://github.com/Fuco1/smartparens
- http://www.emacswiki.org/emacs/ParEdit
- the options listed in http://www.emacswiki.org/emacs/AutoPairs

## Differences to smartparens

Note in particular that [smartparens][smartparens] claims in its
README that it provides "the basic funcionality [of autopair]". *I
don't know why* this claim is made, since at time of writing:

1. it does *not* automatically set itself up for any mode according 
   to the mode's syntax table.
2. it does *not* attempt to automatically balance parentheses

Autopair provides these things out-of-the-box. Smartparens provides
other features, that I personally use [Yasnippet][yasnippet] for.

## `electric-pair-mode` in Emacs 24.4

`electric-pair-mode` is a minor mode in Emacs 24.3, but it's not very
useful or widely used. In Emacs 24.4, `electric-pair-mode`
minor-mode is much improved and actually **supersedes autopair** in 
functionality and general all-around Emacs integration, and I recommend 
you use it instead.



# Neat tricks

- `autopair-autowrap` tells autopair to automatically wrap the
  selection region with the delimiters you're trying to insert.

- `autopair-blink` makes the cursor quickly blink over matching
  braces and quotes just inserted or skipped over. If you find this
  behaviour annoying, set this to nil.

- `autopair-skip-whitespace`, when set to non-nil, tells autopair to
  skip over whitespace when attempting to skip over a closing
  brace. If you set this to 'chomp, the whitespace is not only
  jumped over but also deleted.

Autopair's idea is to always do-what-you-mean, but since some people
are never satisfied, have a look at the following:

# Optional tricks

You shouldn't need this, but to enable autopair in just some types of
buffers, comment out the `autopair-global-mode` and turn on
`autopair-mode` in some major-mode hook, like:

```el
;; use autopair just in c buffers
 
(add-hook 'c-mode-common-hook 
          #'(lambda () (autopair-mode)))
```

Alternatively, do use `autopair-global-mode` and create _exceptions_
using the major mode hooks (watch out for the change in behaviour
emacs 24).

```el
;; use autopair everywhere but c buffers
 
(add-hook 'c-mode-common-hook
           #'(lambda () 
               (setq autopair-dont-activate t) ;; for emacsen < 24
               (autopair-mode -1))             ;; for emacsen >= 24
)
```

# More tricks

`autopair-dont-pair` lets you define special cases of characters you
don't want paired.  Its default value skips pairing single-quote
characters when inside a comment literal, even if the language syntax
tables does pair these characters.

As a further example, to also prevent the `{` (opening brace)
character from being autopaired in C++ comments use this in your
.emacs.

```el
(add-hook 'c++-mode-hook
          #'(lambda ()
               (push ?{
                     (getf autopair-dont-pair :comment))))

```

`autopair-handle-action-fns` lets you write some emacs-lisp that
overrides/extends the actions taken by autopair after it decides
something must be paired, skipped or deleted. To work with triple
quoting in python mode, you can use this for example:

```el
(add-hook 'python-mode-hook
          #'(lambda ()
              (setq autopair-handle-action-fns
                    (list #'autopair-default-handle-action
                          #'autopair-python-triple-quote-action))))
```

where `autopair-python-triple-quote-action` is an example of a
user-written function (which is bundled in `autopair.el`).

See
[this issue](http://code.google.com/p/autopair/issues/detail?id=13)
for an example of clever use of this variable (thanks
[Alex Duller](https://github.com/ramblex).

`autopair-extra-pairs` lets you define extra pairing and skipping
behaviour for pairs not programmed into the syntax table. Watch out,
this is work-in-progress, a little unstable and does not help
balancing at all. To have `<= and =>= pair in =c++-mode` buffers, but
only in code, use:

```el
(add-hook 'c++-mode-hook
          #'(lambda ()
              (push '(?< . ?>)
                    (getf autopair-extra-pairs :code))))
```

if you program in emacs-lisp you might also like the following to
pair backtick (``=) and quote (='`).
  
```el
(add-hook 'emacs-lisp-mode-hook
          #'(lambda ()
              (push '(?` . ?')
                    (getf autopair-extra-pairs :comment))
              (push '(?` . ?')
                    (getf autopair-extra-pairs :string))))
```

# Workarounds

Once you set `autopair-global-mode` everything mostly _just works_ but
a few extensions use tricks that interfere with autopair's own tricks,
disabling autopair or some of the extension's functionality. Using the
customization techniques described above, there are plenty of very
good workarounds for `slime-mode`, `latex-mode`, `term-mode` and even
`viper-mode`.

See this [workaround list][1]

# How it works

The extension works by rebinding the braces and quotes keys, but can
still be minimally intrusive, since the original binding is always
called as if autopair did not exist.
 
The decision of which keys to actually rebind is taken at minor-mode
activation time, based on the current major mode's syntax tables. To
achieve this kind of behaviour, an Emacs variable
`emulation-mode-map-alists` was used.
 
If you set `autopair-pair-criteria` and `autopair-skip-criteria` to
the symbol `help-balance` (which, by the way, is the default), braces
are not autopaired/autoskipped in all situations; the decision to
autopair or autoskip a brace is taken according to the following
table:

```
+---------+------------+-----------+-------------------+
| 1234567 | autopair?  | autoskip? | notes             |
+---------+------------+-----------+-------------------+
|  (())   |  yyyyyyy   |  ---yy--  | balanced          |
+---------+------------+-----------+-------------------+
|  (()))  |  ------y   |  ---yyy-  | too many closings |
+---------+------------+-----------+-------------------+
|  ((())  |  yyyyyyy   |  -------  | too many openings |
+---------+------------+-----------+-------------------+
```

The table is read like this: in a buffer with 7 characters laid out
like the first column, an "y" marks points where an opening brace is
autopaired and in which places would a closing brace be
autoskipped. Quote pairing tries to support similar "intelligence".

[1]: https://code.google.com/p/autopair/issues/list?can=1&q=status%3AWorkaround&colspec=ID+Type+Status+Priority+Milestone+Owner+Summary&cells=tiles
[2]: https://github.com/capitaomorte/autopair#electric-pair-mode-in-emacs-244
[smartparens]: https://github.com/Fuco1/smartparens
[yasnippet]: https://github.com/capitaomorte/yasnippet
