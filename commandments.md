# More Commandments

## The Eleventh Commandment

Use additional arguments when a function needs to know what other arguments to the function have been like so far.


## The Twelfth Commandment

Use `(letrec ...)` to remove arguments that do not change for recursive applications.


## The Thirteenth Commandment

Use `(letrec ...)` to hide and to protect functions.


## The Fourteenth Commandment

Use `(letcc ...)` to return values abruptly and promptly.


## The Fifteenth Commandment v.1

Use `(let ...)` to name the values of repeated expressions.


## TODO: Finish ch 14


## The Sixteenth Commandment

Use `(set! ...)` only with names define in `(let ...)`s.


## The Seventeenth Commandment v.1

Use `(set! ...)` for `(let ((x ...)) ...)` only if there is at least one `(lambda ...)` between it and the `(let ((x ...)) ...)`.


## The Eighteenth Commandment

Use `(set! x ...)` only when the value that `x` refers to is no longer needed.
