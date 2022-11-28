## P3: Scheme Interpreter ##

Please read this WHOLE readme, it contains valuable information that
will help you tackle the trickier portions of the tests.

In this project you will build a metacircular interpreter of a
significant subset of Scheme (the core of Racket). Scheme is similar
to Racket; Generally, most code that can run in this language can also
be run directly in the Racket REPL. We strongly recommend that you
internalize this principle as you debug your implementation: you
should start by running tiny pieces of code in Racket (using the
subset of features you have implemented thusfar) and compare their
results (in Racket) with the result of running your interpreter.

Your implementation will primarily flesh out the implementation of a
single function, `interp`, whose inputs are explained below. The
starter code stubs out each of the cases you have to handle, however
you may wish to split out separate "helper functions" that help
accomplish each case. Our reference implementation is roughly 40 lines
of Racket code, all within the `interp` function; the solution need
not require excessive coding, though the solutions can be subtle and
require starting early.

You may change anything in the implementation of `p3.rkt`, as long as
`run` accepts an `expr?` and returns a `value?`.

Predicates at the top of the file outline the expression language you
must implement (expr?), values (value?), addresses (address?),
environments (environment?), and stores (store?). The importance of
each of these will be explained subsequently.

For full credit, you must implement the full language outlined by the
project. We have provided several testcases for you, though you are
encouraged and allowed to share other testcases. Specifically: we have
some tricky hidden testcases that do satisfy substantive aspects of
the semantics and represent important edge cases. Do not assume that
because the public tests pass, your implementation will satisfy hidden
tests--you will likely need to augment the public tests with your own
tests.

At a high level I would recommend the following strategy for tackling
the project: implement a small core part of the language, maybe
ifarith, say, and build out from there. Test larger and larger
examples as you go. For example, you might start by implementing
single-argument lambdas, and lets with a single binding, followed by
generalizing these to handle arbitrary k-ary lambdas and let-binders.

## Language

You will implement the following language:

```
e ::= (lambda (x ...) e)
    | (let ([x e] ...) e)
    | (begin e e ...)
    | (if e e e)
    | (set! x e)
	| (op ...)
    | (e ...)
    | x | n | b

op ::= + | - | * | / | equal? | add1 | sub1 
     | car | cdr | cons | empty? | append
```

This is a fairly sizeable subset of Scheme. It is a Turing-complete
subset because it includes lambdas, which can then act as loops. Look
at the following example:

```
(((lambda (f) (lambda (x) (if (equal? x 0) 1 (* x ((f f) (- x 1))))))
  (lambda (f) (lambda (x) (if (equal? x 0) 1 (* x ((f f) (- x 1)))))))
 20)
```

Which uses recursion (specifically via the U combinator) to calculate
the factorial of 20. You should know most of the forms from class--we
allow variables (`x`), numbers (`n`), and the two boolean constants
`#t` and `#f`.  We include a specific collection of builtin operations
whose implementation we will inherit from Racket. Note that `(op ...)`
is defined as a special case to call out the fact that it must be
matched first--otherwise it is easy to match the more general
case. Note that this does hamper expressivity a bit, the following
will not work in our implementation:

```
(let ([x +])
  (x 1 2))
```

However, this will make the implementation a bit simpler--you should
use the provided function `apply-builtin-op` to handle the hard parts
for you.

The language also has set! You will implement set! in this language
without using set! in the interpreter. `(set! x e)` sets `x`'s value
to be the result of evaluating `e`. Implementing `set!` requires us to
track an explicit heap/store, as we explain soon--please read the
section on stores later down.

The language has lambdas of any fixed arity (but not polyadic
lambdas), and includes `let`-binding of any number of expressions. Be
careful that you don't implement `let*`'s semantics by accident--we
have tests which check for this case.

Last, you may not have seen `(begin e0 ...)`. There is no specific
reason I have not covered it in the course thusfar, however it is not
hugely useful in the context of functional programming. `(begin e0
... en)` executes each of `e0`, ..., to `en` in succession and returns
the evaluation of `en`. However, `e0` and friends are executed for
their effect:

```
(set! x 0) ;; x is 0
(begin (set! x (+ x 1)) (set! x (+ x 1))) ;; x is 2
```

Also, in this language, we have decided argument evaluation will
specifically happen left to right.

## The `(run e)` and `(interp e env store)` functions

You will implement `interp`, called via our stubbed-out `run`:

run    :  (-> expr? value?)
interp :  (-> expr? environment? store? eval-result?)

Your implementation will follow the metacircular interpreter (i.e.,
write a recursive function that matches on the expression) approach
shown and discussed in class. We have stubbed out `(run e)` to
directly call your interpreter function, `(interp e env store)`. This
function takes three arguments:

- e -- The expression being evaluated. You will do a big match on this
  and then handle each possible input expression.

- env -- This is an environment. Generally, an environment is a
  mapping from variables to values. However, to support `set!`, our
  language will allocate all values on a heap. Thus, environments will
  actually map variables to *addresses*, and stores will finally map
  addresses to *values*. Formally, we would say:
    Env = Var ⇀ Addr
  But in practice, we will implement the environment as a hash from
  variables to addresses.
  
- store -- This is the store. The store is a map from addresses to
  values. You can think of addresses like physical addresses in your
  computer's RAM: each address stores a particular value. Except our
  addresses will all be discrete, and data won't be able to overlap,
  like it can in your computer's RAM. You will need to generate *new*
  addresses and update the environment to reference the addresses you
  generate.

The *return* value of `(interp e env store)` is a `eval-result?`. This
is a tagged list of the form `(result ,value ,store)`. I.e., your
`interp` function should produce a list of length three: the symbol
`'result`, the returned value (to which the input expression
evaluates), and the returned store (a possibly-updated store compared
to the input store).

## Closures

One of the main features of this interpreter is the usage of
"closures" to implement higher-order functions. Closures are the
runtime representation of lambdas. As discussed in class, closures
pair a lambda with its associated environment. For example:

```
((lambda (x) (lambda (y) (y x))) 5)
```

When `(lambda (x) (lambda (y) (y x)))` is applied to 5, it results in
the lambda `(lambda (y) (y x))`. If we then try to apply *that* lambda
to something like `add1`, we get: `(add1 x)` where `x` is
unbound. Thus, instead, when we return `(lambda (y) (y x))`, we do not
return *just* a lambda, we return a *closure*. In your interpreter, we
have stubbed this out within the `value?` type. Closures are
represented as tagged lists which begin with the special symbol
`'closure` and then include a lambda expression and an environment. 

As another example, consider the evaluation of the lambda in the
following Scheme expression:

```
((let ([x 5]) (lambda (b) (if b x 12)))
 #t)
```

At runtime, the `lambda` is returned from the `let`, and `x`
apparently goes out of scope before the function is called. However,
in a correct implementation, a closure is created for the lambda, and
the environment containing `x` is stored. Upon application of the
lambda to `#t`, the captured environment is reinstated, and `x` is
looked up to obtain the final result of `5`.

## Mutation via `set!`

Although `set!` is disallowed to be used in our class in Racket code,
we will be implementing it for this interpreter. The special `(set! x
e)` form takes a variable and an expression. It first evaluates `e`
and then assigns `x` to the resulting value. `set!` is sometimes used
with `begin`, as so:

```
(let ([x 5])
  (begin (set! x 12)
         x))
```

This expression creates a variable `x`, and then assigns (mutates) `x`
to `12`. The resulting value of this expression is `12`, and not `5`.

The inclusion of mutation disallows reasoning about code as a
function. For example:

```
(let ([f (lambda (x) (lambda (y) (begin (set! x (+ x 1)) x)))])
  (let ([g (f 5)]
        [h (lambda (x y) y)])
    (h (g 1) (g 1))))
;; 7
```

As a functional programmer, we might be inclined to think this code
could be rewritten as:

```
(let ([f (lambda (x) (lambda (y) (begin (set! x (+ x 1)) x)))])
  (let ([g (f 5)]
        [h (lambda (x y) y)])
    (let ([result (g 1)])
      (h result result))))
;; 6
```

However, this code evaluates to `6` rather than `7`! For this reason,
the implementations of languages that include mutation must be careful
to preserve the exact behavior of side effects. This can make it much
more challenging to reason about and optimize code written in such
languages.

## Stores (implementing `set!`)

To implement `set!`, we will heap-allocate all values in the
machine. Thus, our interpreter contains not just an expression and an
environment, but also a *store*. As mentioned, in our interpreter, the
environment--typically a hash from variables to values--is actually a
hash from variables to *addresses*. Also previously mentioned,
addresses in our store are analogous to physical addresses in RAM. For
example, an example environment and store might look something like:

env = (hash 'x 'x123 'y 'y523 'z 'z713)
store = (hash 'x 52 'y '(closure (lam...) ...) 'z #t 'a ...)

The environment tracks variables; however, in our case we may not
simply look values up in the environment by using a single `hash-ref`:
instead, we must look up values in the store that correspond to the
correct value in the environment. We can do this via two `hash-ref`s
`(hash-ref store (hash-ref env var-name))`.

Your machine will take the input store as a parameter. In executing a
statement, you will potentially need to *update* the store. Because we
do not explicitly allow you to use `set!` directly, we will instead
have you return an *updated* store. For example, to implement `set!`,
you will update the address in the input store (corresponding to `x`,
the variable being set) and update its value to the result of the
evaluation of `e` (the value being set). Many other forms do not
update the store, and will simply propagate it unchanged to the
result.

Note that redirecting variables through the store will require
*allocating* addresses at points when variables are created. For
example, in applying a lambda `((lambda (x) x) 5)`, you should
allocate a new address `a0` for `x` and update the store to set `a0`
to `5` (the result of evaluating `5`) in the store before then
evaluating the body in an environment updated to have `x` point at
`a0`.

## Grading

Each test used for this project (both public and release tests) has
equal weight. Your grade will be calculated as (tests passed / number
of tests).

The late policy for this project is unchanged. See the course syllabus
for more information.

## Important Notes

- Function application must be defined to have a left-to-right
evaluation order. That means that in the code `((lambda (x y) x) 3 4)`
The lambda is evaluated before the `3`, and the `3` is evaluated
before the `4`. This is important when taking assignment (`set!`) into
account.

- The `set!` form's return value is unspecified. It is not tested in
  this project. You may return `0`, `#t`, or any similar value.

- The `if` form should not evaluate the branch not taken.

- The only "false" value is `#f`. Everything else is considered "true"
  in the context of an `if` form.

- You may not use Racket's `eval`, or any similar function.

- The `begin` form evaluates all of its expressions and returns the
  last one. It is already implemented for you.

- The exact representation of closures is not directly tested. You are
  allowed to change it if you'd like, though we would not specifically
  recommend doing so.

## Unsorted Advice

- Watch the lecture on closure-creating interpreters when it is
  posted. It contains a significant portion of the answer for this
  assignment.

- Consider using `match-define`, as in...

```
(match-define `(result ,v ,sto+) (interp e env store))
```

- Implement the lambda calculus plus numbers first, then build out
  from there. At the very least, you should be able to make small
  examples work such as `((lambda (x) x) 5)`

- Be careful to propagate the store when evaluating
  subexpressions. For example, consider `(let ([x (begin set! y 2)] [z
  y]) z)`, ensure you take care to propagate the store resulting from
  the evaluation of `x`'s binding to the evaluation of `y` in the
  binding for `z`.

- Advice: Use the grammar like a checklist to remember what you have
  left to implement.

- If you wonder how a feature works, try it in the Racket REPL! They
  should have the same semantics (although evaluating a closure will
  look different in this interpreter and Racket).
