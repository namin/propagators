~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Revised Report on the Propagator Model
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

		by Alexey Radul and Gerald Jay Sussman

Abstract: 

	  In the past year we have made serious progress
	  on elaborating the propagator programming model
	  (see 1,2).  Things have gotten serious enough to
	  build a system that can be used for real
	  experiments.


Perhaps the most important problem facing a programmer is the revision
of an existing program to extend it for some new situation.
Unfortunately, the traditional models of programming provide little
support for this activity.  The programmer often finds that
commitments made in the existing code impede the extension, but the
costs of reversing those commitments are excessive.

Such commitments tend to take the form of choices of strategy.  In the
design of any significant system there are many implementation plans
proposed for every component at every level of detail.  However, in
the system that is finally delivered this diversity of plans is lost
and usually only one unified plan is adopted and implemented.  As in
an ecological system, the loss of diversity in the traditional
engineering process has serious consequences.  

The Propagator Programming Model is an attempt to mitigate this
problem.  It is a model that supports the expression and integration
of multiple viewpoints on a design.  It incorporates explicit
structure to support the integration of redundant pieces and
degenerate subsystems.  It will help us integrate the diversity that
was inherent in the design process into the delivered operational
product.

The Propagator Programming Model is built on the idea that the basic
computational elements are autonomous machines interconnected by
shared cells through which they communicate.  Each machine
continuously examines the cells it is interested in, and adds
information to some based on deductions it can make from information
from the others.  Cells accumulate information from the propagators
that produce that information.  The key idea here is additivity.  New
ways to make contributions can be added just by adding new
propagators; if an approach to a problem doesn't turn out to work
well, it can be ignored, dynamically and without disruption.

.. contents::

Propagator System
======================================================================

Although most of this document introduces you to the Scheme-Propagator
system that we have developed in MIT Scheme, the Propagator Model is
really independent of the language.  You should be able to write
propagators in any language you choose, and others should be able to
write subsystems in their favorite language, that cooperate with your
subsystems.  What is necessary is that all users agree on the protocol
by which propagators communicate with the cells that are shared among
subsystems.  These rules are very simple and we can enumerate them
right here:

Cells must support three operations:
- add some content
- collect the content currently accumulated
- register a propagator to be notified when the accumulated content changes

When new content is added to a cell, the cell must merge the addition
with the content already present.  When a propagator asks for the
content of a cell, the cell must deliver a complete summary of the
information that has been added to it.

The merging of content must be commutative, associative, and
idempotent.  The behavior of propagators must be monotonic with
respect to the lattice induced by the merge operation.  


Getting Started
======================================================================

Scheme-Propagators is implemented in `MIT/GNU Scheme`_, which you will
need in order to use it.  You will also need Scheme-Propagators
itself, which you can check out from the `MMP git archive`_.  Once you
have it, go to the ``propagator/`` directory, start up your Scheme and
load the main entry file with ``(load "load")``.  This gives you a
read-eval-print loop (traditionally called a REPL for short) for both
the Scheme-Propagators system and the underlying Scheme
implementation.  Check out the README for more on this.
TODO Real releases?  From a real web place?

.. _`MIT/GNU Scheme`: http://www.gnu.org/software/mit-scheme/
.. _`MMP git archive`: git@github.com:MIT-MMP/propagator.git

Once you've got your REPL, you can start typing away at it to create
propagator networks, give them inputs, ask them to do computations,
and look at the results.

Here's a little propagator example that adds two and three to get
five::

  (define-cell a)
  (define-cell b)
  (add-content a 3)
  (add-content b 2)
  (define-cell answer (e:+ a b))
  (run)
  (content answer) ==> 5

Each of the parenthesized phrases above are things to type into the
REPL, and the ``==> 5`` at the end is the result that Scheme will
print.  I omitted the results of all the other expressions because
they are not interesting.

Let's have a closer look at what's going on in this example, to serve
as a guide for more in-depth discussion later.  ``define-cell`` is a
Scheme macro for making and naming propagator cells::

  (define-cell a)

creates a new cell and binds it to the Scheme variable ``a``.

::

  (define-cell b)

makes another one.  Then ``add-content`` is the Scheme procedure that
directly zaps some information into a propagator cell (all the
propagators use it to talk to the cells, and you can too).  So::

  (add-content a 3)

puts a ``3`` into the cell named ``a``, and::

  (add-content b 2)

puts a ``2`` into the cell named ``b``.  Now ``e:+`` (I'll explain
that naming convention later) is a Scheme procedure that creates a
propagator that adds, attaches it to the given cells as inputs, and
makes a cell to hold the adder's output and returns it.  So::

  (define-cell answer (e:+ a b))

creates an adding propagator, and also creates a cell, now called
``answer``, to hold the result of the addition.  Be careful!  No
computation has happened yet.  You've just made up a network, but it
hasn't done its work yet.  That's what the Scheme procedure ``run`` is
for::

  (run)

actually executes the network, and only when the network is done
computing does it give you back the REPL to interact with.  Finally
``content`` is a Scheme procedure that gets the content of cells::

  (content answer)

looks at what the cell named ``answer`` has now, which is ``5``
because the addition propagator created by ``e:+`` has had a chance to
do its job.  If you had forgotten to type ``(run)`` before typing
``(content answer)``, it would have printed out ``#(*the-nothing*)``,
which means that cell has no information about the value it is meant
to have.


The Details
======================================================================

Now that you know how to play around with our propagators we have to
tell you what we actually provide.  In every coherent system for
building stuff there are primitive parts, the means by which they can
be combined, and means by which combinations can be abstracted so that
they can be named and treated as if they are primitive.  


Making Propagator Networks
======================================================================

The ingredients of a propagator network are cells and propagators.
The cells' job is to remember things; the propagators' job is to
compute.  The analogy is that propagators are like the procedures of a
traditional programming language, and cells are like the memory
locations; the big difference is that cells accumulate partial
information (which may involve arbitrary internal computations), and
can therefore have many propagators reading information from them and
writing information to them.

The two basic operations when making a propagator network are making
cells and attaching propagators to cells.  You already met one way to
make cells in the form of ``define-cell``; we will talk about more
later, but let's talk about propagators first.


Attaching Basic Propagators: d@ and p:foo
----------------------------------------------------------------------

The Scheme procedure ``d@`` attaches propagators to cells.  The
name ``d@`` is mnemonic for "diagram apply".  For
example, ``p:+`` makes adder propagators::

  (d@ p:+ foo bar baz)

means attach a propagator that will add the contents of the cells
named ``foo`` and ``bar`` and write the sum into the cell named ``baz``.
Once attached, whenever either the ``foo`` cell or the ``bar`` cell
gets any new interesting information, the adding propagator will
eventually compute the appropriate sum and give it to ``baz`` as an
update.

Many propagator primitives directly expose procedures from the
underlying Scheme, with the naming convention that ``p:foo`` does the
job ``foo`` to the contents of an appropriate pile of input cells and
gives the result to an output cell (``p`` stands for "propagator"):

p:+
p:-
p:*
p:/
p:abs
p:square
p:sqrt
p:=
p:<
p:>
p:<=
p:>=
p:not
p:and
p:or
p:eq?
p:eqv?
p:expt

Others do not correspond to Scheme procedures exactly:
(p:constant value)
p:swtich
p:conditional
p:conditional-router
p:==
p:cons
p:pair?
p:car
p:cdr

By convention, cells used as outputs go last.

Propagator Expressions: e@
----------------------------------------------------------------------

The ``d@`` style is the right underlying way to think about the
construction of propagator networks.  However, it has the unfortunate
feature that it requires the naming of cells for holding all
intermediate values in a computation, and in that sense programming
with ``d@`` feels a lot like writing assembly language.

It is pretty common to have expressions: one's propagator networks
will have some intermediate values that are produced by only one
propagator, and consumed by only one propagator.  In this case it is a
drag to have to define and name a cell for that value, if one would
just name it "the output of foo".  Scheme-Propagators provides a
syntactic sugar for writing cases like this in an expression style, like a
traditional programming language.

The Scheme procedure ``e@`` attaches propagators in expression style.
The name ``e@`` is mnemonic for "expression apply".  The ``e@``
procedure is just like ``d@``, except it synthesizes an extra cell to
serve as the last argument to ``d@``, and returns it from the ``e@``
expression (whereas the return value of ``d@`` is unspecified).  For
example, here are two ways to do the same thing::

  (define-cell x)
  (define-cell y)
  (define-cell z)
  (d@ p:* x y z)

and::

  (define-cell x)
  (define-cell y)
  (define-cell z (e@ p:* x y))

Generally the ``e@`` style is convenient because it chains in
the familiar way

::

  (e@ p:- w (e@ p:* (e@ p:+ x y) z))

Because of the convention that output cells are listed last,
expressions in ``e@`` style build propagator networks that
compute corresponding Lisp expressions.

On the other hand, the ``d@`` style is necessary when a propagator
needs to be attached to a full set of cells that are already there.
For example, if one wanted to be able to go back from ``z`` and one of
``x`` or ``y`` to the other, rather than just from ``x`` and ``y`` to
``z``, one could write::

  (define-cell x)
  (define-cell y)
  (define-cell z (e@ p:* x y))
  (d@ p:/ z x y)
  (d@ p:/ z y x)

and get a multidirectional constraint::

  (add-content z 6)
  (add-content x 3)
  (run)
  (content y) ==> 2

To save typing when the propagator being attached is known at network
construction time, the ``p:foo`` objects are also themselves
applicable in Scheme, defaulting to applying themselves in the ``d@``
style.  Each also has an ``e:foo`` variant that defaults to the ``e@``
style.  So the following also works::

  (define-cell x)
  (define-cell y)
  (define-cell z (e:* x y))
  (p:/ z x y)
  (p:/ z y x)

TODO This discussion of explicitly applying cells whose content
propagator constructors are not yet known probably goes later in the
document:

The preceding discusses attaching propagators to cells when the
propagators being attached are known at network construction time.
Since Scheme-Propagators is a higer-order language, that will not
always be the case.  (TODO Example?)  More important, like all other
possible data, the propagator being applied may be known only
partially.  What is ``d@`` to do then?

If the cell which is the first argument to ``d@`` does not have a
propagator in it, or has only a partially known propagator in it,
``d@`` constructs a propagator that will dynamically apply propagators
that show up in that cell when they do.  Moreover, ``d@`` ensures that
any uncertainty associated with the question of which propagator is
being applied is forwarded to the output (and also to the interior of
the application).  (TODO Example)

``d@`` always applies in diagram style.  ``e@`` always applies in
expression style.  If you put into operator position a cell that
contains a fully-known propagator at network construction time, it
will be applied either in diagram style or expression style, as
dependent on that propagator's default preference.  If you put into
operator position a cell that does not have a fully-known propagator
at network construction time, it will be applied in diagram style by
default (TODO or should it signal an error?)


Propagator Constraints: c:foo and ce:foo
----------------------------------------------------------------------

Constraints are so useful that many are predefined, and they have
their own naming convention.  ``c:`` stands for "constraining".  A
thing named ``c:foo`` is the constraining analogue of ``p:foo``, in
that in addition to attaching a propagator that does ``foo`` to its
cells, it also attaches ``foo-inverse`` propagators that deduce
"inputs" from "outputs".  For example, the product constraint that we
built in the previous section is available as ``c:*``::

  (define-cell x)
  (define-cell y)
  (define-cell z)
  (d@ c:* x y z)

  (add-content z 12)
  (add-content y 4)
  (run)
  (content x) ==> 3
  
The ``c:foo`` objects, like the ``p:foo`` objects, are also
self-applicable, and also default to applying themselves
in diagram style::

  (c:* x y z)  ==  (d@ c:* x y z)

The ``c:foo`` objects also have ``ce:foo`` analogues, that
apply themselves in expression style::

  (ce:* x y)  ==  (e@ c:* x y)

Of course, not every operation has a useful inverse, so there are
fewer ``c:`` procedures defined than ``p:``:

c:+       ce:+
c:*       ce:*
c:square  ce:square
c:not     ce:not
c:id
c:==      ce:==


Constants and Literal Values
----------------------------------------------------------------------

Programs have embedded constants all the time, and propagator programs
are no different (except that constant values, like all other values,
can be partial).  We've already seen one way to put a Scheme value
into a propagator program: the ``add-content`` procedure zaps a value
straight into a cell.  This is generally encouraged at the REPL, but
frowned upon in actual programs.  It is much nicer to use ``constant``
or ``p:constant`` (they're the same) to make a propagator that will
zap your value into your cell for you::

  (define-cell thing)
  ((constant 5) thing)
  (content thing) ==> #(*the-nothing*)
  (run)
  (content thing) ==> 5

There is also an expression-oriented version, called, naturally,
``e:constant``::

  (define-cell thing (e:constant 5))
  (run)
  (content thing) ==> 5

Constant Conversion
----------------------------------------------------------------------

In fact, inserting constants is so important, that there is one more
nicification of this: whenever possible, the system will convert a raw
constant (i.e. a non-cell Scheme object) into a cell, using
``e:constant``.

Some examples::

  (e:+ x 2)          ==   (e:+ x (e:constant 2))
  (define-cell x 4)  ==   (define-cell x (e:constant 4))
  (c:+ x y 0)        ==   (c:+ x y (e:constant 0))

Making Cells
----------------------------------------------------------------------

Cells are the memory locations of the Scheme-Propagators
language; Scheme variables whose bindings are cells correspond to
Scheme-Propagators variables (Scheme variables whose bindings are
other things look like syntax to Scheme-Propagators).  We've
already met one way to make cells::

  (define-cell x)

creates a Scheme variable named ``x`` and binds a cell to it.  The
underlying mechanism underneath this is the procedure ``make-cell``,
which creates a cell and lets you do whatever you want with it.  So
you could write::

  (define x (make-cell))

which would also make a Scheme variable named ``x`` and bind a cell to
it.  In fact, that is almost exactly what ``define-cell`` does, except
that ``define-cell`` attaches some metadata to the cell it creates to
make it easier to debug the network (see below) and also does constant
conversion (so ``(define-cell x 5)`` makes ``x`` a cell that will get
a ``5`` put into it, whereas ``(define x 5)`` would just bind ``x`` to
``5``).

Just as Scheme has several mechanisms of making variables, so
Scheme-Propagators has corresponding ones.  Corresponding to Scheme's
``let``, Scheme-Propagators has ``let-cells``::

  (let-cells ((foo (e:+ x y))
              (bar (e:* x y)))
    ...)

will create the Scheme bindings ``foo`` and ``bar``, and bind them to
the cells made by ``(e:+ x y)`` and ``(e:* x y)``, respectively (this
code is only sensible if ``x`` and ``y`` are already bound to cells
(or subject to constant conversion)).  The new bindings will only be
visible inside the scope of the ``let-cells``, just like in Scheme;
but if you attach propagators to them, the cells themselves will
continue to exist and function as part of your propagator network.

One notable difference from Scheme: a cell in a propagator network,
unlike a variable in Scheme, has a perfectly good "initial state".
Every cell starts life knowing ``nothing`` about its intended
contents; where Scheme variables have to start life in a weird
"unassigned" state, ``nothing`` is a perfectly good partial
information structure.  This means that it's perfectly reasonable
for ``let-cells`` to make cells with no initialization forms::

  (let-cells (x y (foo (some thing))) ...)

creates cells named ``x`` and ``y``, which are empty and have
no propagators attached to them initially, and also a cell
named ``foo`` like above.  ``let-cells`` also recognizes the
usage::

  (let-cells ((x) (y) (foo (some thing))) ...)

by analogy with Scheme ``let``.

Corresponding to Scheme's ``let*``, Scheme-Propagators has
``let-cells*``.  ``let-cells*`` is to ``let-cells`` what ``let*`` is
to ``let``::

  (let-cells* ((x)
               (y (e:+ x x)))
    ...)

will make a cell named ``x`` and a cell named ``y`` with an adder both
of whose inputs are ``x`` and whose output is ``y``.

Now, ``let-cells`` and ``let-cells*`` are, like ``define-cell``,
basically a convenience over doing the same thing in Scheme with
``let``, ``let*`` and ``make-cell``.  Also like ``define-cell``,
``let-cells`` and ``let-cells*`` do constant conversion (so in
``(let-cells ((x 3)) ...)``, ``x`` becomes a cell, not a Scheme
object), and attach debugging information to the cells they bind.

Since ``let-cells`` is plural (where ``let`` was number-neutral),
Scheme-Propagators also define ``let-cell`` for the case when you just
want to make one cell::

  (let-cell x ...)              ==>  (let-cells (x) ...)
  (let-cell (x (e:+ y z)) ...)  ==>  (let-cells ((x (e:+ y z))) ...)

Scheme-Propagators currently has no analogues of Scheme's ``letrec``
or named ``let`` syntax.

Finally, there is one more, somewhat sneaky way to make cells.
The ``e@`` 
procedure makes and returns a cell to hold the "output" of the
propagator being applied.  These implicit cells are just
like the implicit memory locations that Scheme creates under the hood
for holding the return values of expressions before they get used by
the next expression or assigned to variables.

Conditional Network Construction
----------------------------------------------------------------------

The ``switch`` propagator does conditional propagation --- it only
forwards its input to its output if its control is ``#t``.  As such,
it serves the purpose of controlling the flow of data through an existing
propagator network.  However, it is also appropriate to control the
construction of more network, for example to design recursive networks
that expand themselves no further than needed.  The basic idea here
is to delay the construction of some chunk of network until
some information appears on its boundary, and control whether
said information appears by judicious use of ``switch`` propagators.  The
low-level tools for accomplishing this effect are
``delayed-propagator-constructor`` and ``switch``.  Scheme macros that
do the right thing at a higher level are also provided:

``(p:when internal-cells condition-cell body ...)``
  Delays the construction of the body until reason to believe the
  condition may be true appears in the condition-cell.  The
  ``condition-cell`` argument is an expression to evaluate to produce
  the cell controlling whether construction of the ``body`` takes
  place.  The ``body`` is an arbitrary collection of code, defining
  some amount of propagator network that will not be built until the
  controlling cell indicates that it should.  The ``internal-cells``
  argument is a list of the free variables in ``body``.  This is the
  same kind of kludge as the ``import`` clause in
  ``propagator-lambda`` (which see).

``(e:when internal-cells condition-cell body ...)``
  Expression-style variant of ``p:when``.  Augments its boundary with
  a fresh cell, which is then synchronized with the cell returned from
  the last expression in ``body`` when ``body`` is constructed.

``(p:unless internal-cells condition-cell body ...)``

``(e:unless internal-cells condition-cell body ...)``
  Same as ``p:when`` and ``e:when``, but reversing the sense of the
  control cell.

``(p:if internal-cells condition-cell consequent alternate)``
  Two-armed conditional construction.  Just like two instances of
  ``p:when``: constructs the network indicated by the consequent form
  when the condition-cell becomes possibly true, and constructs the
  network indicated by the alternate form when the condition-cell
  becomes possibly false.  Note that both can occur for the same
  ``p:if`` over the life of a single computation, for example if the
  condition-cell comes to have a TMS that includes a ``#t`` contingent
  on some premises and later a ``#f`` contingent on others.

``(e:if internal-cells condition-cell consequent alternate)``
  Expression-style variant of ``p:if``.

Making New Compound Propagators
======================================================================

So, you know the primitives (the supplied propagators) and the means
of combination (how to make cells and wire bunches of propagators up
into networks).  Now for the means of abstraction.  A propagator
constructor such as ``p:+`` is like a wiring diagram with a few holes
where it can be attached to other structures.  Supply ``p:+`` with
cells, and it makes an actual propagator for addition whose inputs and
outputs are those cells.  How do you make compound propagator
constructors?

The main way to abstract propagator construction is with the
``define-d:propagator`` and ``define-e:propagator`` Scheme macros.
``define-d:propagator`` defines a compound propagator in diagram style,
that is, with explicit named parameters for the entire boundary of the
compound::

  (define-d:propagator (my-sum-constraint x y z)
    (p:+ x y z)
    (p:- z y x)
    (p:- z x y))

``define-e:propagator`` defines a compound propagator in expression
style, that is, expecting the body of the propagator to return one
additional cell to add to the boundary at the end::

  (define-e:propagator (double x)
    (e:+ x x))

Both defining forms will make variants with names beginning in ``p:``
and ``e:``, that default to being applied in diagram and expression
style, respectively.  Note that this definition does not bind
``double``.

With these definitions we can use those pieces to build more complex
structures::

  (p:my-sum-constraint x (e:double x) z)

which can themselves be abstracted so that they can be used
as if they were primitive::

  (define-d:propagator (foo x z)
    (p:my-sum-constraint x (e:double x) z))

``define-propagator`` is an alias for ``define-d:propagator`` because
that's the most common use case.

Just like in Scheme, the definition syntaxes have a corresponding
syntax for anonymous compound propagators, ``lambda-d:propagator`` and
``lambda-e:propagator``.

Compound propagator constructors perform constant conversion::

  (p:my-sum-constraint x 3 z)  ==  (p:my-sum-constraint x (e:constant 3) z)

``define-propagator`` and ``define-e:propagator`` respect the ``c:``
and ``ce:`` naming convention, in that if the name supplied for
definition begins with ``c:`` or ``ce:``, that pair of prefixes will
be used in the names actually defined instead of ``p:`` and ``e:``.
So::

  (define-propagator (foo ...) ...)     defines  p:foo and e:foo
  (define-propagator (p:foo ...) ...)   defines  p:foo and e:foo
  (define-propagator (e:foo ...) ...)   defines  p:foo and e:foo
  (define-propagator (c:foo ...) ...)   defines  c:foo and ce:foo
  (define-propagator (ce:foo ...) ...)  defines  c:foo and ce:foo

Lexical Scope
----------------------------------------------------------------------

Compound propagator definitions can be closed over cells available in
their lexical environment::

  (define-e:propagator (addn n)
    (define-e:propagator (the-adder x)
      (import n)
      (e:+ n x))
    e:the-adder)

``import`` is a kludge, which is a consequence of the embedding of
Scheme-Propagators into Scheme.  Without enough access to the Scheme
interpreter, or enough Bawden-compliant wizardry, we cannot detect the
free variables in an expression, so they must be listed explicitly by
the user.  Globally bound objects like ``e:+`` (and ``p:addn`` and
``e:addn`` if the above were evaluated at the top level) need not be
mentioned.

Recursion
----------------------------------------------------------------------

Propagator abstractions defined by ``define-propagator`` are expanded
immediately when applied to cells.  Therefore, magic is needed to
build recursive networks, because otherwise the structure would be
expanded infinitely far.  As in Scheme, this magic is in ``if``.  The
Scheme-Propagators construct ``p:if`` (which is implemented as a
Scheme macro) delays the construction of the diagrams in its branches
until information is available about the predicate.  Specifically, the
consequent is constucted only when there is good information to the
effect that the predicate might be true, and the alternate is
constructed only when there is good information to the effect that the
predicate might be false.  Note that, unlike in Scheme, these can both
occur to the same ``p:if``.

In Scheme-Propagators, the one-armed conditional construction
construct ``p:when`` is more fundamental than the two-armed construct
``p:if``.  This is because, where Scheme's ``if`` is about selecting
values, and so has to have two options to select from, ``p:when`` and
``p:if`` are about building machinery, and there is no particular
reason why choosing among two pieces of machinery to construct is any
more basic than choosing whether or not to construct one particular
piece.

For example, here is the familiar recursive ``factorial``, rendered in
propagators with ``p:if``::

  (define-propagator (p:factorial n n!)
    (p:if (n n!) (e:= 0 n)
      (p:== 1 n!)
      (p:== (e:* n (e:factorial (e:- n 1))) n!)))

The only syntactic difference between this and what one would write in
Scheme for this same job is that this is written in diagram style,
with an explicit name for the cell that holds the answer, and that
``p:if`` needs to be told the names of the non-global variables that
are free in its branches, just like the ``import`` clause of a
propagator definition (and for the same kludgerous reason).
``p:when`` is the one-armed version.  ``p:unless`` is also provided;
it reverses the sense of the predicate.

Like everything else whose name begins with ``p:``, ``p:if`` and co
have expression-style variants.  The difference is that the tail
positions of the branches are expected to return cells, which are
wired together and returned to the caller of the ``e:if``.  Here is
``factorial`` again, in expression style::

  (define-e:propagator (e:factorial n)
    (e:if (n) (e:= 0 n)
      1
      (e:* n (e:factorial (e:- n 1)))))

Looks familiar, doesn't it?


Macrology
----------------------------------------------------------------------

Sometimes you will need to make something that looks like a macro to
Scheme-Propagators.  The macro language of Scheme-Propagators is
Scheme.  For example::

  (define (my-diagram x y z)
    (p:+ x y z)
    (p:- z y x)
    (p:- z x y))

``my-diagram`` is a Scheme-Propagators macro that, when given three
cells, wires up three arithmetic propagators to them.  This simple
example of course gains nothing from being a macro rather
than a normal compound propagator, but using Scheme as a macro
language lets you do more interesting things::

  (define (require-distinct cells)
    (for-each-distinct-pair
     (lambda (c1 c2)
       (forbid (e:= c1 c2)))
     cells))

This ``require-distinct`` uses a Scheme iterator to perform a
repetitive task over a bunch of Scheme-Propagators cells.

This is quite convenient, but sometimes one wants the debugging data
provided by ``define-propagator``.  This is what
``define-propagator-syntax`` is for.  Just change ``define`` to
``define-propagator-syntax``::

  (define-propagator-syntax (require-distinct cells)
    (for-each-distinct-pair
     (lambda (c1 c2)
       (forbid (e:= c1 c2)))
     cells))


Using Partial Information
======================================================================

Partial, accumulatable information is essential to
multidirectional, nonsequential programming.  Each "memory
location" of Scheme-Propagators, that is each cell, maintains not "a
value", but "all the information it has about a value".  Such
information may be as little as "I know absolutely nothing about my
value", as much as "I know everything there is to know about my value,
and it is ``42``", and many possible variations in between; and also
one not-in-between variation, which is "Stop the presses!  I know
there is a contradiction!"

All these various possible states of information are represented (per
force) as Scheme objects.  The Scheme object ``nothing`` represents
the information "I don't know anything".  This only takes a single
Scheme object, because not knowing anything is a single state of
knowledge.  Most Scheme objects represent "perfect, consistent"
information: the Scheme object ``5`` represents the information "I
know everything there is to know, and the answer is ``5``."  There are
also several Scheme types provided with the system that denote
specific other states of knowledge, and you can make your own.  For
example, objects of type ``interval?`` contain an upper bound and a
lower bound, and represent information of the form "I know my value is
between this real number and that one."

The way to get partial knowledge into the network is to put it into
cells with ``add-content`` or constant propagators.  For example::

  (define-cell x (make-interval 3 5))

produces a cell named ``x`` that now holds the partial information
``(make-interval 3 5)``, which means that its value is
between ``3`` and ``5``.

Partial information structures are generally built to be contagious,
so that once you've inserted a structure of a certain type into
the network, the normal propagators will generally produce answers
in kind, and, if needed, coerce their inputs into the right form
to co-operate.  For example, if ``x`` has an interval like above,

::

  (define-cell y (e:+ x 2))

will make an adder that will eventually need to add ``2`` to the
interval between ``3`` and ``5``.  This is a perfectly reasonable
thing to ask, because both ``2`` and ``(make-interval 3 5)`` are
states of knowledge about the inputs to that adder, so it ought to
produce the best possible representation of the knowledge it can
deduce about the result of the addition.  In this case, that would be
the interval between ``5`` and ``7``::

  (run)
  (content y)  ==>  #(interval 5 7)

The key thing about partial information is that it's
cumulative.  So if you also added some other knowledge to the ``y``
cell, it would need to merge with the interval that's there to
represent the complete knowledge available as a result::

  (add-content y (make-interval 4 6))
  (content y)  ==>  #(interval 5 6)

If incoming knowledge hopelessly contradicts the knowledge a cell
already has, it will complain::

  (add-content y 15)  ==>  An error

stop the network mid-stride, and give you a chance to examine the
situation so you can debug the program that led to it, using the
standard MIT Scheme debugging facilities.


Built-in Partial Information Structures
======================================================================

- nothing
- just a value
- intervals
- cons cells
- propagator cells
- closures
- supported values
- truth maintenance systems
- contradiction

Nothing
----------------------------------------------------------------------

A Raw Scheme Object
----------------------------------------------------------------------

This state of information indicates that the content of the cell is
completely known, and is exactly (by ``eqv?``) that object.  Note:
floating point numbers are compared by approximate numerical equality;
this is guaranteed to screw you eventually, but we don't know how to
do better.

Numerical Intervals
----------------------------------------------------------------------

An object of type ``interval?`` has fields for a lower bound and an
upper bound.  Such an object represents the information "This value is
between these bounds."  Cells merge intervals by intersecting them.

(make-interval low high)
  Creates an interval with the given lower and upper bounds

(interval-low interval)
  Extracts the lower bound of an interval

(interval-high interval)
  Extracts the upper bound of an interval

(interval? thing)
  Tests whether the given object is an interval

As an interval arithmetic facility, this one is very primitive.  In
particular it assumes that all the numbers involved are positive.  The
main purpose of including it is to have a partial information
structure with an intuitive meaning, and that requires nontrivial
operations on the information it is over.

Compound Data
----------------------------------------------------------------------

Scheme pairs are not assumed to denote exactly themselves.  A
propagator cell will merge Scheme pairs by recursively merging the
``car`` and ``cdr`` fields.  The fields should contain further
propagator cells; given the behavior of propagator cells as mergeable
data, the effect will be unification.  A Scheme pair merged with a
Scheme object of a different type will produce a contradiction.

cons, car, cdr, pair?, null?
  These are Scheme procedures with their usual Scheme meanings.

p:cons, e:cons, p:car, e:cdr, p:pair?, e:pair?, p:null?, e:null?
  Propagators for dealing with compound data.  For example::

    (p:cons x y z)

  puts a pair holding the cells ``x`` and ``y`` into cell ``z``.
  
::
    (p:car z w)

  ensures that the cell in the car of the pair in cell ``z`` is
  equivalent to cell ``w``.

slotful-information-type
  Declares that additional Scheme data structures should be merged
  the way pairs are.  For example::

    (slotful-information-type pair? cons car cdr)

  is the declaration that causes the system to treat Scheme pairs the
  way it does.

Propagator Cells as Partial Information
----------------------------------------------------------------------

Propagator cells merge with each other by attaching bidirectional
identity propagators that keep the contents of the cells in sync.
These identity propagators will cause the contents of the cells to
merge, both now and in the future.

Closures
----------------------------------------------------------------------

Propagator closures as mergeable data behave like a compound data
structure.  A closure is a code pointer together with an environment.
The code pointer is a Scheme procedure; the environment is a map from
names to cells, and as such is a compound structure containing cells.
Code pointers merge by testing that they point to the same code
(merging closures with different code produces a contradiction), and
environments merge by merging all the cells they contain in
corresponding places.

make-closure, make-e:closure
  Scheme procedures for directly constructing closure objects.  The
  closures are defined in diagram or expression style, respectively.

lambda-d:propagator, lambda-e:propagator
  Scheme-Propagators syntax for anonymous compound propagator
  constructors (which are implemented as closures).

define-propagator
  Internally produces lambda-d:propagator or lambda-e:propagator
  and puts the results into appropriately named cells.

Truth Maintenance Systems
----------------------------------------------------------------------

A Truth Maintenance System (TMS) is a kind of partial information
structure that may appear in a cell.  A TMS is a set of contingent
values.  A contingent value is any partial information object that
describes the "value" in the cell, together with a set of premises.
The premises are Scheme objects that have no interesting properties
except identity (by ``eq?``).  A worldview defines which premises are
believed.

The meaning of a TMS as information is the logical ``and`` of the
meanings of all of its contingent values.  The meaning of each
contingent value is an implication: The conjunction of the premises
implies the contingent information.  Therefore, given a worldview,
some of the contingent information is believed and some is not.  If
the TMS is queried, it produces the best summary it can of the
believed information, together with the full set of premises that
information is contingent upon.

In this system, there is a single current global worldview, which
starts out believing all premises.  The worldview may be changed to
exclude (or re-include) individual premises, allowing the user to
examine the consequences of different consistent subsets of premises.

(kick-out! premise)
  Remove the given premise from the current worldview.

(bring-in! premise)
  Return the given premise to the current worldview.

(premise-in? premise)
  Is the given premise believed in the current worldview?

(contingent info premises)
  Constructs a contingency object representing the information
  that the given info is contingent on the given list of premises.

(contingent-info contingency-object)
  The information that is contingent.

(contingent-premises contingency-object)
  The list of premises on which that information is contingent.

(contingency-object-believed? contingency-object)
  Whether the given contingency object is believed.

(make-tms contingency-object-list)
  Constructs a TMS with the given contingency objects as its initial
  set.

(tms-query tms)
  Returns a contingency object representing the strongest deduction
  the given TMS can make in the current worldview.  tms-query gives
  the contingency with the strongest contingent information that is
  believed in the current worldview.  Given that desideratum,
  tms-query tries to minimize the premises that information is
  contingent upon.

Calling ``initialize-scheduler`` resets the worldview to believing all
premises.

TMSes merge by appending their lists of known contingencies (and
sweeping out redundant ones).  Usually, propagators react to TMSes by
querying them to obtain ingredients for computation.  The result of a
computation is contingent on the premises of the ingredients that
contribute to that result.

Contradiction
----------------------------------------------------------------------

The Scheme object ``the-contradiction`` represents a completely
contradictory state of information.  If a cell ever finds itself in
such a completely contradictory state, it will signal an error.  The
explicit ``the-contradiction`` object is useful, however, for
representing contradictory information in recursive contexts.  For
example, a truth maintenance system may discover that some collection
of premises leads to a contradiction --- this is represented by a
``the-contradiction`` object contingent on those premises.

Implicit Dependency-Directed Search
----------------------------------------------------------------------

If a cell discovers that it contains a TMS that harbors a contingent
contradiction, the cell will signal that the premises of that
contradiction form a nogood set, and that nogood set will be recorded.
For the worldview to be consistent, at least one of those premises
must be removed.  The system maintains the invariant that the current
worldview never has a subset which is a known nogood.

If a nogood set consists entirely of user-introduced premises, the
computation will be suspended, a description of the nogood set will be
printed, and the user will have the opportunity to remove an offending
premise (with ``kick-out!``) and, if desired, resume the computation
(with ``run``).

There is also a facility for introducing hypothetical premises that
the system is free to manipulate automatically.  If a nogood set
contains at least one hypothetical, some hypothetical from that nogood
set will be retracted, and the computation will proceed.

``(p:amb cell)``, ``(e:amb)``
  A propagator that emits a TMS consisting of a pair of contingencies.
  One contains the information ``#t`` contingent on one fresh
  hypothetical premise, and the other contains the information ``#f``
  contingent on anther.  ``amb`` also tries to maintain the invariant
  that exactly one of those premises is believed.  If the current
  worldview is such that bringing either premise in will cause a known
  nogood set to be believed, then, by performing a cut, the ``amb``
  discovers and signals a new nogood set that does not include either
  of them.  Together with the reaction of the system to nogood sets,
  this induces an emergent satisfiability solver by the resolution
  principle.

``(p:require cell)``, ``(e:require)``
  A propagator that requires its given cell to be true (to wit,
  signals contradictions if it is not).

``(p:forbid cell)``, ``(e:forbid)``
  A propagator that forbids its given cell from being true (to wit,
  signals contradictions if it is).

``(p:one-of input ... output)``, ``(e:one-of input ...)``
  An n-ary version of ``amb``.  Picks one of the objects in the given
  input cells using an appropriate collection of ``amb`` propagators
  and puts it into its output cell.

``(require-distinct cells)``
  Requires all of the objects in its list of input cells to be
  distinct (in the sense of ``eqv?``)


Making New Kinds of Partial Information
======================================================================

The partial information types are defined by a suite of generic
operations.  The critical ones for defining the actual partial
information types are ``equivalent?``, ``merge``, and
``contradictory?``, which test whether two information structures
represent the same information, merge given information structures,
and test whether a given information structure represents an
impossible state, respectively.  In addition, the primitive
propagators are equipped with generic operations for giving them
custom behaviors on the various information structures, and the
generic operation ``binary-map`` is very useful for the circumstance
when a whole class of propagators should handle a particular
information type uniformly.

To create your own partial information structure, you should create an
appropriate Scheme data structure to represent it, and then add
handlers to the operations ``equivalent?``, ``merge``, and
``contradictory?`` to define that data structure's interpretation as
information.  In order to do anything useful with your new information
structure, you will also need to make sure that the propagators you
intend to use with it can deal with it appropriately.  You can of
course create custom propagators that handle your partial information
structure.  Standard generic operations are also provided for
extending the built-in primitive propagators to handle new partial
information types.  Compound propagators are a non-issue because they
will just pass the relevant structures around to the appropriate
primitives.

It is also important to make sure that your new partial information
structure intermixes and interoperates properly with the existing
ones (see Built-in Partial Information Structures).

Method addition in the generic operation system used in
Scheme-Propagators is done with the ``defhandler`` procedure::

  (defhandler operation handler arg-predicate ...)

The generic operations system is a predicate dispatch system.  Every
handler is keyed by a bunch of predicates that must accept the
arguments to the generic procedure in turn; if they do, that handler
is invoked.  For example, merging two intervals with each other
can be defined as::

  (defhandler merge intersect-intervals interval? interval?)

You can also define your own generic operations, but that is not
relevant here.  TODO Add a pointer to documentation of the generic
operations system, including defining generics, precedence of
handlers, and tagging procedures as type testers for performance;
also use of SOS specializers as predicates directly.
Also the coersion subsystem?

An Example: Adding Interval Arithmetic
----------------------------------------------------------------------

The first step is to define a data structure to represent an interval.
Intervals have upper and lower bounds, so a Scheme record structure
with constructor ``make-interval``, accessors ``interval-low`` and
``interval-high``, and predicate ``interval?`` will do.

The second step is to define handlers for the generic operations that
every partial information structure must implement.  Assuming
appropriate procedures for intersecting intervals and for testing them
for equality and emptiness, those handlers would be::

  (defhandler equivalent? interval-equal? interval? interval?)
  (defhandler merge intersect-intervals interval? interval?)
  (defhandler contradictory? empty-interval? interval?)

To make intervals interoperate with numbers in the same network,
we can add a few more handlers::

  (define (number=interval? number interval)
    (= number (interval-low interval) (interval-high interval)))
  (defhandler equivalent? number=interval? number? interval?)
  (defhandler equivalent? (binary-flip number=interval?) interval? number?)

  (define (number-in-interval number interval)
    (if (<= (interval-low interval) number (interval-high interval))
	number
	the-contradiction))
  (defhandler merge number-in-interval number? interval?)
  (defhandler merge (binary-flip number-in-interval) interval? number?)

The third step is to teach the arithmetic propagators to handle
intervals.  Interval arithmetic does not fit into the ``binary-map``
worldview (which see below), so the only way to do intervals is to
individually add the appropriate handlers to the generic procedures
underlying the primitive propagators::

  (defhandler generic-+ add-interval interval? interval?)
  (defhandler generic-- sub-interval interval? interval?)
  (defhandler generic-* mul-interval interval? interval?)
  (defhandler generic-/ div-interval interval? interval?)
  (defhandler generic-sqrt sqrt-interval interval?)
  ;; ...

In order for the binary propagators to handle the situation where that
propagator has an interval on one input and a number on the other,
further handlers need to be added that tell it what to do in those
circumstances.  The generic procedure system has been extended
with support for automatic coercions for this purpose.

Generic Coercions
----------------------------------------------------------------------

Every number can be seen as an interval (whose lower and upper bounds
are equal).  The definition of arithmetic on mixed intervals and
numbers can be deduced from the definitions of arithmetic on just
intervals, arithmetic on just numbers, and this procedure for viewing
numbers as intervals.  The generic operations system provided with
Scheme-Propagators has explicit support for this idea.

``(declare-coercion-target type [ default-coercion ])``
  This is a Scheme macro that expands into the definitions needed to
  declare ``type`` as something that other objects may be coerced
  into.  If supplied, it also registers a default coercion from
  anything declared coercible to ``type``.

  ``declare-coercion-target`` defines the procedure ``type-able?``,
  which tests whether a given object has been declared to be coercible
  to ``type``, and the procedure ``->type``, which does that coercion.
  These rely on the type-tester for ``type`` already being defined and
  named ``type?``.  For example::

    (declare-coercion-target interval)

  relies on the procedure ``interval?`` and defines the procedures
  ``->interval`` and ``interval-able?``.  This call does not declare a
  default means of coercing arbitrary objects into intervals.

``(declare-coercion from-type to-coercer [ mechanism ])``
  Declares that the given ``from-type`` is coercible by the given
  coercer operation, either by the given ``mechanism`` if supplied or
  by the default mechanism declared in the definition of the given
  coercer.  For example::

    (declare-coercion number? ->interval (lambda (x) (make-interval x x)))

  declares that Scheme number objects may be coerced to intervals
  whose lower and upper bounds are equal to that number.  After this
  declaration, ``interval-able?`` will return true on numbers, and
  ``->interval`` will make intervals out of numbers.

``(defhandler-coercing operation handler coercer)``
  The given generic operation must be binary.  Defines handlers for
  the given generic operation that have two effects: ``handler`` is
  invoked if that operation is given two arguments of the type
  corresponding to ``coercer``; and if one argument is of that type
  and the other has been declared coercable to that type it will be so
  coerced, and then handler will be invoked.  For example::

    (defhandler-coercing generic-+ add-interval ->interval)

  declares that intervals should be added by ``add-interval``, and
  that anything ``interval-able?`` can be added to an interval by
  first coercing it into an interval with ``->interval`` and then
  doing ``add-interval``.  This subsumes

  ::
    (defhandler generic-+ add-interval interval? interval?)

  ``defhandler-coercing`` may only be called after a call to
  ``declare-coercion-target`` defining the appropriate coercer and
  coercability tester procedures (but the various specific coercions
  may be declared later).


TODO Describe declare-type-tester?

The Partial Information Generics
----------------------------------------------------------------------

``(equivalent? info1 info2)  ==>  #t or #f``

The ``equivalent?`` procedure is used by cells to determine whether
their content has actually changed after an update.  Its job is to
ascertain, for any two partial information structures, whether they
represent the same information.  As a fast path, any two ``eqv?``
objects are assumed to represent equivalent information structures.
The default operation on ``equivalent?`` returns false for any two
non-``eqv?`` objects.

A handler for ``equivalent?`` is expected to accept two partial
information structures and return ``#t`` if they represent
semantically the same information, and ``#f`` if they do not.

The built-in ``equivalent?`` determines an equivalence relation.
Extensions to it must maintain this invariant.

``(merge info1 info2)  ==>  new-info``

The ``merge`` procedure is the key to the propagation idea.  Its job
is to take any two partial information structures, and produce a new
one that represents all the information present in both of the
input structures.  This happens every time a propagator gives a cell
some new information.  Any two ``equivalent?`` information structures
merge to identically the first of them.  The default operation for
``merge`` on a pair of non-``equivalent?`` structures that the handlers for
``merge`` do not recognize is to assume that they cannot be usefully
merged, and return ``the-contradiction``.

A handler for ``merge`` is exptected to accept two partial
information structures and return another partial information
structure that semantically includes all the information present in
both input structures.  The handler may return
``the-contradiction`` to indicate that the two given partial
information structures are completely mutually exclusive.

``merge`` is expected to determine a (semi-)lattice (up to equivalence
by ``equivalent?``).  That is

- associativity::

  (merge X (merge Y Z))  ~  (merge (merge X Y) Z)
  (equivalent? (merge X (merge Y Z)) (merge (merge X Y) Z)) ==> #t

- commutativity::

  (merge X Y)  ~  (merge Y X)
  (equivalent? (merge X Y) (merge Y X)) ==> #t

- idempotence::

  (X ~ Y) implies (X ~ (merge X Y))
  (or (not (equivalent? X Y)) (equivalent? X (merge X Y))) ==> #t

``(contradictory? info)  ==>  #t or #f``

The ``contradictory?`` procedure tests whether a given information
structure represents an impossible situation.  ``contradictory?``
states of information may arise in the computation without causing
errors.  For example, a TMS (which see) may contain a contradiction in
a contingent context, without itself being ``contradictory?``.  But if
a ``contradictory?`` object gets to the top level, that is if a cell
discovers that it directly contains a ``contradictory?`` state of
information, it will signal an error and stop the computation.

A handler for ``contradictory?`` is expected to accept a partial
information structure, and to return ``#t`` if it represents an
impossible situation (such as an empty interval) or ``#f`` if it does
not.



TODO Make sure that all the behavior of all built-in partial
information structures under all the given generics is duly
documented.


The Full Story on Merge
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The description of ``merge`` as always returning a new partial
information structure is an approximation.  Sometimes, ``merge`` may
return a new partial information structure together with instructions
for an additional effect that needs to be carried out.  For example,
when merging two propagator cells (see Cells as Partial Information),
the new information is just one of those cells, but the two cells also
need to be connected with propagators that will synchronize their
contents.  For another example, in Scheme-Propagators, if a merge
produces a TMS (which see) that contains a contingent contradiction,
the premises that contradiction depends upon must be signalled as a
nogood set (that this requires signalling and is not just another
partial information structure is a consequence of an implementation
decision of TMSes in Scheme-Propagators).

The fully nuanced question that ``merge`` answers is

  "What do I need to do to the network in order to make it reflect
  the discovery that these two information structures are about the
  same object?"

In the common case, the answer to this question is going to be
"Record: that object is best described by this information structure".
This answer is represented by returning the relevant information
structure directly.  Another possible answer is "These two information
structures cannot describe the same object."  This answer is
represented by returning ``the-contradiction``.  Other answers, such
as "Record this information structure and connect these two cells with
synchronizing propagators", are represented by the ``effectful`` data
structure, which has one field for a new partial information structure
to record, and one field for a list of other effects to carry out.
These instructions are represented as explicit objects returned from
``merge`` rather than being carried out directly because this allows
recursive calls to ``merge`` to modify the effects to account for the
context in which that ``merge`` occurs.  For example, if a merge of
two cells occurs in a contingent context inside a merge of two
TMSes, then the instructions to connect those two cells must be
adjusted to make the connection also contingent on the appropriate
premises.

``(make-effectful info effects)``
  Constructs a new effectful result of merge, with the given new
  partial information structure and the given list of effects to carry
  out.  If the resulting effectful object reaches the top level in a
  cell, those effects will be executed in the order they appear in the
  list.

``(effectful-info effectful)``
  Returns the new information content carried in the given
  effectful object.

``(effectful-effects effectful)``
  Returns the list of effects that this effectful object carries.

``(effectful? thing)``
  Tells whether the given object is an effectful object.

``(->effectful thing)``
  Coerces a possibly-effectless information structure into an
  effectful object.  If the ``thing`` was already effectful,
  returns it, otherwise wraps it into an effectful object
  with an empty list of effects.

``(effectful-> effectful)``
  Attempts to coerce an effectful object into an explicitly effectless
  one.  If the given effectful object was not carrying any effects
  that would have any effect when executed, returns just the
  information structure it was carrying.  Otherwise, returns
  the given effectful object.

``(effectful-bind effectful func)``
  Runs the given ``func`` on the information content in the given
  ``effectful`` object, and reattaches any effects.  The effectful
  object may actually be a partial information structure without
  explicit effects.  The func may return a new partial information
  structure or a new effectful object.  The overall result of
  ``effectful-bind`` is the information returned by the call to
  ``func``, together with all the effects in the original effectful
  object, and any effects in the return value of the ``func``.  The
  former effects are listed first.

``(effectful-list-bind effectfuls func)``
  Like ``effectful-bind``, but accepts a list of effectful objects,
  and calls the ``func`` on the list of their information contents.

There are two reasons why this matters to a user of the system.
First, callers of ``merge`` (for example recursive ones in contexts
where a new partial information structure is defined that may contain
arbitrary other ones) must be aware that ``merge`` may return an
``effectful`` object.  In this case, it is the resposibility of the
caller to ``merge`` to shepherd the effects appropriately, adjusting
them if necessary.  For example, the ``merge`` handler for two pairs
recursively merges the cars and cdrs of the pairs.  If either of those
recursive merges produces effects, the pair merge forwards all of
them.  Here is the code that does that::

  (define (pair-merge pair1 pair2)
    (effectful-bind (merge (car pair1) (car pair2))
      (lambda (car-answer)
        (effectful-bind (merge (cdr pair1) (cdr pair2))
 	  (lambda (cdr-answer)
 	    (cons car-answer cdr-answer))))))
 
  (defhandler merge pair-merge pair? pair?)

N.B.: The car merge and the cdr merge may both produce effects.  If
so, these effects will be executed in FIFO order, that is, car effects
first, then cdr effects.  This order is an arbitrary decision that we
as the designers of Scheme-Propagators are not committed to.  All
effects built into Scheme-Propagators are independent, in that their
executions commute.

Scheme-Propagators has two built-in effect types:
``cell-join-effect``, defined in ``core/cells.scm``, instructs the
system to make sure two cells are joined by synchronizing propagators;
``nogood-effect``, defined in ``core/contradictions.scm``, instructs
the system to record that a list of premises constitutes a nogood set.
(The error that the system signals when discovering a toplevel
contradiction is not an effect in this sense).


Second, a new partial information structure may want to have some
side-effect when merged.  This must be accomplished through returning
an appropriate ``effectful`` object containing appropriate
instructions.  New types of effects can be defined for that purpose.
For example, the built-in TMSes are added to the system through this
mechanism.

The handling of effects is extensible through two generic procedures.

``(execute-effect effect)``
  The ``execute-effect`` procedure is used by cells to actually
  execute any effects that reach the top level.  A handler for
  ``execute-effect`` should execute the effect specified by the given
  effect object.  The return value of ``execute-effect`` is not used.

``(redundant-effect? effect)  ==>  #t or #f``
  The ``redundant-effect?`` procedure is used to determine which
  effects will predictably have no effect if executed, so they may be
  removed.  For example, synchronizing a cell to itself, or
  synchronizing two cells that are already synchronized, are redundant
  effects.  Detecting redundant effects is important for testing
  network quiescence.

  The default operation of ``redundant-effect?`` is to return ``#f``
  for all effects, which is conservative but could lead to excess
  computation in the network.  A handler for ``redundant-effect?`` is
  expected to return ``#t`` if the effect will provably have no
  consequence on any values to be computed in the future, or ``#f`` if
  the effect may have consequences.

If an effect is generated by a ``merge`` that occurs in a contingent
context in a TMS, the TMS will modify the effect to incorporate the
contingency.  This mechanism is also extensible.  To teach TMSes
about making new effects contingent, add handlers to the generic
operation ``generic-attach-premises``.

``((generic-attach-premises effect) premises)  ==>  new-effect``
  The ``generic-attach-premises`` procedure is used by the TMS
  machinery to modify effects produced by merges of contingent
  information.  A handler for ``generic-attach-premises`` must return
  a procedure that will accept a list of premises and return a new
  effect, which represents the same action but appropriately
  contingent on those premises.  In particular, the consequences of
  the action must be properly undone or made irrelevant if any
  premises supporting that action are retracted.  For example, the
  instruction to join two cells by synchronizing propagators is made
  contingent on premises by causing those synchronizing propagators to
  synchronize contingently.


Individual Propagator Generics
----------------------------------------------------------------------

Most primitive propagators are actually built from generic Scheme functions.
Those propagators can therefore be extended to new
partial information types just by adding appropriate methods to their
generic operations.  This is what we did in the interval example.
Don't forget to teach the propagators what to do if they encounter
your partial information structure on one input and a different one on
another --- if both represent states of knowledge about compatible
ultimate values, it should be possible to produce a state of knowledge
about the results of the computation (though in extreme cases that
state of knowledge might be ``nothing``, implying no new information
produced by the propagator).

TODO Table of primitive propagators and their generic operations.

Uniform Applicative Extension of Propagators
----------------------------------------------------------------------

Also, most (TODO document which) primitive propagators are wrapped
with the ``nary-mapping`` wrapper function around their underlying
generic operation.  This wrapper function is an implementation of the
idea of applicative functors (TODO cite McBride and Paterson), so if
your partial information structure is an applicative functor, you can
use this to teach most propagators how to handle it.

``((binary-map info1 info2) f)  ==>  new-info``
The generic procedure ``binary-map`` encodes how to apply a strict
function to partial information arguments.  ``binary-map`` itself is
generic over the two information arguments, and is expected to return
a handler that will accept the desired function ``f`` and properly
apply it.  For example, consider contingent information.  A strict
operation on the underlying information that is actually contingent
should be applied by collecting the premises that both inputs are
contingent on, applying the function, and wrapping the result up in a
new contingency object that contains the result of the function
contingent upon the set-union of the premises from both inputs::

  (define (contingency-binary-map c1 c2)
    (lambda (f)
      (contingent
       (f (contingent-info c1) (contingent-info c2))
       (set-union (contingent-premises c1) (contingent-premises c2)))))

  (defhandler binary-map contingency-binary-map contingency? contingency?)

Note that the information inside a contingency object may itself be
partial, and so perhaps necessitate a recursive call to
``binary-map``.  This recursion is handled by the given function
``f``, and need not the invoked explicitly in handlers for
``binary-map``.

A handler for ``binary-map`` is expected to accept two partial
information structures and return a procedure of one argument that
will accept a binary function.  It is free to apply that function as
many or as few times as necessary, and is expected to produce the
appropriate result of "mapping" that function over the information in
the input partial information structures to produce a new partial
information structure, encoding all the appropriate uncertainty from
both inputs.

The ``nary-mapping`` wrapper works by repeated use of ``binary-map``
on arguments of arity greater than two.  For unary arguments,
``nary-mapping`` invokes ``binary-map`` with a bogus second argument.
Therefore, be sure to supply handlers to ``binary-map`` that handle
applications thereof that have your new partial information structure
as one argument, and a raw Scheme object as the other (this is a good
idea anyway, and saves the trouble of writing handlers for an explicit
``unary-map`` operation).

TODO Table of all primitive propagators that are affected by binary-map.

Interoperation with Existing Partial Information Types
----------------------------------------------------------------------

A new partial information structure may interact with an existing one
in two ways:

- as arguments to merge or to binary propagators
- by containment (of and by)

The first is in general handled by making sure that ``merge``,
``binary-map``, and all approriate individual propagator generic
operations have methods that can handle any combinations that may
arise.  Often, the way to deal with two information structures of
different but compatible types is to realize that one of them can be
seen as an instance of the other type.  The coercion machinery (which
see) allows one to declare when this situation obtains so that
``defhandler-coercing`` does the right thing.  The specific touch
points for this are the type testers and coercers of the existing
partial information types::

  | Type                | Predicate      | Coercer      |
  |---------------------+----------------+--------------|
  | Nothing             | nothing?       | --           |
  | Raw Scheme object   | various        | --           |
  | Numerical interval  | interval?      | ->interval   |
  | Scheme pairs        | pair?          | --           |
  | Propagator cells    | cell?          | --           |
  | Propagator closures | closure?       | --           |
  | Contingency object  | contingent?    | ->contingent |
  | TMS                 | tms?           | ->tms        |
  | Contradiction       | contradictory? | --           |

Notes:

- The ``nothing`` information structure defines methods on ``merge``
  and the propagators that do the right thing for any other object, so
  does not require any additional effort.

- TMSes automatically coerce to TMS any object that is declared
  coercible to a raw contingency object.

For example::

  (declare-coercion interval? ->contingent)

allows raw intervals to be seen as TMSes.  This has the effect that if
a binary operation (either ``merge`` or a primitive propagator subject
to ``nary-mapping``) encounter a TMS on one input and an interval on
the other, it will coerce the interval to a TMS containing exactly
that interval contingent on the empty set of premises, and then
operate on those two structures as on TMSes.


The second kind of interoperation is handled by correctly dealing with
merge effects (which see).  If you make a new partial information
structure that contains others, you must make sure to handle any merge
effects that may arise when recursively merging the partial
information your structure contains.  If you make a new partial
information structure that may need to have effects performed on
merge, you should return those as appropriate merge effects in an
``effectful`` structure, and, if you need to create new kinds of
effects in addition to the built-in ones, you should extend the
generic operations ``execute-effect``, ``redundant-effect?``, and
``generic-attach-premises``.


Making New Primitive Propagators
======================================================================

Direct Construction from Functions
----------------------------------------------------------------------

The fundamental way to make your own primitive propagators is
the procedure ``function->propagator-constructor``.  It takes a Scheme
function, and makes a propagator construction procedure out of it that
makes a propagator that does the job implemented by that Scheme
function.  The propagator constructor in question takes one more
argument than the original function, the extra argument being the cell
into which to write the output.  So the result of
``function->propagator-constructor`` is a diagram-style procedure
(complete with (most of) the debugging information, and the constant
conversion).  The return value of ``function->propagator-constructor``
can be put into a cell, just same way that a Scheme procedure
can be the value of a Scheme variable.  For example, you might define::

  (define-cell p:my-primitive (function->propagator-constructor do-it))

where ``do-it`` is the appropriate Scheme function.

Two things to pay attention to: ``function->propagator-constructor``
wraps the given function up into a propagator directly, and it is up
to the function itself to handle any interesting partial information
type that might come out of its argument cells.  Notably, ``nothing``
might show up in the arguments of that function when it is called.
Therefore, it may be appropriate the make the function itself generic,
and/or wrap it in ``nary-mapping``.  For example, check out how the
provided primitive ``p:and`` and ``p:or`` propagators are implemented,
in ``core/standard-propagators.scm``.

The second thing is metadata.  ``function->propagator-constructor``
can supply all the metadata that the debugger uses except the name for
your function.  That you need to add yourself, with ``(name!
your-function 'some-name)``.


Propagator Constructor Combinators
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Once you've made a diagram-style propagator constructor, you can make
a variant that likes to be applied in expression style with
``expression-style-variant``.  For example, ``e:and`` is actually
defined as::

  (define-cell e:and (expression-style-variant p:and))

You can also delay the actual construction of your primitives
if you want with ``delayed-propagator-constructor``, though that's
really more useful with recursive compound propagators.


DWIM Propagator Construction
----------------------------------------------------------------------

All that wrapping in ``nary-mapping``, and naming your propagator
functions with ``name!``, and calling ``expression-style-variant`` to
convert them to expression-style versions can get tedious.  This whole
shebang is automated by the ``propagatify`` macro::

  (propagatify + nary-mapping)

turns into

::

  (define generic-+ (make-generic-operator 2 '+ +))
  (define-cell p:+
   (function->propagator-constructor (nary-mapping generic-+)))
  (define-cell e:+ (expression-style-variant p:eq?))

Use this with some caution; you may not always want ``nary-mapping``,
and you may sometimes want to ``propagatify`` the raw Scheme function
instead of making a corresponding generic operator.  The macro is
defined in ``core/sugar.scm``; comments there describe it more fully.

Note that ``propagatify`` follows the naming convention that the
Scheme procedure ``foo`` becomes a generic procedure named
``generic-foo`` and then turns into propagators ``p:foo`` and
``e:foo``.

Compound Cell Carrier Construction
----------------------------------------------------------------------

``p:cons`` is an interesting propagator, because while it performs the
job of a Scheme procedure (to wit, ``cons``), it operates directly on
the cells that are its arguments, rather than on their contents.
This patterns is also abstracted:

``(function->cell-carrier-constructor f)``
  Is like ``function->propagator-constructor``, except it makes
  propagators that operate on the cells that are their arguments
  rather than on the partial information structures those cells
  contain.  ``p:cons`` could have been defined as::

    (define-cell p:cons (function->cell-carrier-constructor cons))

The rest of this machinery is in ``core/carrying-cells.scm``.

Defining ``p:cons`` to operate on its argument cells constitutes a
decision to follow the "carrying cells" rather than the "copying data"
strategy from the propagator thesis.

Fully-manual Low-level Propagator Construction
----------------------------------------------------------------------

Finally, when the thing you want your propagator to do is so low-level and
interesting that it doesn't even correspond to a Scheme function,
there's always the ``propagator`` procedure.  This is the lowest level
interface to asking cells to notify a propagator when they change.
``propagator`` expects a list of cells that your propagator is
interested in, and a thunk that implements the job that propagator is
supposed to do.  The scheduler will execute your thunk from time to
time --- the only promise is that it will run at least once after the
last time any cell in the supplied neighbor list gains any new
information.  For example::

  (define (my-hairy-thing cell1 cell2)
    (propagator (list cell1 cell2)
      (lambda ()
        do-something-presumably-with-cell1-and-cell2)))

The ``propagator`` procedure being the lowest possible level, it has
no access to any useful sources of metadata, so you will need to
provide yourself any metadata you want to be able to access later.
For an example of how this facility is used, see the implementations
of ``function->propagator-constructor`` and
``delayed-propagator-constructor`` in ``core/propagators.scm``.


Debugging
======================================================================

There is no stand-alone "propagator debugger"; if something goes
wrong, the underlying Scheme debugger is your friend.  Some effort
has, however, been expended on making your life easier.

In normal operation, Scheme-Propagators keeps track of some metadata
about the network that is running.  This metadata can be invaluable
for debugging propagator networks.  The specific data it tries to
track is:

- The names (non-unique but semantic) of all the cells and
  propagators.  This is in contrast with the unique but non-semantic
  object hashes of all the cells and propagators that MIT Scheme
  tracks anyway.

- Which propagators are connected to which cells.

- Whether the connections are input, output, or both.

- The grouping structure of the propagator network, as approximately
  defined by the call structure of the Scheme procedures that
  constructed it.

To make sure that your network tracks this metadata well, you should
use the high level interfaces to making cells, propagators, and
propagator constructors when possible (``define-cell``, ``let-cells``,
``define-propagator``, ``propagatify``, etc).  Any gaps not
filled by use of these interfaces must either be accepted as gaps or
be filled by hand.

Perhaps the most spectacular use of the metadata facility is to
draw pictures of your propagator network.  Just type::

  (draw:show-graph)

at the REPL and watch what happens!  If the picture does not look like
the graph you thought you made, make sure the connection metadata is
collected appropriately, but then check your code to see whether you
miswired something.  If the picture contains useless gibberish in the
labels, make sure the names of things are correctly assigned and
tracked.  If ``dot`` crashes, maybe your network is too big for it.
For more on various pictures you can draw, look in the source comments
in ``extensions/draw.scm``.

Of course, in order to use the metadata for debugging, you must be
able to read it.  Inspection procedures using the metadata are provided:

name
  the name of an object, should it have one

cell?
  whether something is a cell or not

propagator?
  whether something is a propagator or not

propagator-inputs
  the inputs of a propagator (a list of cells)

propagator-outputs
  the outputs of a propagator (a list of cells)

neighbors
  the readers of a cell (a list of propagators)

cell-non-readers
  other propagators somehow associated with a cell (presumably ones
  that write to it)

cell-connections
  all propagators around a cell (the append of the neighbors
  and the non-readers)

network-group-of
  a metadata object representing the context in which
  the object being examined was created (see ``core/metadata.scm``
  to learn what you can do with them)

You can use these at least somewhat to wander around a network you are
debugging.  Be advised that both cells and propagators are represented
directly as Scheme procedures, and therefore do not print very nicely
at the REPL.

If you find yourself doing something strange that circumvents the
usual metadata tracking mechanisms, you can add the desired metadata
yourself.  All the metadata collection procedures are defined in
``core/metadata.scm``; they generally use the ``eq-properties``
mechanism in ``support/eq-properties.scm`` to track the metadata, so
you can use it to add more.  In particular, see the definition of, say,
``function->propagator-constructor`` or ``define-propagator``
for examples of how this is done.


Miscellany
======================================================================

Implicit Cell Syntax
----------------------------------------------------------------------

A quirky little feature, called
``%%``.  This is a Scheme object, therefore Scheme-Propagators syntax,
for controlling the argument position of the implicit cell that an
expression-style application will make and return.  Perhaps examples
are best::

  (e: foo bar)     ==  (e: foo bar %%)

  (e: foo %% bar)  ==  (let-cell new (p: foo new bar) new)

I borrowed this idea from Guy Steele's PhD thesis on constraint
languages, and it was a year between when I implemented it and
when I first used it.  The use case I do have is when I
want to make a new cell participate in an input position
in a constraint with some existing cells::

  (define-cell x)
  (define-cell z)
  (define-cell y (ce:+ x %% z))
  (add-content x 5)
  (add-content y 3)
  (run)
  (content z) ==> 8

Perhaps this use case could also be served by adding more
expression-style constraint procedures (namely ``ce:-``, which I do
not currently have), but then again maybe it's elegant.

Reboots
----------------------------------------------------------------------

The procedure ``initialize-scheduler`` wipes out an existing
propagator network and lets you start afresh::

  build lots of network
  ...
  (initialize-scheduler)
  (run) --- nothing happens; no propagators to run!

Compiling
----------------------------------------------------------------------

It turns out that ``make-cell`` and ``cell?`` are also MIT Scheme
primitives, so if you want to compile your Scheme-Propagators
code, be sure to put

::

  (declare (usual-integrations make-cell cell?))

at the top of your source files.  Also, of course, you need to be
suitably careful to make sure that the defined macros are available to
the syntaxer when it processes your file.  See
``support/auto-compilation.scm`` for how I do this, and, say,
``core/load.scm`` for how I use the compiler.

Scmutils
----------------------------------------------------------------------

The Scmutils_ system built by Gerald Jay Sussman for thinking about
physics can be very useful for many purposes.  Among other things,
it knows about units and dimensions, about symbolic algebra,
about solving systems of equations, etc.  Scheme-Propagators runs
in Scmutils just as well as in MIT Scheme; and some of the unit
tests in the self-test suite rely on Scmutils.

.. _Scmutils: http://groups.csail.mit.edu/mac/users/gjs/6946/linux-install.htm

Editing
----------------------------------------------------------------------

I edit code in Emacs.  Emacs of course has a Scheme mode; nothing more
need be said about that here.

If you are going to edit any parenthesized source code in Emacs,
`Paredit mode`_ is a godsend.

.. _`Paredit mode`: http://www.emacswiki.org/emacs/ParEdit

In addition to the above, I find it very useful to have my editor
highlight and indent some of the Scheme-Propagators macros I have
defined the same way as their Scheme analogues; notably
``define-propagator`` and ``let-cells``.  Sadly the
Emacs Scheme mode does not do this by default, so you need to tweak
the Emacs config to do that.  The file ``support/scm-propagators.el``
contains a dump of the relevant portion of my Emacs configuration.

Hacking
----------------------------------------------------------------------

Scheme-Propagators is a work in progress.  Be aware that I
will continue to hack it to my heart's content.  Likewise, feel free
to hack it to yours --- let me know if you invent or implement
something interesting.

TODO Describe where in the source various constructs are defined?  So that
it is possible to mimic them (e.g. more primitive propagators) and/or
adapt them.

Examples
----------------------------------------------------------------------

There are some basic examples in ``core/example-networks.scm``; more
elaborate examples are available in ``examples/``.

Arbitrary Choices
----------------------------------------------------------------------

Several language design choices affecting the structure of
Scheme-Propagators appeared arbitrary at the time they were made.
First, diagram style application was picked as the default over
expression style when applying cells whose contents are not yet known,
and for defining compound propagators when the style is not specified
more clearly.  The main rationale for this decision was that choosing
expression style as the default would have made Scheme-Propagators too
much like every other programming language; the unusual expressive
power of fanin that the propagator model offers can be taken advantage
of only if at least some of one's code actually has fanin, and writing
code with fanin requires diagram style.  So we chose to make diagram
style the default, to emphasize that potential.

Second, there was a choice about where to put the delaying of pieces
of propagator network that should be constructed only conditionally.
Every recursion traverses an abstraction boundary and a conditional
statement every time it goes around.  Every recursion must encounter
at least one delay barrier every time it goes around, or the
construction of the network may generate spurious infinite regresses.
But where should that barrier go?  There were three plausible
alterntives: the first idea was to put the barrier around the
application of recursive compound propagators; the second was to
generalize this to put it around the application of all compound
propagators; and the third was to capture the bodies of conditional
expressions like ``p:if`` and delay only their construction.  During
most of the development of Scheme-Propagators, we were using option 1,
on the grounds that it sufficed and was easy to implement.  Doing this
had the effect that in order to actually make a proper recursive
propagator, one had to manually "guard", using a hand-crafted pile of
``switch`` propagators, all the i/o of a recursive call to prevent it
from being expanded prematurely.  For example, a recursive factorial
network written in that style would have looked something like::

  (define-propagator (p:factorial n n!)
    (let-cells ((done? (e:= n 0)) n-again n!-again)
      (p:conditional-wire (e:not done?) n n-again)
      (p:conditional-wire (e:not done?) n! n!-again)
      (p:* (e:factorial (e:- n-again 1)) n-again n!-again)
      (p:conditional-wire done? 1 n!)))

with the added caveat that it would need to be marked as being
recursive, so the expansion of the internal factorial would be delayed
until it got some information on its boundary (which would be
prevented from happening in the base case by the ``conditional-wire``
propagators).  Very late in the game we finally decided to write a
series of macros (``p:when``, ``p:unless``, ``p:if``, and their
expression-style variants) that automated the process of constructing
those ``conditional-wire`` propagators.  On making these macros work,
we realized that adjusting ``p:when`` and company to delay their
interior would be just as easy as delaying the opening of
abstractions.  At that point we decided to switch to doing it that
way, on the grounds that ``if`` is special in all other computer
languages, so it might as well be special here too, and we will leave
the operation of abstractions relatively simple.  (Partial information
makes abstractions complicated enough as it is!)  This has the further
nice feature that it sidesteps a possible bug with delayed
abstractions: that being that if one wanted to create a nullary
abstraction, automatic delay of its expansion would presumably not be
what one wanted.

Third, the decision to go with the carrying cells strategy for
compound data felt, while not really arbitrary, at least enough not
forced by the rest of the design to be worth some mention.  The topic
is discussed at length elsewhere, and the available options detailed;
so here we will just note why we ended up choosing carrying cells.
For a long time, copying data seemed like the right choice, because it
avoided spooky "action at a distance"; and merges did not require
changing the structure of the network.  The downside of copying data,
namely the cost of the copying, seemed small enough to ignore.  Then
we tried to write a program for thinking about electrical circuits.

The specific killer part of the electrical circuits program was that
we tried to equip it with observers that built a data structure for
every circuit element containing its various parameters and state
variables, and for every subcircuit a data structure containing its
circuit elements, all the way up.  When this program turned out to be
horribly slow, we realized that copying data actually produces a
quadratic amount of work: every time any circuit variable is updated,
the whole chain of communication all the way from resistor to complete
breadboard is activated, and they repeat merges of all the compounds
that they had accumulated, just to push that one little piece of
information all the way to the toplevel observer.  In addition, these
summary structures turned out to be less useful for debugging than we
had hoped, because the updates of the summary structures would be
propagator operations just like the main computation, so when the
latter would stop for some strange reason, we always had to wonder
whether the summaries were up to date.

Carrying cells seemed an appealing solution to both problems.  If the
summaries carried cells instead of copying data, then updates to those
cells would not have to trouble the whole pipe by which the cells were
carried, but would just be transmitted through those cells.  Also, if
we played our cards right, we should have been able to arrange for
exactly the cells where the computation was actually happening to be
the ones carried all the way to where we could get them from those
summary structures, so that the summaries would always be up to date
with the underlying computation.  But what about the pesky fact that
merging structures that carry cells requires side effects on the
network?  What if that merge is contingent on some premises because
the cell-carriers are in some TMS?

That was when merge effects were invented.  We realized that merging
really should have legitimate side effects on the network, but should
package those effects up in manipulable objects that it returns,
instead of trying to just execute them.  So the question that merge
answers was changed from

  What is the least-commitment information structure that captures
  all the knowledge in these two information structures?

to 

  What needs to be done to the network in order to make it reflect the
  discovery that these two information structures are about the same
  object?

The latter nicely subsumes the former: a normal merge is just the
answer "record in the appropriate cell that the object of interest is
described by this information structure".  So everything fell into
place.  The strange ``set!`` in the most basic definition of the cell
is, indeed, an effect that needs to be performed on the network to
acknowledge the discovery that two particular information structures
are about the same object.  The even stranger error signalled on
contradiction is an effect too: the thing that needs to be done to the
network to reflect the discovery that two completely incompatible
information structures describe the same object is to crash.  And now
both merging cells carried by compound structures and signalling
nogoods by TMSes become perfectly reasonable, respectable citizens of
the propagator world; and they can interoperate with being contingent
by the enclosing TMS modifying the effects to reflect the context in
which they were generated before passing them on up out of its own
call to merge.

With that change of perspective on merging, a whole chunk of problems
suddenly collapsed.  Cells could be merged with a simple "link these
two with (conditional) identity propagators".  Therefore compound data
could be merged by recursively merging their fields, regardless of
whether they were carrying cells or other partial information
structures.  Closures fell into place --- they were just a particular
kind of compound data, and merged the way compound data merges.
Closures had been a conceptual problem for the copying data view of
the world, because closures really felt like they wanted to able to
attach their interior propagators to cells closed over from the
enclosing lexical environment; but for that, it seemed that the
lexical environment would need to be a cell-carrying data structure.
But now that carrying cells works, there is no problem.  It was on
that wave of euphoria that the carrying cells strategy rode into its
current place as the standard way to make compound structures in the
propagator world.  Carrying cells certainly still feels cleaner and
nicer than copying data; but it may be that copying data really could
still be made to work in all the scenarios where carrying cells is
currently winning.  We just decided not to pursue that path.

And on the note of copying data being preferable because it preserves
locality, maybe ``cons`` really should be the locality-breaking object.

Revision History of this Guide
----------------------------------------------------------------------

First written May 5, 2010 by Alexey Radul
Rewritten August 17, 2010 by Alexey Radul and Gerald Jay Sussman

How this supports the goal
======================================================================

TODO
