
README file for BT Analyser - a prototype LTL model checker
for Behavior Trees

Version 1.2, 19 May, 2015.

Author: Sentot Kromodimoeljo


==================== Installation (Compiling BT Analyser) ====================


----- Lisp platforms -----

BT Analyser has been tested on SBCL (the preferred Lisp platform)
with Linux, Windows and MacOS X operating systems; GCL in ANSI mode;
and CCL (Clozure Common Lisp). It ought to run on any ANSI Common
Lisp platform (may need conditionalized code for abort handling and
for sockets to be added to lexicon.lisp), although ABCL would be too
slow. To be able to handle large problems, a 64-bit platform is
recommended (a 32-bit platform limits the address space to 4GByte).

SBCL for various OS platforms can be downloaded from
www.sbcl.org/platform-table.html (accessed 27-AUG-2014).

GCL for various OS platforms are available. The homepage for
GCL is www.gnu.org/software/gcl/ (accessed 27-AUG-2014).
Click on "Get Latest Release".

Alternatively, on Linux platforms, GCL can be installed from
the software centre. However, the GCL installed may be
CLtL1 which is not ANSI Common Lisp. To make it ANSI Common Lisp,
in a Debian-based distribution, the file /etc/default/gcl must
be modified to have the following two lines:
DEFAULT_GCL_ANSI="yes"
DEFAULT_GCL_PROF="yes"

Clozure Common Lisp (CCL) for various OS platforms can be downloaded
from ccl.clozure.com/download.html (accessed 27-AUG-2014).
Note that CCL does not pretty-print s-expressions, thus the
output can be difficult to read.

IMPORTANT NOTE

Without changes, the working directory when running Lisp to load
and compile BT Analyser is assumed to be the main directory of
BT Analyser (where the source files reside}. This can usually be
ensured by running Lisp from the source directory of the prototype.
Otherwise you may need to set it via the Lisp variable
*default-pathname-defaults*, for example

  (setf *default-pathname-defaults* (truename "/home/username/BT-Analyser/"))

assuming the main directory is /home/username/BT-Analyser/.
An example using Windows would be

  (setf *default-pathname-defaults* (truename "C:/Users/username/BT-Analyser/"))

Alternatively, the line

  (defvar *source-directory* "./")

in files compile-lexicon.lisp, compile-all.lisp and load-all.lisp
can be modified, e.g., in Windows, to

  (defvar *source-directory* "C:/Users/username/BT-Analyser/")

It would be convenient for subsequent usage of BT-Analyser to
modify the line for load-all.lisp.


----- Compilation -----

A subdirectory for object files must be created. Its name must
correspond to the Lisp plaform, e.g., SBCL, GCL, or CCL (see
files compile-lexicon.lisp, compile-all.lisp and load-all.lisp).
For other Lisp platforms, the files compile-lexicon.lisp,
compile-all.lisp and load-all.lisp must also be modified
accordingly.

The file lexicon.lisp must be compiled first and separately.
Run an ANSI Lisp platform and enter the following at the
read-eval prompt:

  (load "compile-lexicon.lisp")

After the compilation is finished, exit Lisp, e.g., using

  (exit)

in SBCL. The exit command may be different in different Lisp
platforms, e.g., (quit) or the CONTROL-D key.

Once lexicon.lisp has been compiled, the rest of the prototype
can be compiled. Again run the ANSI Lisp. This time enter the
following at the prompt:

  (load "compile-all.lisp")

Exit Lisp after the compilation finishes.


==================== Quick User's Guide ====================


----- Loading -----

After the prototype has been compiled (compilation need only be
performed once), the prototype can be run. At the ANSI Common Lisp
prompt, enter the following:

  (load "load-all.lisp")

  (in-package "bt")

For some ANSI Common Lisp platforms, the Lisp system can be run
with various options. For SBCL, the following command line invocation:

  sbcl --dynamic-space-size 16384

will run SBCL with a virtual memory size of 16GB.


----- BT Source File -----

The prototype accepts BT source files for TextBE and ComBE
(i.e., source files with the TextBT syntax).
Currently, no operators are allowed in expressions.

----- A Simple Usage Session -----

Assuming "examples/model2_120119.bt" refers to an example TextBT file
that is included in the distribution, at the ANSI Common Lisp prompt,
the following commands are entered.

  (load "load-all.lisp")

  (in-package "bt")

  (process-bt-file "examples/model2_120119.bt")

  (ltl-check '(g (not (and (not (= |SV-Calculator| |VAL-calculateDS|))
                           (= |SV-MetData| |VAL-pending|)
                           (x (= |SV-Calculator| |VAL-calculateDS|))))))

  (find-counterexample-path)

  (print-history)

  (print-event 5)

The above sequence of commands produces 5 events.
- process-bt-file produces an event that corresponds to the BT model.
- ltl-check produces 3 events:
  - an event for the set of reachable states
  - an event for the LTL specification (which produces a tableau)
  - an event for the set of reachable fair (with respect to the
    specification) states without any global constraint
- find-counterexample-path produces an event that corresponds to
  a counterexample path.

The command print-history prints a summary of the sequence of events
produced.

The command print-event prints an event in detail. In the above case,
it prints the 5th event which corresponds to the counterexample path.


==================== LTL Syntax ====================


The prototype uses S-expressions for LTL formulas.
The following table shows the syntax for all the operations.


Conventional syntax     S-expression syntax     Note

a = b                   (= a b)
not p                   (not p)
p and q                 (and p q)               can have >2 arguments
p or q                  (or p q)                can have >2 arguments
p implies q             (implies p q)
X p                     (x p)                   next time
F p                     (f p)                   eventually
G p                     (g p)                   globally
p U q                   (u p q)                 strong until
p R r                   (r p q)                 release

A note for those not familiar with Lisp: an "unevaluated" S-expression
is quoted using a single quote (') in front, e.g., '(not p).
General Lisp expressions involving literals, variables, function
applications, quotes, backquotes and commas can be used to build an
S-expression. However, the prototype runs in the "bt" package in which
not all symbols from the standard package in ANSI Common Lisp are
imported.

Also note that a symbol not enclosed in || is case-insensitive
and treated as all uppercase.

BT component names are prefixed with SV-. As an example, the BT
component name Calculator must be input as |SV-Calculator| in the
S-expression syntax (the symbol must be enclosed in || since it is
mixed-case).

BT component attribute names are appended to BT component names,
separated by a dot (.). As an example, the attribute Type of
the component DMP appears as |SV-DMP.Type| in the S-expression
syntax.

BT values are prefixed with VAL-. As an example, the BT values
pending appears as |VAL-pending| in the S-expression syntax.
Note that BT values are overloaded. A |VAL-pending| value for a
component or attribute is different from a |VAL-pending| value
for a different component or attribute.

There is a well-formedness requirement for equalities.
In the equality (= LEFT RIGHT), LEFT must be either a component
name, an attribute name or a PC name (e.g., PC2); and RIGHT
must be an appropriate BT value if LEFT is a component name or an
attribute name, or RIGHT must be an integer if LEFT is a PC name.


==================== Commands ====================


The prototype allows analysis to be performed in an incremental
fashion. Of course analysis can be performed only if there is a
model. For the prototype, a model is specified by a TextBT file
(a file containing text that specifies a Behavior Tree in
TextBT syntax, as used by the TextBE and ComBE tools). A model
is loaded using the process-bt-file command. Its syntax is

  (process-bt-file filename-string)

We saw its usage in the above simple session example.

  (process-bt-file "examples/model2_120119.bt")

Most ANSI Common Lisp platforms allow forward slashes in
file specifiers even if the OS platform is Microsoft Windows.

If the loading is successful, an event for a model will appear in
the history of the session. There can be at most one model event
in the history. Any model previously loaded, along with its
analysis, will be removed.

Relative pathnames can be convenient when using the prototype.
The set-working-directory can be used to change the working
directory. Its syntax is

  (set-working-directory filename-string)

An example use:

  (set-working-directory "/home/user/project/bt/")


-----

There are several types of analysis supported by the prototype.
Some of them require the presence of an LTL specification.
The primitive command for entering an LTL specification is
ltl-specification (a primitive command generates at most one
event in the history). Its syntax is

  (ltl-specification ltl-formula)

As an example, the specification for the above simple session could have
been entered using the command:

  (ltl-specification '(g (not (and (not (= |SV-Calculator| |VAL-calculateDS|))
                                   (= |SV-MetData| |VAL-pending|)
                                  (x (= |SV-Calculator| |VAL-calculateDS|))))))

If the command is successful, an event for the LTL specification will appear
in the history.

The set of reachable states may be computed first before
an augmented model for the LTL specification is constructed
(the default if the BT translation uses prioritisation),
and the augmented model uses the reachability information.
Thus the command may produce two events.

-----

An analysis in the prototype that does not require an LTL 
specification is computing general reachability (reachability
from an initial state). The BT translator initialises only the
PCs (the PC of the main thread is set to 1 while other PCs are
set to 0); other state variables are not initialised and each
can have any value of the appropriate type. Thus the set of
initial states is characterised by an assignment of values to
PCs.

The primitive command for computing general reachability is
reachable-states. Its syntax is

  (reachable-states)

If the command is successful, an event for reachable states will
appear in the history. There can be at most one reachable states
event in the history, thus if the set of reachable states for
the model has been previously computed, the command has no effect.

The set of reachable states can be used to determine potential
deadlocks (assuming the modelled system is non-terminating).
The command for showing deadlocked states is print-deadlocked-states.
Its syntax is

  (print-deadlocked-states)

If the set of reachable states has not been computed, then
it is computed first and an event for reachable states placed
in the history.

-----

An analysis which is at the heart of symbolic model checking
as proposed by McMillan is the computation of "fair" states.
The command for computing a set of fair states is fair-states.
Its syntax is

  (fair-states &optional gc)

The optional parameter gc is for a global constraint, which
must be a state formula (an LTL formula with no temporal operation).
The global constraint must be satisfied by all states in the set.
Having no global constraint is equivalent to having the global
constraint true.

Examples:

  (fair-states)

  (fair-states '(not (= PC2 3)))

The computation of fair states requires the presence of an LTL
specification. If there is no LTL specification, the command has
no effects. Depending on the setting of the options (to be
described later), the set of reachable states may be computed first,
thus the command may generate multiple events.

There can be more than one event for fair states, but they must
have different global constraints.

There is a command theorem-is-proved which checks if a set of
fair states can give rise to counterexamples without computing
the counterexamples. Its syntax is

  (theorem-is-proved & optional gc)

It checks the appropriate set of fair states and computes the set
if it has not been computed. It returns T if there can be no
counterexample path (with the global constraint) and returns
NIL otherwise.

There is also a macro command ltl-check:

  (ltl-check ltl-formula)

which expands to

  (ltl-specification ltl-formula)
  (fair-states)

-----

In some cases it may be desirable to compute the set of
counterexample states (states that can be in counterexample paths).
The command for computing a set of counterexample states is
counterexample-states. Its syntax is

  (counterexample-states &optional gc)

The command requires the appropriate set of fair states and
computes the set of fair states first if it has not been computed.

Computing a set of counterexample states may reduce the size of
the search space for counterexample path generation.
However, the computation can be expensive.

-----

The preferred method of generating a counterexample path is
using the find-counterexample-path command. The syntax for the
command is

  (find-counterexample-path &optional label cc gc)

The optional label argument is a string specifying the
requirement label of a BT node. The cycle part of the counterexample
path must pass through the BT node.
The optional cc argument is a cycle constraint (a state formula)
that must be fulfilled in the cycle part of the counterexample path.
The optional gc argument is a global constraint (a state formula)
that must be satisfied by all states in the counterexample path.

Examples:

  (find-counterexample-path)

  (find-counterexample-path "9.5")

  (find-counterexample-path "9.5" nil '(not (= PC2 3)))

If successful, the command returns T. Otherwise it returns NIL.


-----

An alternative for generating counterexample paths without the
need to compute sets of fair states is on-the-fly model checking.
The main command for on-the-fly model checking is
on-the-fly-ltl-check. Its syntax is

  (on-the-fly-ltl-check ltl-formula &optional gc)

The parameter ltl-formula is an LTL specification.
If ltl-formula is not the same as the current LTL specification,
then the current specification and its analysis are removed
and ltl-formula becomes the new specification
The command either produces a counterexample path or returns NIL
if there is no counterexample path.

On-the-fly model checking can be very fast in producing counterexample
paths. However, the paths produced tend to be too long. In addition,
if there is no counterexample path, it can run for a very long time.

Using the on-the-fly-ltl-check-mc command, on-the-fly model
checking can operate on fair states. The syntax of the command
is

  (on-the-fly-ltl-check-mc ltl-formula &optional gc)

On-the-fly model checking can also be combined with directed
counterexample generation (with the latter used as a post-processor),
using the combo ltl-check command. The syntax of the command is

  (combo-ltl-check ltl-formula &optional gc)


==================== Options ====================


Various options can be set for the prototype. The parameters
for the options can be changed in the middle of a session
(the changes may cause some results to be removed).

-----

There are two parameters associated with the construction
of a model from a BT source file:
  - *prioritizing* (with possible values T or NIL), which specifies
    whether BT internal transitions are to be prioritized over BT
    external events. A value of T means internal transitions are
    prioritized. The default value for this parameter is T.
  - *goto-semantics* (with possible values T or NIL), which
    specifies whether references to BT nodes in the same thread
    are to be treated as "gotos". The default value for this
    parameter is T.

Changes to *prioritizing* must be done through the set-priority
command. It has the following syntax:

  (set-priority &optional value)

The optional value must be T or NIL.

Changes to *goto-semantics* must be done through the set-goto-semantics
command. It has the following syntax:

  (set-goto-semantics &optional value)

The optional value must be T or NIL.

-----

There is a parameter *encoding-scheme* associated with the LTL
encoding scheme used. The possible values for *encoding-scheme*
are:
  CGH (the classis LTL encoding scheme), GBA, GBA-LOOSE, RGBA,
  RGBA-LOOSE, TGBA, TGBA-LOOSE, RTGBA or RTGBA-LOOSE.
The default value for *encoding-scheme* is TGBA-LOOSE.
The command for changing the value of the *encoding-scheme*
parameter is set-encoding-scheme. Its syntax is

  (set-encoding-scheme &optional scheme)

where the optional scheme argument must be one of the values
listed above. If scheme is not specified, then it defaults
to TGBA-LOOSE.

Examples:

  (set-encoding-scheme 'cgh)

  (set-encoding-scheme)

  (set-encoding-scheme 'tgba-loose)

If the value of *encoding-scheme* is changed and an LTL
specification exists, then the tableau for the specification
is modified and all results of analysis after the
LTL specification are discarded.


==================== Printing Events ====================


The print-history command can be used to print the history:

  (print-history)

A summary of each event in the history is printed.
Each event is indexed using an integer: 1 for the first
event in the history, 2 for the second event, etc.

-----

To get more detailed information on an event, the
print-event command can be used. Its syntax is

  (print-event index)

where index is an integer index.

Example:

  (print-event 1)

For most users, the model event, the LTL specification
event and counterexample path events can be of interest.
Other events may contain messy detailed information
that are of interest to advanced users only
(and equalities may be presented in the form of their encodings).

To print the latest event, the command print-last-event can be
used:

  (print-last-event)


==================== Undoing Events ====================


The command for undoing the last event is undo-event:

  (undo-event)

The command to undo to event indexed by i (not including event i,
thus event i becomes the last event) is undo-back-to-event:

  (undo-back-to-event i)

If event i is also to be undone, the command is undo-back-through-event:

  (undo-back-through-event i)


==================== Test Path Generation ====================


It is recommended that reachability analysis be performed before
test paths are generated (after the BT file is loaded):

  (reachable-states)

---

The main command for generating test paths is find-test-paths:

  (find-test-paths source intermediates target blocks)

where source and target are elementary block indices for the
starting and ending transitions respectively, intermediates is
a list of block indices for the "nodes of interest" and blocks is
a list of block indices for the "system states".

The command produces a list of test paths. Each test path is a list
of elementary block indices representing transitions in the path.

---

A generated test path may not be feasible. To check the feasibility
of a test path, use the function check-test-path:

  (check-test-path path)

The call returns t if path is feasible and nil otherwise.

---

There are utility functions to help map BT nodes to elementary
block indices. One such function is indices-of-update-blocks:

  (indices-of-update-blocks component-string &optional behaviour-string)

An example usage:

  (indices-of-update-blocks "SSM" "Ok")

which produces a list of indices for elementary blocks that correspond
to the state realisations "[Ok]" for the "SSM" component.

Similarly, there is indices-of-event-blocks for (external) events:

  (indices-of-event-blocks component-string &optional behaviour-string)

An example usage:

  (indices-of-event-blocks "Filter" "invalid(s1)")

---

There is a function for computing the precondition for a path
called test-path-precondition:

  (test-path-precondition path)

It produces a symbolic formula (in s-expression form) for
the precondition. If the path is not feasible, the precondition
is (FALSE).

---

There is a function for computing the preamble for a test path:

  (test-path-preamble start path)

where start is the index for the "starting" block and path is the test path.

Similary, there is a function for computing the postamble for a test path,
which is a path from the end of the test path back to the "starting" block:

  (test-path-postamble start path)

---

A test path can be printed in a form suitable for humans, using
the print-test-path-summary:

  (print-test-path-summary path)


==================== SSL Socket Server ====================


BT Analyser can be run as a server using SSL sockets with
with SBCL, GCL and CCL. For other ANSI Lisp platforms, the
code for SSL sockets need to be added to "lexicon.lisp".

The function to run BT Analyser as a socket server is

  (socket-server-mode &optional port)

where an optional port number can be specified (defaults to 12).

Responses from the server to the client is in XML format.
The file "api.txt" describes the XML formats, and is intended
to describe an API for test path generation purposes.


For testing purposes, a simple client is provided (must be invoked
from a separate process). The function to invoke is:

  (socket-client-mode &optional port host)

Commands are issued from the client without quoting (except for
string quotes).
