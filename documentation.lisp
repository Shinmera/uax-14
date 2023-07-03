(in-package #:org.shirakumo.alloy.uax-14)

(docs:define-docs
  (variable *line-break-database-file*
    "Variable containing the absolute path of the line break database file.

See LOAD-DATABASES
See COMPILE-DATABASES")
  
  (variable *pair-table-file*
    "Variable containing the absolute path of the pair table file.

See LOAD-DATABASES
See COMPILE-DATABASES")
  
  (type no-database-files
    "Warning signalled when LOAD-DATABASES is called and the files are not present.

Two restarts must be active when this condition is signalled:

  COMPILE --- Call COMPILE-DATABASES
  ABORT   --- Abort loading the databases, leaving them at their
              previous state.

See LOAD-DATABASES")
  
  (function load-databases
    "Loads the databases from their files into memory.

If one of the files is missing, a warning of type NO-DATABASE-FILES is
signalled. If the loading succeeds, T is returned.

See *LINE-BREAK-DATABASE-FILE*
See *PAIR-TABLE-FILE*
See NO-DATABASE-FILES")
  
  (function compile-databases
    "Compiles the database files from their sources.

This will load an optional part of the system and compile the database
files to an efficient byte representation. If the compilation is
successful, LOAD-DATABASES is called automatically.

See *LINE-BREAK-DATABASE-FILE*
See *PAIR-TABLE-FILE*
See LOAD-DATABASES")

  (type breaker
    "Contains line breaking state.

An instance of this is only useful for passing to MAKE-BREAKER and
NEXT-BREAK. It contains internal state that manages the line breaking
algorithm.

See MAKE-BREAKER
See NEXT-BREAK")
  
  (function make-breaker
    "Returns a breaker that can find line break opportunities in the given string.

If the optional breaker argument is supplied, the supplied breaker is
modified and reset to work with the new string instead. This allows
you to re-use a breaker.

Note that while you may pass a non-simple string, modifying this
string without resetting any breaker using it will result in undefined
behaviour.

See BREAKER")
  
  (function next-break
    "Returns the next line breaking opportunity of the breaker, if any.

Returns two values:

  POSITION  --- The character index in the string at which the break
                is located, or NIL if no further breaks are possible.
  MANDATORY --- Whether the break must be made at this location.

Note that there is always in the very least one break opportunity,
namely at the end of the string. However, after consuming this break
opportunity, NEXT-BREAK will return NIL.

Note that you may have to insert additional line breaks as required by
the layout constraints.

See BREAKER")
  
  (function list-breaks
    "Returns a list of all line break opportunities in the string.

The list has the following form:

  LIST  ::= ENTRY+
  ENTRY ::= (position mandatory)

This is equivalent to constructing a breaker and collecting the values
of NEXT-BREAK in a loop.

See MAKE-BREAKER
See NEXT-BREAK")
  
  (function break-string
    "Returns a list of all the pieces of the string, broken.

If MANDATORY-ONLY is T, the string is only split at mandatory line
break opportunities, otherwise it is split at every opportunity.

See MAKE-BREAKER
See NEXT-BREAK"))
