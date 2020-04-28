
Directories:
hunscode: original starter code files - not used
junk: files that don't matter
lib: modified starter code files - used in Morris2006
sample_stnus: .stnu files for testing
test_output: output.txt contains output from ./2006/all.lisp tests
2006:
  - parser.lisp: parses .stnu file into stnu instance
  - morris2006-min-heap.lisp: implements morris's 2006 dc-checking algorithm
  - all.lisp: wraps all 2006 files. contains all tests for morris 2006.
2014:
  - parser.lisp: parses .stnu file into stnu instance (updated for 2014)
  - sandbox.lisp: contains stnu class (updated for 2014)
  - morris2014.lisp: contains a poorly commented but functioning implementation
    of Morris's 2014 dc-checking algorithm!
  - all.lisp: wraps relevant 2014 files. parses all .stnu files in the
    sample_stnus directory and prints results
    (along with a bunch of debugging stuff)


How to run:
go to 2014 directory, load "all.lisp" in aclemacs

Stuff to look at:
- The last lines printed by all.lisp are results for each sample .stnu file
  Tentatively, they are all correct.
- Is there a better way to check if an edge is unsuitable? (see function
  "unsuitable" in morris2014.lisp)

Note:
My plan for the next couple of days is to clean up the comments in 2006, add
comments to 2014, and move elements of the 2014 files around in a way that
makes more sense (renaming sandbox.lisp). I would also like to make a new print
function for my updated stnu class and print test output to a file like 2006.
