Directories:
lib: modified starter code files - used in Morris2006
sample_stnus: .stnu files for testing
test_output:
  - output.txt contains output from ./2006/all.lisp small tests
  - big-test-output2006.txt contains output from ./2006/all/lisp 200 node tests
  - small-test-output2014.txt contains output from ./2014/all.lisp small tests
  - big-test-output2014.txt contains output from ./2014/all/lisp big tests
2006:
  - parser.lisp: parses .stnu file into stnu instance
  - morris2006-min-heap.lisp: implements morris's 2006 dc-checking algorithm
  - all.lisp: wraps all 2006 files. contains all tests for morris 2006.
2014:
  - parser.lisp: parses .stnu file into stnu instance (updated for 2014)
  - new-stnu-2014.lisp: contains nu-stnu class (updated for 2014)
  - morris2014.lisp: implements Morris's 2014 dc-checking algorithm
  - all.lisp: wraps relevant 2014 files. contains all tests for morris 2014

How to run:
go to 2014 directory, load "all.lisp" in aclemacs
go to 2014 directory, load "all.lisp" in aclemacs
** 200 node tests for 2006 are commented out because they take forever **

Stuff to look at:
- Why do my 2006 tests take several hours to run???
- Why are 2 test cases in 2006 failing?
- What does this mean? ->
"8480248 bytes have been tenured, next gc will be global.
See the documentation for variable *GLOBAL-GC-BEHAVIOR* for more information."
- I didn't try with the 400 node test stnus.

Notes on the class:
- Not having deadlines was convenient but ultimately unhelpful.
- I need deadlines to function.
- I liked turning in whatever we had on Fridays. You should do that next time.
- Giving updates in class was good. Social pressure to perform is very motivating.
- * I probably wouldn't think that if I hadn't gotten mine to work as well *
- Debugging Morris 2006 would have been much much easier if we could have
walked though an example from start to finish with all of the edges that get
added and in what order (not just the solution edges). Think 241 ~becoming the
algorithm~ style. I can print out what my program is doing, but that's only
helpful if I know exactly what should be happening (beyond just a conceptual
understanding)
