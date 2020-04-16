Files that don't matter:
activity_log.txt : activity log, not updated since before spring break
stnu.txt: morris 2006 pseudocode from course website
>> anything in a sub-directory <<

Files that aren't mine:
dc-fixnum-arrays.lisp
dc-inf-arith.lisp
params.lisp
vectors.lisp

Files that I changed:
new-new-stnu: copied form of new-stnu.lisp. updated nu-stnu class.

Test files:
sample.stnu: small example from course website
sample2.stnu: magic loop from class slides

Files that matter:
parser.lisp: parses .stnu file into NU-STNU instance
morris2006.lisp: implements morris' 2006 dc-checking algorithm
all.lisp: wraps other files, runs morris 2006 implementation on sample2.stnu

How to run:
open aclemacs in terminal
M-x fi:common-lisp
[enter] [enter] [enter] [enter] [enter] [enter] [enter]
at CL-USER(2): prompt, enter (load "all.lisp")

Results of tests go to output.txt

Note:
There are print statements commented out throughout the IS-DC function in
Morris2006.lisp. Uncomment to watch how the queuing procedure in my
implementation creates an infinite loop.
I tried really hard but I can't fix it.
