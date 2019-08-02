## Comments

CRAN re-submission - I have addressed the comments from the previous submission as follows:

"Please write package names, software names and API names in
single quotes (e.g. 'Distance') in your Description."

* Added single quotes around software name.

"Please replace \dontrun{} by \donttest{} in your Rd-files."

* Replaced \dontrun{} with \donttest{}

"Please replace cat() by message() or warning() in your functions (except for print() and summary() functions)."

* Replaced cat() used for progress counter with message(). All other cat() usage is in show functions for S4 classes.

## Test environments
* local OS X install, R 3.6.1
* win-builder (devel, release and oldrelease)

## R CMD check results
There were no ERRORs or WARNINGs. 

There was 1 NOTE:

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Laura Marshall <lhm@st-and.ac.uk>'

New submission
