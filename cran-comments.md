## Comments

CRAN re-submission - I have addressed the comments from the 1st submission as follows:

"Please rephrase the second sentence of the description."

* Second sentence of the description has been rephrased 

If there are references describing the methods in your package, please
add these in the description field of your DESCRIPTION file in the form

* I have added a reference to the description section of the DESCRIPTION file, I have also added a URL to our project website.

"Some code lines in examples are commented out."

* write.transects.Rd example has been uncommented and now saves to tempdir(). 

"Please only write/save files if the user has specified a directory in
the function themselves."

* I have double checked and write.transects will only write to file if the user specifies a data soure name. If the user does not supply a data source name an error is generated.

* run.coverage also only saves files if a directory is specified by the user. (No other functions write/save files.)

"Please unwrap the examples if they are executable in < 5 sec, or create
additionally small toy examples to allow automatic testing."

* The examples enclosed in \dontrun take > 5 seconds to run. I added comments to the user to explain why this code is inside don't run and I also added toy examples at the end of the examples section in the documentation with warnings to the user about the interpretability of these examples. I have also manually run all the examples inside the dontrun sections and can confirm that they run.

## Test environments
* local OS X install, R 3.6.1
* win-builder (devel, release and oldrelease)

## R CMD check results
There were no ERRORs or WARNINGs. 

There was 1 NOTE:

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Laura Marshall <lhm@st-and.ac.uk>'

New submission
