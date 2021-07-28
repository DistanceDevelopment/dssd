## Test environments
* local OS X install, R 4.1.0
* win-builder (devel and release)
* github workflows: windows (4.1), mac (4,1), ubuntu-18.04 (4.1 & devel) 
* Fedora Linux, R-devel, clang, gfortran
* Ubuntu Linux 20.04.1 LTS, R-release, GCC
* Windows Server 2008 R2 SP1, R-devel, 32/64 bit

## R CMD check results
There were no ERRORs, WARNINGs or NOTEs. 

Error on first submission was due to differences in results from setting the seed, these tests are now only run locally.

I cannot replicate installation error in Windows for first submission.

## Reverse dependencies

I am also the maintainer for dsims and I have run tests for dsims using this new version of dssd and it is passing all checks.
