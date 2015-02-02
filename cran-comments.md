##Submission notes for trustOptim, version 0.8.5

### Resubmission response notes:

-  Added a LF at the end of src/Makevars
-  Specified the Authors@R field in terms of a person() function.


### Test environments

-  local OS X 10.10.1 install, R 3.1.2, CRAN compiled binary
-  win_builder

### R CMD check results
There were no ERRORs or WARNINGs

NOTE is probably related to changing from the Author/Maintainer fields
to the Authors@R field in the DESCRIPTION file.  There is no change to
the maintainer.

### Warnings about abs
I received an email from Brian Ripley on 15-Dec-2014, saying that clang++ was generating notes on calls to abs for floating point arguments when -Wall -pedantic flags were enabled.  I was not able to replicate that on my system.  Nevertheless, I did replace abs with std::abs, as suggested by the compiler notes in the logs.


