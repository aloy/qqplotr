## Resubmission
This is a resubmission. In this version I have:

* Fixed URLs in the documentation by changing http --> https, 
adding trailing slashes, and following moved
content as appropriate.

## Test envs

* win-builder 
* macOS Catalina 10.15.7 (local install), R 4.0.5
* Ubuntu 20.04 (on github), R-devel, R-release
* Ubuntu Linux 20.04.1 LTS (on r-hub), R-release, GCC

## R CMD check results

There were no ERRORs, WARNINGs, or NOTEs.

## Downstream dependencies
I have also run R CMD check on downstream dependencies of qqplotr 
(https://github.com/aloy/qqplotr/tree/master/revdep). 
All packages that I could install passed except:

* latrend: I received Warning: replacing previous import ‘data.table:::=’ by ‘ggplot2:::=’ when loading ‘latrend’. This isn't related to changes to qqplotr.
