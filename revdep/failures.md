# HLMdiag

<details>

* Version: 0.5.0
* GitHub: https://github.com/aloy/HLMdiag
* Source code: https://github.com/cran/HLMdiag
* Date/Publication: 2021-05-02 04:30:08 UTC
* Number of recursive dependencies: 103

Run `revdepcheck::revdep_details(, "HLMdiag")` for more info

</details>

## In both

*   checking whether package ‘HLMdiag’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/aloy/Documents/r_packages/qqplotr/revdep/checks.noindex/HLMdiag/new/HLMdiag.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘HLMdiag’ ...
** package ‘HLMdiag’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
clang -mmacosx-version-min=10.13 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I'/Users/aloy/Documents/r_packages/qqplotr/revdep/library.noindex/HLMdiag/Rcpp/include' -I'/Users/aloy/Documents/r_packages/qqplotr/revdep/library.noindex/HLMdiag/RcppArmadillo/include' -I/usr/local/include   -fPIC  -Wall -g -O2  -c HLMdiag_init.c -o HLMdiag_init.o
clang++ -mmacosx-version-min=10.13 -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I'/Users/aloy/Documents/r_packages/qqplotr/revdep/library.noindex/HLMdiag/Rcpp/include' -I'/Users/aloy/Documents/r_packages/qqplotr/revdep/library.noindex/HLMdiag/RcppArmadillo/include' -I/usr/local/include   -fPIC  -Wall -g -O2  -c cooksd_obs.cpp -o cooksd_obs.o
In file included from cooksd_obs.cpp:1:
In file included from /Users/aloy/Documents/r_packages/qqplotr/revdep/library.noindex/HLMdiag/RcppArmadillo/include/RcppArmadillo.h:29:
In file included from /Users/aloy/Documents/r_packages/qqplotr/revdep/library.noindex/HLMdiag/RcppArmadillo/include/RcppArmadillo/interface/RcppArmadilloForward.h:25:
In file included from /Users/aloy/Documents/r_packages/qqplotr/revdep/library.noindex/HLMdiag/Rcpp/include/RcppCommon.h:168:
...
                                                      ^
1 warning generated.
clang++ -mmacosx-version-min=10.13 -std=gnu++11 -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -single_module -multiply_defined suppress -L/Library/Frameworks/R.framework/Resources/lib -L/usr/local/lib -o HLMdiag.so HLMdiag_init.o cooksd_obs.o cooksd_subset.o covratio.o covtrace.o linear_algebra_fnc.o mdffits_subset.o -L/Library/Frameworks/R.framework/Resources/lib -lRlapack -L/Library/Frameworks/R.framework/Resources/lib -lRblas -L/usr/local/gfortran/lib/gcc/x86_64-apple-darwin18/8.2.0 -L/usr/local/gfortran/lib -lgfortran -lquadmath -lm -F/Library/Frameworks/R.framework/.. -framework R -Wl,-framework -Wl,CoreFoundation
ld: warning: directory not found for option '-L/usr/local/gfortran/lib/gcc/x86_64-apple-darwin18/8.2.0'
ld: warning: directory not found for option '-L/usr/local/gfortran/lib'
ld: library not found for -lgfortran
clang: error: linker command failed with exit code 1 (use -v to see invocation)
make: *** [HLMdiag.so] Error 1
ERROR: compilation failed for package ‘HLMdiag’
* removing ‘/Users/aloy/Documents/r_packages/qqplotr/revdep/checks.noindex/HLMdiag/new/HLMdiag.Rcheck/HLMdiag’


```
### CRAN

```
* installing *source* package ‘HLMdiag’ ...
** package ‘HLMdiag’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
clang -mmacosx-version-min=10.13 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I'/Users/aloy/Documents/r_packages/qqplotr/revdep/library.noindex/HLMdiag/Rcpp/include' -I'/Users/aloy/Documents/r_packages/qqplotr/revdep/library.noindex/HLMdiag/RcppArmadillo/include' -I/usr/local/include   -fPIC  -Wall -g -O2  -c HLMdiag_init.c -o HLMdiag_init.o
clang++ -mmacosx-version-min=10.13 -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I'/Users/aloy/Documents/r_packages/qqplotr/revdep/library.noindex/HLMdiag/Rcpp/include' -I'/Users/aloy/Documents/r_packages/qqplotr/revdep/library.noindex/HLMdiag/RcppArmadillo/include' -I/usr/local/include   -fPIC  -Wall -g -O2  -c cooksd_obs.cpp -o cooksd_obs.o
In file included from cooksd_obs.cpp:1:
In file included from /Users/aloy/Documents/r_packages/qqplotr/revdep/library.noindex/HLMdiag/RcppArmadillo/include/RcppArmadillo.h:29:
In file included from /Users/aloy/Documents/r_packages/qqplotr/revdep/library.noindex/HLMdiag/RcppArmadillo/include/RcppArmadillo/interface/RcppArmadilloForward.h:25:
In file included from /Users/aloy/Documents/r_packages/qqplotr/revdep/library.noindex/HLMdiag/Rcpp/include/RcppCommon.h:168:
...
                                                      ^
1 warning generated.
clang++ -mmacosx-version-min=10.13 -std=gnu++11 -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -single_module -multiply_defined suppress -L/Library/Frameworks/R.framework/Resources/lib -L/usr/local/lib -o HLMdiag.so HLMdiag_init.o cooksd_obs.o cooksd_subset.o covratio.o covtrace.o linear_algebra_fnc.o mdffits_subset.o -L/Library/Frameworks/R.framework/Resources/lib -lRlapack -L/Library/Frameworks/R.framework/Resources/lib -lRblas -L/usr/local/gfortran/lib/gcc/x86_64-apple-darwin18/8.2.0 -L/usr/local/gfortran/lib -lgfortran -lquadmath -lm -F/Library/Frameworks/R.framework/.. -framework R -Wl,-framework -Wl,CoreFoundation
ld: warning: directory not found for option '-L/usr/local/gfortran/lib/gcc/x86_64-apple-darwin18/8.2.0'
ld: warning: directory not found for option '-L/usr/local/gfortran/lib'
ld: library not found for -lgfortran
clang: error: linker command failed with exit code 1 (use -v to see invocation)
make: *** [HLMdiag.so] Error 1
ERROR: compilation failed for package ‘HLMdiag’
* removing ‘/Users/aloy/Documents/r_packages/qqplotr/revdep/checks.noindex/HLMdiag/old/HLMdiag.Rcheck/HLMdiag’


```
