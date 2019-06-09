[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active) [![Downloads](https://cranlogs.r-pkg.org/badges/ChemoSpecUtils)](https://cran.r-project.org/package=ChemoSpecUtils)

## What is ChemoSpecUtils?

`ChemoSpecUtils` is  collection of functions that supports both `ChemoSpec` and `ChemoSpec2D`.  Users access the functions in `ChemoSpecUtils` automagically via the other packages.

## How to install ChemoSpecUtils

If you install `ChemoSpec` or `ChemoSpec2D` then `ChemoSpecUtils` should automatically be installed.  If not, or if you want to look at the code, here are the instructions.

### From CRAN using R:

````r
chooseCRANmirror() # choose a CRAN mirror
install.packages("ChemoSpecUtils")
library("ChemoSpecUtils")
````

### To install from Github using R:

````r
install.packages("devtools")
library("devtools")
install_github(repo = "bryanhanson/ChemoSpecUtils@master")
````

If you use `@some_other_branch` you can get other branches that might be available.  They may or may not pass CRAN checks and thus may not install automatically using the method above.  Check the NEWS file to see what's up.

### License Information

`ChemoSpecUtils` is distributed under the GPL-3 license, as stated in the DESCRIPTION file.  For more info, see the [GPL site.](https://gnu.org/licenses/gpl.html)

Questions?  hanson@depauw.edu
