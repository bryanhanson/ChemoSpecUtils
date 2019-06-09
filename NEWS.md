# ChemoSpecUtils 0.3.20 2019-06-09
## Improvements
* Internal changes to the handling of PCA results were made to accommodate the introduction of additional PCA options.  Some of these are listed below.
* New function `getVarExplained` handles this common task for all data reduction methods.
* Function `plotScree.mia` was eliminated via the introduction of `.getVarExplained` and small changes to `plotScree.default`.
* NAMESPACE cleaned up a bit.
* Miscellaneous polishing of functions and documentation.

## Misc.
* `conColScheme` was moved from `ChemoSpec` to `ChemoSpecUtils` so it could be readily available for `Spectra2D` objects.  It was also re-written in the style of `updateGroups` for consistency and user-friendliness.
* Documentation `colorSymbol` was moved from `ChemoSpec` to `ChemoSpecUtils`.

## Bug Fixes
* An issue with the setting of x and y limits in `plotScores` was fixed.  This was a long standing bug that somehow escaped notice (dating to the first few months of `ChemoSpec`!).
* `.getVarExplained` was returning the eigenvalues, not the percent variance explained, for `class(mia)`.  This affected scree plots for `class(mia)` in `ChemoSpec2D`.
* `plotScores` was no longer including the percent variance explained in score plots, if `xlab` and/or `ylab` were not provided.  Fixed.

# ChemoSpecUtils 0.2.211 2019-02-28
## Bug Fixes
* `chkSpectra.Spectra2D` would fail if there was only one sample when checking to see if NAs were present in the same positions across all data sets.  Fixed.
* Replaced code for cosine distance calculation in `rowDist`.  The previous version was giving slightly wrong answers.

# ChemoSpecUtils 0.2.204 2018-12-19
## Bug Fixes
* `updateGroups` now reports the old groups if `new.grps = NULL`, as intended and claimed in the documentation.

## Bug Fixes
* `plotScree` now knows what to do with class `princomp`.

# ChemoSpecUtils 0.2.197 2018-12-02
## Bug Fixes
* More changes to `.chkArgs` since errors were introduced via the most recent "fix"!  Updated unit tests for the function, but even more work should be done there.  Using `HandyStuff::examplePage` to look for errors and warnings.  None found.

# ChemoSpecUtils 0.2.170 2018-11-28
## Bug Fixes
* Fixed a problem in `.chkArgs(mode = 13)` that was giving an error in `ChemoSpec::r_pcaSpectra`.

# ChemoSpecUtils 0.2.163 2018-11-28
## Improvements
* Added function `updateGroups`.

# ChemoSpecUtils 0.2.160 2018-11-23
## Bug Fixes
* Fixed a problem with color assignment in `.groupNcolor` (the problem seems to have been present from the very first version of `ChemoSpec`).  Reported by Reinhard Kerschner (many thanks!).

## Improvements
* Made `check4Gaps` gap checking more robust, with better examples and documentation.  If no gap found, a data frame with one row is returned.  In turn, this required changes over in `.binData` and `binSpectra` which are in `ChemoSpec`.  In addition, while `check4Gaps` still accepts an argument `tol` no other functions are using it, and a much more suitable default value is computed in `check4Gaps` rather than being computed in another function and passed here.

# ChemoSpecUtils 0.2.142 2018-11-09
## Improvements
* Consistent argument checking introduced via `.chkArgs`.

# ChemoSpecUtils 0.2.23 2018-10-28
## Improvements
* Improved `sumSpectra` output format.
* `removeGroup` and `removeSample` go full S3 dispatch for consistency among the three packages.
* Unit tests moved from `ChemoSpec2D`.
* `hcaScores` moved from `ChemoSpec` and converted to S3 dispatch.

## New Features
* New function `chkArgs` and associated unit tests added, but it is not yet used in any of the other functions.

## Misc.
Seems to work correctly on behalf of `ChemoSpec` and `ChemoSpec2D` devel versions.

# ChemoSpecUtils 0.1.62 2018-10-12
## Misc.
* S3 generics created for `chkSpectra`, `removeFreq`, `sumSpectra`, `sumGroups`.
* Removed Additional_repositories field from DESCRIPTION (I was using it incorrectly).
* Removed Rd cross references to `ChemoSpec2D` for CRAN, temporarily.

# ChemoSpecUtils 0.1.52 2018-10-08
## Misc.
* Additional_repositories added to DESCRIPTION.
* Added dependency on `ChemoSpec` >= 5.0.

# ChemoSpecUtils 0.1.44 2018-10-08
## Misc.
* ORCID added to DESCRIPTION.

# ChemoSpecUtils 0.1.43 2018-10-08
## Misc.
* `.groupNcolor` added with argument mode.
* README.md added.

# ChemoSpecUtils 0.1.38 2018-10-06
## Misc.
* General polishing and documentation improvements.

# ChemoSpecUtils 0.1.12 2018-10-01
## Misc.
* Seems to be fully functional with the 5.0 series of ChemoSpec.

# ChemoSpecUtils 0.1.1 2018-09-27
## Misc.
* `addLegend` gets an example.

# ChemoSpecUtils 0.1.1 2018-09-27
## Misc.
* Package framework & first set of functions.
