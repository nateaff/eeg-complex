
<!-- README.md is generated from README.Rmd. Please edit that file -->
eeg-complex
-----------

This package contains code that reproduces figures and analyses used in my MA thesis, *Improvement of epsilon-complexity estimation and an application to seizure prediction*.

For an overview of the research and individual analyses see the [project website](https://nateaff.github.io/eeg-complex/).

The [ecomplex](https://github.com/nateaff/ecomplex) package contains the `R` version of the procedure for computing the epsilon-complexity of a single variable function or time series. The repositories for the three other packages used in this thesis, [tssims](https://github.com/nateaff/tssims), [tsfeats](https://github.com/nateaff/tsfeats) and the imaginatively named [tssegment](https://github.com/nateaff/tssegment), are also on github. The first two packages unify access to a set of simulations and features, respectively. The `tssegment` package provides an interface to the segmentation and classification methods used for predicting seizures.

The current draft of the complete thesis is [here.](https://github.com/nateaff/eeg-complex/blob/master/docs/thesis/thesis.pdf) (This is a draft and has not yet been submitted.)

Organization
------------

This is an `R` package with additional files that follow the structure of a [workflowr](https://github.com/jdblischak/workflowr) project. See the [workflowr documentation](https://jdblischak.github.io/workflowr/index.html) for more information. The [workflowr readme](https://github.com/jdblischak/workflowr) also as a nice overview of similar project management packages.

This repository follows the structure of a workflowr project with the additional folders and files associated with an `R` package -- man files,the `R` directory and description files, etc.
