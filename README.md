# Deep Learning Flows for Neuroimaging
<!-- badges: start -->
[![Travis build status](https://travis-ci.com/muschellij2/dl4ni.svg?branch=master)](https://travis-ci.com/muschellij2/dl4ni)
[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/muschellij2/dl4ni?branch=master&svg=true)](https://ci.appveyor.com/project/muschellij2/dl4ni)
<!-- badges: end -->
This document describes the use of the `dl4ni` package, Deep Learning Flows for Neuroimaging.

## Introduction

Recent years have seen the rise of _Deep Learning_ applied to Neuroimgining problems. Some concepts and state-of-the-art are reviewed [here](https://bitbucket.org/neuroimaginador/dl4ni/wiki/deep_learning) and [here](https://bitbucket.org/neuroimaginador/dl4ni/wiki/deep_learning_neuroimaging).

There are a lot of packages in the R ecosystem focused on several aspects of Neuroimage processing, many of them listed in the [CRAN Task View: Medical Imaging](https://CRAN.R-project.org/view=MedicalImaging) section: `oro.nifti`, `oro.dicom`, `RNifti`, `fmri`, `divest` (among others) for reading/writing of images; `ANTsR`, `fslr` for more advanced processing methods; `RNiftyReg` specialized in registration, `mritc` focused in image segmentation. Many more, for DTI, fMRI, molecular imaging, and visualization are available in [CRAN](https://CRAN.R-project.org).

For _Deep Learning_, there are basically 4 packages maintained by diverse organizations: `mxnet` from [Apache MXNet](https://mxnet.incubator.apache.org), `h2o` from [H2O.ai](https://www.h2o.ai/) and `tensorflow` and `keras` from Google and [RStudio](https://www.rstudio.com/). All of these packages are available at CRAN.

The lack of packages to apply Deep Learning techniques to Neuroimaging pipelines inspired the creation of the `dl4ni` package.

## Main Features

This package has been written and is developed with some key features in mind:

- Simple definition of Neuroimaging flows, which can incorporate both `R` functions (including those from other packages, such as `ANTsR` or `fslr`) and _Deep Learning_ models. 
- Simple definition of _Deep Learning_ models. Many wrappers to common models ([U-Net](http://google.com/?q=unet), [SegNet](http://google.com/?q=segnet) ...) have been included to ease the definition of models.
- Highly customizable models, allowing concatenation and merging of models, layers, branching...
- Creation of a common interface for all models built with this package, regarding training, testing and inference. This is achieved by the use of R6 classes.
- Tackle different types of problems: classification, labelling or regression.
- Work natively with NIfTI files.
- Native 3D, although vector representations of images are allowed (and, in some cases, adviced).
- Simple memory management. Users can impose memory constraints when training a model, batch size is automatically adjusted.
- Dataset repositories. Some datasets regarding typical Neuroimaging problems (brain extraction, segmentation, parcellation...) are released in public repositories, and a mechanism to get these datasets into our local system to be able to train models is also included in the package.

## Requirements

This package is based in the `keras` package, so a working installation of both `tensorflow` and `keras` is a minimum requirement. Check [the package website](https://keras.rstudio.com/) for detailed instructions on how to get those packages installed and running.

## Installation

Currently, this package is not in CRAN. The only way to get this package installed is by obtaining the source and building and installing the package in a local system.

```
#!bash
R CMD INSTALL /path/to/dl4ni
``` 

## Examples

Feel free to use any of the provided demos in the package. Just load the package and list the available demos as:

```
#!R
library(dl4ni)

list.files(system.file("demo", package = "dl4ni"))
``` 

And open the one you like most!

## See also
To know more about the use of this package, head to the [wiki](https://bitbucket.org/neuroimaginador/dl4ni/wiki/).

