# Deep Learning Flows for Neuroimaging

This document describes the use of the `dl4ni` package, Deep Learning Flows for Neuroimaging.

## Introduction

TOWRITE: Importance of Deep Learning in Neuroimaging.

There are a lot of packages in the R ecosystem focused on several aspects of Neuroimage processing, many of them listed in the [CRAN Task View: Medical Imaging](https://CRAN.R-project.org/view=MedicalImaging) section: `oro.nifti`, `oro.dicom`, `RNifti`, `fmri`, `divest` (among others) for reading/writing of images; `ANTsR`, `fslr` for more advanced processing methods; `RNiftyReg` specialized in registration, `mritc` focused in image segmentation. Many more, for DTI, fMRI, molecular imaging, and visualization are available in [CRAN](https://CRAN.R-project.org).

For _Deep Learning_, there are basically 4 packages maintained by diverse organizations: `mxnet` from [Apache MXNet](https://mxnet.incubator.apache.org), `h2o` from [H2O.ai](https://www.h2o.ai/) and `tensorflow` and `keras` from Google and [RStudio](https://www.rstudio.com/). All of these packages are available at CRAN.

The lack of packages to apply Deep Learning techniques to Neuroimaging pipelines inspired the creation of the `dl4ni` package.

## Requirements

This package is based in the `keras` package, so a working installation of both `tensorflow` and `keras` is a minimum requirement. Check [the package website](https://keras.rstudio.com/) for detailed instructions on how to get those packages instelled and running.

## Installation

Currently, this package is not in CRAN. The only way to get this package installed is by obtaining the source and building and installing the package in a local system.

## Models
### Model Architecture
### Model Specification
### Training Configuration
### Model Fit
### Inference
### Model Save and Load
### Demos
#### Brain Extraction
#### Brain Segmentation 
## Flows
### Flow Architecture
#### Adding Inputs and Functions
#### Adding Existing Models 
#### Adding Trainable Models
### Training Parts of the Flow
### Executing a Flow
### Flow Save and Load
### Demos
#### Brain Parcellation
#### Lesion Detection
#### Tumor Segmentation 
