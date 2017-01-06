# Introduction
`Beiwe-Analysis` is our GitHub code repository for analyzing Beiwe and collaborator data.  The idea is to help make study analysis as systematic and error-free as the data itself.  Some of this code references **`Python`** or **`C`**-related code for speed, but most of this code is in **`R`**, and is readily converted into an **`R`** package.  In the near future, we will provide a notebook example showing how to use these functions.

This is largely an effort of Ian and Patrick, but we intend to include any other efforts as well.  These efforts are closely related to Matt's `beiwedata`, and we plan to merge our efforts in the future.

# Table of Contents
- [Overview](#Overview)
    - [Preprocessing](#Preprocessing)
    - [Processing](#Processing)
    - [Outputs](#Outputs)
    - [Utility](#Utility)
    

## Overview
![beiwe_analysis_overview](https://github.com/onnela-lab/Beiwe-Analysis/blob/master/Figures/beiwe_analysis_overview.png)
## Preprocessing
Beiwe data comes in [standard formats](https://github.com/onnela-lab/beiwedata#data-overview), but requires processing before generating any insights.  This includes the most basic tasks, such as determining how much data you have, or its quality.  Also, our collaborators usually have separate data they would like to combine with Beiwe data.  It is not obvious how all of this should be done.

We say data is ***preprocessed*** if it takes the form of a two-dimensional array (*i.e.* matrix, data frame) with at least two columns:
* **`Datetime`**, a string containing a universal format for date and time.  For now, it's a **`POSIXct`** object, but this may change.
* **`Person`**, a string containing an ID for the subject at hand.

The goal of preprocessing with raw Beiwe data or collaborator is to convert it to processed data.  For now, all preprocessed data is small enough to be loaded into working memory.

## Processing
Even after preprocessing, most analyses require basic steps before creating plots or performing statistical analysis.  For example, plotting may require preprocessed data using several different datasteams, or statistical analysis may require comparing one time with a fixed time lag in the past.  To avoid errors in these tasks, we list how we do them here.  Data is called ***processed*** if it is preprocessed, and is the product of the functions specified here.  The goal is to do as much processing as possible using only these functions.

## Outputs
Once the data is processed, we arrive at the juicy part: plots and statistical analysis.  These functions take processed data in a specified format as inputs, and return highly specific ***outputs*** such as plots, models, or tests.  These functions should not contain any processing steps.

## Utility
A great deal of functions call into the above categories, but some do not.  All other helper functions belong in **utility**.  Examples include package dependencies, and global constants such as plotting colors.
