# What is XIFF?

We have developed two shiny applications CLIFF and TIFF for cell 
line and tissue sample analysis, respectively. 
The redundant functions used in both applications were collected in
this new package called XIFF, which is designed to provide database 
access, machine learning and plotting functionality, comparison class 
handling and overlapping R/shiny functions.

## XIFF Application Programming Interface (API)

The core functionality of XIFF is also available for R-programmers.

### Installation

Usually XIFF is already installed in your R installation.
If you are interested in the latest development version have a look at our
[bitbucket repository](https://bitbucket.biscrum.com/projects/BIARD/repos/xiff/browse)

### Quick start

All data retrieval functions are part of TIFF and CLIFF,
which is the recommended way to get data without knowing the
database structure. If you want to access the database directly, 
here is an example. Start an R session in RStudio and run the 
following code:

```
library(CLIFF)
library(XIFF)
library(tidyverse)

setDbOptions(getSettings())

sql <- paste("SELECT celllinename, 2*2^log2relativecopynumber",
             "AS relativecopynumber",
             "FROM cellline.processedcopynumberview",
             "WHERE ensg = 'ENSG00000141736'",
             "ORDER BY log2relativecopynumber DESC LIMIT 20")

getPostgresql(sql)
```

This data frame shows you the top 20 cell lines with ERBB2 amplification.
