# T4 Paquete R
# GeneGraphs


GeneGraphs is an R package that allows users to visualize and analyze gene networks. It provides tools for constructing and manipulating gene networks, as well as for performing various types of analysis on these networks.

## Installation

You can install the development version of GeneGraphs from GitHub with:

```r
# install.packages("devtools")
devtools::install_github("ImaicelaBetzabethBiotech/GeneGraphs")
```
# Usage
To use GeneGraphs, you will need to load the required libraries:

library(RTCGA)
library(RTCGA.mRNA)
library(ggplot2)
library(ggpubr)
library(FactoMineR)
library(factoextra)
library(dplyr)
library(carData)
library(car)
library(multcomp)
library(reshape2)
Then, you can start using the functions provided by the package to construct and analyze gene networks.
```r
> boxplot_expression(expr,"dataset","all")
>
```
![BoxplotALL](https://user-images.githubusercontent.com/117690624/229288576-acb3c94c-9c68-4259-9bd7-f749f79eb2a8.png)


# Authors

* Betzabeth Imaicela
* Johanna Tanguila

# License
This project is licensed under the terms of the MIT license.


