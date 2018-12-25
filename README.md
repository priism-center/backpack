# backpack
_One package to rule them all, one package to find them, one package to bring them all and in the darkness bind them!_

Easily discover, collect, and learn to use packages for statiscal modeling. Use built-in binders (collections) of packages for topics in statistics. Create your own binders to store collections of packages for easy installing and loading within your projects. Also, explore lessons on multivariate statistical modeling in R. 

## Instructions

To download this package, ensure you have devtools installed

If not:
`install_packages(‘devtools’)`
Then,

`devtools::install_github(“priism-center/backpack”)`

To get started with using the package, attach the package to your environment
`library(backpack)`
and then initalize the package with a path in which to store personal binders. For example:
`initialize_backpack("~/Documents/")`

Everytime you load a new R Session, remember to set path for backpack to the one you initialized to. For example:
`set_backpack_path("~/Documents/")`

## Authors
This project was developed as part of the Fall '18 Statistical Consulting course, part of the Applied Statistics for Social Science Research, NYU Steinhardt. 

**Consultants**: Bianca Brusco, Andrea Hassler, Kaushik Mohan and Madison Volpe

**Client**: Prof. Ying Lu
