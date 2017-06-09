## MASH.R6 (Modular Analysis and Simulation for Human Health - Mosquito-borne Pathogen Transmission)

### R6 Object-oriented Version

* MASH.R6 is a development version of MASH-MBPT. This repository is a package for the R programming language, and can be installed by the R command: `devtools::install_github(repo = "smitdave/MASH", ref = "MASH.R6")`. 
* MASH-MBPT is being reprogrammed in R6 OOP style for greater flexibility of the code, better representation of the authors' vision in code, more durable codebase, true modularity, and more logical opportunities for parallelism.
* The R6 class framework can be found here: https://github.com/wch/R6
* `MASH.R6` currently has a placeholder docs website at: https://smitdave.github.io/MASH/index.html which is being built with `pkgdown`.
* For bug reports please open an issue on this GitHub repository by clicking on the "Issues" tab at the top of the page and then "Create an Issue". If possible please provide a detailed explanation of the circumstances under which the bug was encountered.

### Code Optimization

* There is a project to use `RcppR6` to export `C++` classes and methods to the `R6` framework to optimize bits of the MASH.R6 codebase most in need, and can be found here: https://slwu89.github.io/RcppQueues/
