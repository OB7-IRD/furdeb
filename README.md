![furbed_logo](furdeb_logo.png)

# Package furdeb

FUnctions for R DEvelopment and Beyond!

# Package installation

You can run the following code in the R console:

```R
# devtools is a necessary package
# if it is not installed, run the following line
install.packages("devtools")
# this package use rJava package as dependency. If an error occur link to this package during the install, check if you have a 32 bits and 64 bits Java environment install on your computer.
# load the last package version from GitHub
devtools::install_github("https://github.com/OB7-IRD/furdeb")
# load the library
library(furdeb)
# you can access the package documentation with the following line
?furdeb
# If you want the documentation of a specific package function use the same syntax, for example for the function access_dbconnection
?access_dbconnection
```
