# Install MASH for Development

Get the source code.
```bash
git clone git@github.com:dd-harp/MASH.git
```

Open RStudio. Use RStudio to "open project" on the file
`MASH/macro/macro.rproj`. On the RStudio command line,
install these packages:
```R
install.packages(c("RcppProgress", "BH", "RcppArmadillo"))
```
Then go to the Build menu, and "Install and Restart."
