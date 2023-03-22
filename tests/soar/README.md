Source: https://github.com/mcglinnlab/soar

# soar: species occupancy aggregator in R
Aggregator for citizen science data
Ashley Woods, Dan McGlinn

# How to install soar

The easiest option is to install the package directly from GitHub using the
package `devtools`. If you do not already have `devtools` installed then need to
install it.

```r
install.packages('devtools')
library(devtools)
```

Then to install the package from github:

```r
install_github('mcglinnlab/soar')
```

# Launch the GUI

```r
library(soar)
gui()
```

Have fun entering in species common or scientific names. We'll be adding more 
in time. Please leave us feedback as an issue! 

