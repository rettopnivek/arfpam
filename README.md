## Assorted R Functions for Processing, Analysis, and Modeling

The R package **arfpam** provides a collection of functions to help with a variety of common tasks when processing, plotting, analyzing, and modeling data.

### Prerequisites

R (version > 3.0)

### Installation

```r
# Install development version from Github
devtools::install_github("rettopnivek/arfpam")
```

### Usage

The package **arfpam** contains a diverse and eclectic set of functions. Below are examples for some of the most noteworthy functions.

#### Custom operators

```r
# Use %+=% and %-=% to increment 
# or decrement a value
inc <- c( 0, 0 )
for (i in 1:5) {
  inc[1] %+=% 1; inc[2] %-=% 1;
  print( inc )
}
#> [1] 1 -1
#> [1] 2 -2
#> [1] 3 -3
#> [1] 4 -4
#> [1] 5 -5

# Use %p% to combine strings
'Hello' %p% ' ' %p% 'world'
#> [1] "Hello world"

# Use %w% to see if a phrase is 
# contained within any of a set 
# of character strings
'at' %w% c( 'cat', 'bat', 'dog' )
#> [1]  TRUE  TRUE FALSE
```

#### Indices and sequences

```r
# Define vector of values
x <- 1:9
# Loop over x and extract 
# elements in sets of 3
for ( i in 1:3 ) {
  print( x[ over( 1:3, i ) ] )
}
#> [1] 1 2 3
#> [1] 4 5 6
#> [1] 7 8 9

# Extract odd values
every( 1:8, 2 )
#> [1] 1 3 5 7
# Extract even valuess
every( 1:8, 2, 2 )
#> [1] 2 4 6 8
# Extract every 4th value
every( 1:8, 4 )
#> [1] 1 5

# Create linear sequence of 
# 5 values starting from 0 
# and ending at 1
lin( 0, 1, 5 )
#> [1] 0.00 0.25 0.50 0.75 1.00
```

#### Math

```r
# Convert a probability to log-odds
logit( .5 )
#> [1] 0
# Convert log-odds to a probability
logistic( 0 )
#> [1] 0.5

# Raise a value to a power
pow( 2, 3 )
#> [1] 8
```

#### Statistics

```r
# Standard error of the mean
x <- rnorm( 100 ) # SE should be ~0.1
round( sem( x ), 1 )
#> [1] 0.1

# Convert width for confidence/credible 
# intervals into lower and upper limits
bounds( .95 )
#> [1] 0.025 0.975
bounds( .5 )
#> [1] 0.25 0.75
```

One can see a complete list of the functions provided by **arfpam** via the command:

```r
ls(pos = "package:arfpam")
```
