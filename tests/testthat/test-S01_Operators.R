# Test for functions in 'S01_Operators.R'
# Written by Kevin Potter
# email: kevin.w.potter@gmail.com
# Please email me directly if you
# have any questions or comments
# Last updated 2021-03-31

# Table of contents
# 1) The operator `%p%`
# 2) The operators `%+=%` and %-=%`

###
### 1) The operator `%p%`
###

test_that("%p% combines multiple strings", {
  expect_equal( "Hello" %p% " " %p% "world", "Hello world" )
})

test_that( "%p% combines single string with vector of strings", {
  expect_equal( "A" %p% 1:3, c( "A1", "A2", "A3" ) )
})

test_that( "%p% recycles cases for unbalanced vectors", {
  expect_equal( 1:2 %p% 1:4, c( "11", "22", "13", "24" ) )
})

###
### 2) The operators `%+=%` and %-=%`
###

test_that("%+=% increments a variable", {
  inc <- 0; for (i in 1:3) { inc %+=% 1 };
  expect_equal( inc, 3 )
})

test_that("%-=% decrements a variable", {
  inc <- 3; for (i in 1:3) { inc %-=% 1 };
  expect_equal( inc, 0 )
})

test_that("%+=% respects scoping of functions", {
  inc <- 0;
  foo <- function() { for (i in 1:3) inc %+=% 1 }
  foo()
  expect_equal( inc, 0 )
})

test_that("%+=% prioritized in order of operations", {
  x <- 1; y <- 1;
  invisible( x %+=% y/2 );
  expect_equal( x, 2 )
})

test_that("%+=% with brackets ensures desired order of operations", {
  x <- 1; y <- 1;
  invisible( x %+=% (y/2) );
  expect_equal( x, 1.5 )
})

test_that("%+=% is vectorized with predefined variables", {
  x <- 1:3
  expect_equal( x %+=% 3, 4:6 )
  x <- 1
  expect_equal( x %+=% 1:3, 2:4 )
})

test_that("%+=% does not work as an addition operator", {
  expect_error( 1:3 %+=% 3 )
})

