# Test for functions in 'S03_Utilities.R'
# Written by Kevin Potter
# email: kevin.w.potter@gmail.com
# Please email me directly if you
# have any questions or comments
# Last updated 2021-04-16

# Table of contents
# 1) The function 'over'
# 2) The function 'every'
# 3) The function 'has_NA'
# 4) The function 'lin'
# 5) The function 'empty_list'

###
### 1) The function 'over'
###

test_that("over returns range as is", {
  expect_equal( over( 1:3, 1 ) , 1:3 )
})

test_that("over updates range", {
  expect_equal( over( 1:3, 2 ) , 4:6 )
})

test_that("over returns subset of range", {
  expect_equal( over( 1:2, 2, per = 3 ) , 4:5 )
})

test_that("over adjusts iterator", {
  expect_equal( over( 1:2, 2, adj = 1 ) , 7:8 )
})

test_that("over rounds to integers", {
  expect_equal( over( c( .1, .9, .5) , 1 ) , c( 0, 1, 0 ) )
})

# Note default behavior with negative values
# (e.g., {-1, -2, -3}) will take highest value
# (i.e., -1) as 'per' value.
test_that("over by default takes least negative value", {
  expect_equal( over( -(1:3), 2 ) , c( -2, -3, -4 ) )
})

###
### 2) The function 'every'
###

test_that("every extracts odd values", {
  expect_equal( every(1:5 ) , c( 1, 3, 5 ) )
})

test_that("every extracts even values", {
  expect_equal( every(1:5, 2, 2), c( 2, 4 ) )
})

test_that("every extracts custom increments", {
  expect_equal( every(1:6, 3), c(1, 4) )
})

test_that("every can start at custom point", {
  expect_equal( every(1:6, 2, 3), c(3, 5) )
})

###
### 3) The function 'has_NA'
###

###
### 4) The function 'lin'
###

###
### 5) The function 'empty_list'
###

test_that("empty_list creates specified elements", {
  expect_equal( empty_list( 3 ),
                c( list(NULL), list(NULL), list(NULL) ) )
})

test_that("empty_list creates named lists", {
  expect_equal( empty_list( 3, c( 'S01', 'S02', 'S03' ) ),
                c( S01 = list(NULL),
                   S02 = list(NULL),
                   S03 = list(NULL) ) )
})

test_that("empty_list does not add names for mismatched labels", {
  expect_equal( suppressWarnings( empty_list( 3, 'S01' ) ),
                c( list(NULL), list(NULL), list(NULL) ) )
})

test_that("empty_list returns warning for mismatched labels", {
  expect_warning( empty_list( 3, 'S01' ) )
})

