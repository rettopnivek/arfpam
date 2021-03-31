# Test for functions in 'S03_Utilities.R'
# Written by Kevin Potter
# email: kevin.w.potter@gmail.com
# Please email me directly if you
# have any questions or comments
# Last updated 2021-03-31

# Table of contents
# 1) The function 'over'

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

