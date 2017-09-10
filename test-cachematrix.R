library(testthat)

source("./cachematrix.R")

getInversableMatrix <- function() {
    matrix( c(5, 1, 0,
              3,-1, 2,
              4, 0,-1), nrow=3, byrow=TRUE)
}

test_that("makeCacheMatrixInvalidInput", {
    expect_that(makeCacheMatrix(c(1, 2, 3)), throws_error())
})

test_that("makeCacheMatrix", {
    m <- getInversableMatrix() 

    specialM <- makeCacheMatrix(m)

    expect_that(specialM$get(), equals(m))
    expect_that(specialM$getInvMatrix(), equals(NULL))
})

test_that("cacheSolve", {
    m <- getInversableMatrix() 

    specialM <- makeCacheMatrix(m)

    invMatrix <- cacheSolve(specialM)

    # Verify result
    expect_that(invMatrix, equals(solve(m)))
    # Verify if cache is set
    expect_that(specialM$getInvMatrix(), equals(invMatrix))

    # Verify cache result
    expect_that(cacheSolve(specialM), equals(invMatrix))
})
