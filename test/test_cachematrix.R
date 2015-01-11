## Defines unit tests for functions in cachematrix

context("cachematrix")

test_that("makeCacheMatrix() returns a list", {
    x <- makeCacheMatrix()
    expect_is(x, "list")
})

test_that("makeCacheMatrix(m)$get returns the matrix", {
    m <- matrix(2,3)
    x <- makeCacheMatrix(m)
    expect_identical(x$get(), m)
})

test_that("makeCacheMatrix(m)$set stores a new matrix", {
    x <- makeCacheMatrix()
    m <- matrix(2,3)
    x$set(m)
    expect_identical(x$get(), m)
})

test_that("makeCacheMatrix(m) sets/gets the cached inverse", {
    x <- makeCacheMatrix()

    expect_identical(x$getInverse(), NULL)

    ## bogus inverse, just for unit testing
    im <- matrix(1,1)
    x$setInverse(im)
    expect_identical(x$getInverse(), im)
})

test_that("cacheSolve(x) uses the cached inverse", {
    x <- makeCacheMatrix()

    ## mock a cached inverse and expect to get it back
    im <- matrix(2,2)
    x$setInverse(im)
    expect_identical(cacheSolve(x), im)
})

test_that("cacheSolve(x) caches a newly computed inverse", {
    ## expected inverse
    im <- matrix(c(-2, 1, 1.5, -0.5), nrow=2)
    m <- matrix(1:4, nrow=2, ncol=2)
    x <- makeCacheMatrix(m)
    expect_equal(cacheSolve(x), im)
})

