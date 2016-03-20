## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix: This function creates a special “matrix” object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
     x <<- y
     m <<- NULL
  }
  get <- function() x
  setcachedcalc <- function(solve) m <<- solve
  getcachedcalc <- function() m
  
  list(set = set, get = get,
       setcachedcalc = setcachedcalc,
       getcachedcalc = getcachedcalc)

}


## cacheSolve: This function computes the inverse of the special
## “matrix” returned by makeCacheMatrix above. If the inverse has
## already been calculated (and the matrix has not changed), then
## cacheSolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {

  m <- x$getcachedcalc()
  
  if(!is.null(m)) {
    message("returning cached m")
      return(m)
  }
  
  message("no m in cache - caching x")
  
  data <- x$get()
  m <- solve(data, ...)
  x$setcachedcalc(m)
  m
}

## to test
## source("cachematrix.R")
## a <- diag(2,2)
## b <- diag(3,3)
## ca=makeCacheMatrix(a)
## cb=makeCacheMatrix(b)
## ica=cacheSolve(ca)
## ica=cacheSolve(ca)
## icb=cacheSolve(cb)
## icb=cacheSolve(cb)
