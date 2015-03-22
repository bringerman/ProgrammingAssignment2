# Our samples from the assignment as a reference and starting point 
#makeVector <- function(x = numeric()) {
#   m <- NULL
#   set <- function(y) {
#     x <<- y
#     m <<- NULL
#   }
#   get <- function() x
#   setmean <- function(mean) m <<- mean
#   getmean <- function() m
#   list(set = set, get = get,
#        setmean = setmean,
#        getmean = getmean)
# }
# 
# 
# 
# cachemean <- function(x, ...) {
#   m <- x$getmean()
#   if(!is.null(m)) {
#     message("getting cached data")
#     return(m)
#   }
#   data <- x$get()
#   m <- mean(data, ...)
#   x$setmean(m)
#   m
# }



## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Instead of making the cache vector, we are going to apply these concepts to a matrix function

## This function is very close the vector function
## We are caching the inverse of our matrix into inv
## 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## we have to make an assumption that we can invert the matrix with this function.
## We have two code paths here 
## 1. If the inverted matrix is already cached, print that we
## are using the cached data and return inv

## 2. If not, we have to get the inverse here and return it. 

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
