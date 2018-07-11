## here we need to cach matrix inverse to save time and pc 
##resources in every time we calculate same inverse

##to check if matrix is invertable
install.packages("matrixcalc")
library(matrixcalc)

##  This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  if (!is.singular.matrix(x)){
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
  }
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above, 
## and retrieve it from cache if matrix not changed

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  
  if (!is.null(inv)) {
    message("Getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}

#for testing
mat <- matrix(rnorm(9),3,3)
mat2 <- makeCacheMatrix(mat)
cacheSolve(mat2)

