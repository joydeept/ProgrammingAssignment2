## Programming Assignment 2
## 
## Write the following functions:
## 1.makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## 2.cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
## IMPORTANT: assume that the matrix supplied is always invertible.


## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  
  cached_inverse <- NULL
  
  ### calling set will:
  ### 1. store the matrix in the argument in the function's "local" object x
  ### 2. reset the cached inverse object.
  set <- function (m) {
    x <<- m
    cached_inverse <- NULL
  }
  
  ### calling get will simply return the value of the function's local object x
  get <- function () x
  
  ### calling setInverse will cache the passed in argument (assuming it is the inverse)
  setInverse <- function (inv) cached_inverse <<- inv
  
  ### calling getInverse will return the value of the cache
  getInverse <- function () cached_inverse
  
  ### list returns the four functions in this function vector
  list (set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}



## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve will retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  m <- solve(x$get())
  
  x$setInverse(m)
  
  m
  
}


### Testing if this works

### create a new matrix and get the inverse
### 
### > mat = makeCacheMatrix(matrix(c(1,2,3,4,1,6,7,8,1), 3,3))
### > cacheSolve(mat)
### [,1]        [,2]        [,3]
### [1,] -0.45192308  0.36538462  0.24038462
### [2,]  0.21153846 -0.19230769  0.05769231
###[3,]  0.08653846  0.05769231 -0.06730769

### get the inverse again without changing the matrix.
### should return the cached inverse.
###
### > cacheSolve(mat)
### getting cached data
### [,1]        [,2]        [,3]
### [1,] -0.45192308  0.36538462  0.24038462
### [2,]  0.21153846 -0.19230769  0.05769231
### [3,]  0.08653846  0.05769231 -0.06730769

### change the matrix and get the inverse.
### should reset cache and recalculate the inverse
###
### > mat = makeCacheMatrix(matrix(c(1,2,2,1), 2,2))
### > cacheSolve(mat)
### [,1]       [,2]
### [1,] -0.3333333  0.6666667
### [2,]  0.6666667 -0.3333333

### End test