# This function creates a special "matrix" object that can cache its inverse.

# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  setmatrix <- function(y) {
    x <<- y
    inv <<- NULL
  }
  getmatrix <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(setmatrix=setmatrix, getmatrix=getmatrix, setinverse=setinverse, getinverse=getinverse)
}




# This function computes the inverse of the special matrix returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), then cacheSolve 
# should retrieve the inverse from the cache.



# This function assumes that the matrix is always invertible.
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

## Sample run:

# my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
#my_matrix$getmatrix()
##[,1] [,2]
##[1,]    1    3
##[2,]    2    4

# my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
#  my_matrix$getinverse()
##NULL

#cacheSolve(my_matrix)
### [,1] [,2]
### [1,]   -2  1.5
### [2,]    1 -0.5
# cacheSolve(my_matrix)
### getting cached data
### [,1] [,2]
### [1,]   -2  1.5
### [2,]    1 -0.5
# my_matrix$getInverse()
### [,1] [,2]
### [1,]   -2  1.5
### [2,]    1 -0.5
# my_matrix$setmatrix(matrix(c(2, 2, 1, 4), 2, 2))
# my_matrix$getmatrix()
### [,1] [,2]
### [1,]    2    1
### [2,]    2    4
# my_matrix$getinverse()
### NULL
