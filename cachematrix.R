##--- Programming Assignment 2: Lexical Scoping      ----

## This function establishes a matrix, as well as the conditions to retrieve 
## values in the parent environment for future coding. If the matrix has been
## changed, it will set m to null, so that subsequent formulas do not retrive
## incorrect cached data.
## The two functions, when combined, allow the program to retrieve a complex
## calculation from cache instead of recalulating and tying up resources.

makeCacheMatrix <- function(x = matrix()) {
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


#---------------------------------------------------
## This function first tests whether a cache of data has been established.
## If not, it inverts the matrix and stores the value in m.
## If so, it retrieves the cached data.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
  ## Return a matrix that is the inverse of 'x'
}

## ----------------------Checking the program------------------------------
#Example #1
##> my_matrix1 <- matrix(rnorm(16),4,4)
##> ex1_matrix <- makeCacheMatrix(my_matrix1)
##> cacheSolve(ex1_matrix)
##[,1]        [,2]        [,3]       [,4]
##[1,]  0.6764058  0.35618129 -0.11805740  0.1163890
##[2,] -0.1609888  0.08495711 -0.21215485  0.3236158
##[3,] -0.9819465 -0.29535739  0.89942178 -0.4055239
##[4,] -0.1261990  0.32961841  0.07070148 -0.2271219

#Example #2

my_matrix2 <- makeCacheMatrix(matrix(1:4, 2, 2))
my_matrix2$get()

##      [,1] [,2]
##[1,]    1    3
##[2,]    2    4

cacheSolve(my_matrix2)

##     [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5

