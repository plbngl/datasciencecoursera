## The first function (makeCacheMatrix) is a container to store a matrix and its inverse. 
## Both can be written and read via the embedded set and get functions.
## The second function (cacheSolve) calculate the inverse of the matrix or, if has been already calculated, it retrieves it
## from the cache, thus saving computational resources.


## This first function creates an object that can store and give back values from a given matrix.
##It contains 4 functions:
##set(): Sets the Input matrix (x).   
##get(): Returns the Input matrix
##setinv(): Sets the inverse (inv) of the matrix (x) in the variable m. It overwrites the m in the parent environement
## using <<-
##getinv(): Returns the inverse of the matrix m


makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## The second function uses the object created by makeCacheMatrix (x) as argument.
## First it gets the inv variable from x (x$getinv()),
## if it is present ( or not null), it returns a message and the saved value (return (i)).
## if is not present, it uses the solve() function to calculate the inverse of the matrix, 
## stores it in the matrix object (x$setinv(i))
## and returns the calculated value. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}