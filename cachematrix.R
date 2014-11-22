
##this functions Calculate the Matrix inverse and store it in cache

##makeCacheMatrix produces a vector of 4 functions that set and get the Matrix
# to compute over, and set and get the inverse of that matrix.

makeCacheMatrix <- function(x = matrix()) {
  I <- NULL
  set <- function(y) {
    x <<- y
    I <<- NULL
  }
  get <- function() x
  setInverse  <- function(Inverse) I <<- Inverse
  getInverse  <- function() I
  list(set = set, get = get,
       setInverse  = setInverse ,
       getInverse  = getInverse )
}

# chache solve calculates and returs the inversed matrix of x
cacheSolve <- function(x, ...) {
  I <- x$getInverse ()
  if(!is.null(I)) {
    message("getting cached data")
    return(I)
  }
  data <- x$get()
  I <- solve(data, ...)
  x$setInverse (I)
  I
        ## Return a matrix that is the inverse of 'x'
}
