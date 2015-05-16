## rprog-014 ProgrammingAssignment2
## Mark Anderson

## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a cached Matrix with getters and setters
##   for the cached matrix 'm', and it's inverse 'inverse'
##   set -- set the value of 'm'
##   get -- gets the value of 'm'
##   setinverse -- set the value of 'inverse'
##   getinverse -- gets the value of 'inverse'

makeCacheMatrix <- function(m = matrix()) {
  inv <- NULL
  set <- function(p_m) {
    m <<- p_m
    inv <<- NULL
  }
  get <- function() m
  setinverse <- function(p_inverse) inv <<- p_inverse
  getinverse <- function() inv
  list(
    set = set, 
    get = get, 
    setinverse = setinverse, 
    getinverse = getinverse
  )
}


## cacheSolve returns the inverse of a matrix.
##   if called previous, it returns cached value
##   if called for the first time, it computes inverse and caches

cacheSolve <- function(m_p) {
  inv <- m_p$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- m_p$get()
  inv <- solve(data)
  m_p$setinverse(inv)
  inv
}
