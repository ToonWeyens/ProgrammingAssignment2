## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.
## Have a look at https://stackoverflow.com/a/2630222 for more info

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL # inverse
  set <- function(y) {
    x <<- y # set matrix in parent
    inv <<- NULL # unset inverse (in case it's been set)
  }
  get <- function() x
  setInv <- function(newInv) inv <<- newInv
  getInv <- function() inv
  # return all methods
  list(set=set, get=get, setInv=setInv, getInv=getInv)
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then cacheSolve should retrieve the inverse from the
## cache.
## Have a look at https://stackoverflow.com/a/2630222 for more info

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  tic <- Sys.time()
  
  inv <- x$getInv()
  if (!is.null(inv)) {
    message("getting cached data")
  } else {
    inv <- solve(x$get())
    x$setInv(inv)
  }
  
  toc <- Sys.time()
  print(toc-tic)
  
  inv
}

## Test implementation

testCacheSolve <- function(mySize = 100, mySeed = 5, myTol = 1E-10) {
  set.seed(mySeed)
  m = makeCacheMatrix()
  m$set(matrix(rexp(mySize^2), mySize, mySize))

  print('No cache should be triggered')
  mInv <- cacheSolve(m)
  print('Doing it again should trigger the cache')
  for (id in 1:5) {
    mInv2 <- cacheSolve(m)
  }
  
  # check whether cached value is identical to calculated value
  # This should be identical up to machine precision
  stopifnot(sum(!mInv == mInv2)==0)
  
  # check inverse
  stopifnot(norm(m$get()%*%mInv-diag(mySize)) < myTol)
  
  print('Executed successfully')
}