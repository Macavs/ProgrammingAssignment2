###makeCacheMatrix (a function with an argument "x" which is a matrix) generate a 
###matrix object that can cache its inverse 
makeCacheMatrix <- function(x = matrix()) {
  ###define the object where the inverse will be hold
  invemat <- NULL
  setmat <- function(y) {
    x <<- y
    invemat <<- NULL
  }
  #### retrive the matrix argument of makeCacheMatrix function
  getmat <- function() {x
  }
  ####assign a value to invemat
  setinve <- function(inverse) {invemat <<- inverse
  }
  ########retrieve value of invemat 
  getinve <- function() {invemat
  }
  ####creates a list of elements that will be refer in the next function
  list(setmat = setmat, getmat = getmat,
       setinve = setinve,
       getinve = getinve)
}



#####cacheSolve function computes the inverse of matrix returned by makeCacheMatrix function.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  ###retrieve the inverse value of the element getinve from the list obtained in the makeCacheMatrix function
  invemat<- x$getinve()
  ######if the value is not NULL (has already been calculated) return this message and the inverse
  if(!is.null(invemat)) {
    message("getting inverse value from cache")
    return(invemat)
  }
  ###If the value is NULL retrieve the matrix from the argument of makeCacheMatrix function and apply the solve function
  data <- x$getmat()
  invemat<- solve(data, ...)
  #####set the value and return the inverse value
  x$setinve(invemat)
  invemat
}

#####Testing the functions

test=c(1,9,4,7,5,3,5,8,2)
test
dim(test)=c(3,3)
solve(test)
r=makeCacheMatrix(test)
cacheSolve(r)

