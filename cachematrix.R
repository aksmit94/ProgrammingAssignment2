## This contains twp functions, one of which takes a non-singular matrix as an input and returns a list of 4 functions;
## the second one takes the output of the first function, either calculates and returns the inverse of that matrix or displays the cached value.


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL           #Initializing the inverse value as NULL
  set <- function(y) {  # function to change the input matrix   
    x <<- y
    inv <<- NULL
  }
  
  get <- function() {   # function to return the input matrix of makeCacheMatrix()
    x
  }
  
  setinv <- function(inverse) { # function to set the calculated inverse value from cacheSolve into inv 
    inv <<- inverse        #superassignment operator
  }
  
  getinv <- function() { # function to get the inverse value stored in inv
    inv
  }
  list( set = set, get = get, setinv = setinv, getinv = getinv) # output list returned by makeCacheMatrix()
}


cacheSolve <- function(x, ...) { 
  inv <- x$getinv()      # Setting in inv the inverse value from the above function
  if(!is.null(inv)) {    # checks if inv has stored value of inverse of x i.e. the inverse of x has already been calculated
    message("getting cached data")
    return(inv)          # Returns inv and exits the function cacheSolve
  }
  # when inv is NULL :
  mat <- x$get()         # store original input matrix in a variable mat
  inv <- solve(mat)      # solve() finds inverse of x and stores it in inv
  x$setinv(inv)          # setting value of inv (still NULL in upper function) equal to calculated inverse value
  inv  ## Return a matrix that is the inverse of 'x'
       
}
