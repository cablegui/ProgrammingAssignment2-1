## This assignment is used to demonstrate the basic concepts of R. Aspects like functions within functions, list of functions, 
## <<- operator to assign values to variables which are in an environment above the current environment

## makeCacheMatrix function creates an environment which holds the matrix data supplied in argument x
## It also holds the basic property functions to interact with the data for setting and getting
makeCacheMatrix <- function(x = matrix()) 
{
    
    m <- NULL
    set <- function(y) 
    {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(solver) m <<- solver
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## cacheSolve function is a closure function which calculates the inverse of the matrix set in the environment created my makeCacheMatrix function
## If the function cacheSolve is called on the same function variable then it returns the cached data rather than running solve function again 
## by checking for null object in the if clause
cacheSolve <- function(x, ...) 
{
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}
