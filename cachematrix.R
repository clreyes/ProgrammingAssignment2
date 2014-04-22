## the following two functions are used to create a special object that
## stores a matrix and caches the inverse of that matrix once computed

## makeCacheMatrix returns a list of functions with a function to 
## set the value of a matrix,
## get the value of the matrix,
## set the inverse of the matrix, 
## get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    x.inv <- NULL
    set <- function(y) {
        x <<- y
        x.inv <<- NULL
    }
    get <- function() x
    setinv <- function(inv){
        x.inv <<- inv    
    }
    getinv <- function() x.inv
    list(set=set,get=get,setinv=setinv,getinv=getinv)
}

## cacheSolve returns the inverse of 'x' from cached data if available
## otherwise, it computes the inverse of 'x' using 'solve', stores the
## inverse of 'x' in cache and prints it on the console
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    data<-x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}
