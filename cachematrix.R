## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# 1. Set the value of the matrix
# 2. Get the value of the matrix
# 3. Set the value of the mean
# 4. Get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv<- function(inverse) inv <<- inverse
    getinv<- function() inv
    list(set= set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function
# it does the inverse of the matrix created with the function above. It checks if the 
#invese has already been calculated. If so, it gets the inverse from the cache. In our case
# it calculate the inverse of a matrix and sets the value of the inverse in the case
# via the setinv function

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv<- solve(data, ...)
    x$setinv(inv)
    inv
}

#final version

