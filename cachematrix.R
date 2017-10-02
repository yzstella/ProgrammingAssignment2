## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix is to create a special object that stores a matrix and chaches its inverse:
##set the value of matrix -- get the value of the matrix -- set the value of the inverse -- get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## cacheSolve computes the inverse of the special matrix returned by makeCacheMatrix above. If the inverse has already been
## calculated, then retrive the inverse from the cache

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
        ## Return a matrix that is the inverse of 'x'
}
