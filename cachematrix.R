## The makeCacheMatrix functions creates an S3 object that can be used in the 
## cacheSolve function which finds and stores the inverse of the matrix

## makeCacheMatrix creates an object to be used in cacheSolve to store inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y){
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) i <<- inverse
    getinv <- function() i    
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}




## takes makeCacheMatrix object, returns and stores inverse

cacheSolve <- function(x, ...) {
    i <- x$getinv()
    if(!is.null(i)){
        message('getting cached data')
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}

## Testing
# m <- matrix(c(3, -1, 2, 5, 1, 0, -2, 3, 4),nrow = 3, byrow = TRUE)
# a <- makeCacheMatrix(m)
# cacheSolve(a)
# cacheSolve(a)