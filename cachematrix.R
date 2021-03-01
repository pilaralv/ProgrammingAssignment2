## The functions below will calculate the inverse of a matrix,
## add it to the cache, and return cached value if matrix inverse
## already computed. 

## Making the matrix and setting inverse

makeCacheMatrix <- function(x=matrix()){
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function () x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set=set, get=get,
             setinverse=setinverse,
             getinverse=getinverse)
} 


## Getting the inverse through solve function or cache

cacheSolve <- function(x,...){
        inv <- x$getinverse()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data,...)
        x$setinverse(inv)
        inv
}