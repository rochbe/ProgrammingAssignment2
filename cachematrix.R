## Caching inverse of matrix.

## Usage: -

## # Random square matrix.
## m1 <- matrix(sample(1:25), nrow = 5, ncol = 5)

## # Create object.
## cachedm <- makeCacheMatrix(m1)

## # First time calculates inverse.
## m1inv <- cacheSolve(cachedm)

## # Subsequently retrieve cached value.
## m1inv <- cacheSolve(cachedm)

## # Check that matrix multiplication of inverse and matrix
## # gives identity matrix (within rounding errors).
## cachedm$getsolve() %*% cachedm$get()

## # Random square matrix.
## m2 <- matrix(sample(1:25), nrow = 5, ncol = 5)

## # Assign to object.
## cachedm$set(m2)

## # First time calculates inverse.
## m2inv <- cacheSolve(cachedm)

## # Subsequently retrieve cached value.
## m2inv <- cacheSolve(cachedm)


## Make 'matrix' whose inverse can be cached.

makeCacheMatrix <- function(x = matrix())
{
    ## 'x' is square matrix.

    ## Return list of functions: -
    ## set - sets matrix.
    ## get - gets matrix.
    ## setsolve - sets inverse of matrix. Non-user function. Used by cacheSolve.
    ## getsolve - gets inverse of matrix. Only valid after cacheSolve used.

    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## Return matrix that is inverse of 'x'

cacheSolve <- function(x, ...)
{
    ## 'x' is makeCacheMatrix object.

    ## Return inverse Matrix.
    ## If already calculated, returns cached value.

    s <- x$getsolve()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    s
}
