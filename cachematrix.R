## epicenegreen 032015
## Take advantage of the scoping rules of the R language and
## how they can be manipulated to preserve state inside of
## an R object.
## Create a matrix that can be cached and have its inverse cached.

## makeCacheMatrix: This function creates a special "matrix" object
## that can cache its inverse.
makeCacheMatrix <- function(x = matrix(mode=numeric)) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setSolve <- function(solve) m <<- solve
    getSolve <- function() m
    list(set = set, get = get,
         setSolve = setSolve,
         getSolve = getSolve)

}

## cacheSolve: This function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above. If the inverse has
## already been calculated (and the matrix has not changed), then
## the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getSolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    
    ## first thought inverse meant a reversal of the matrix
    ## rev(x) reversal of a vector
    ## is.matrix tests if its argument is a (strict) matrix.
    ## vector v <- dim(x) of rows, columns
    ## for each row, rev(row)
    ## set inverse to rev(v)
    ## d <- dim(x)
    ## m <- matrix(rev(as.vector(x)), nrow = d[1], ncol = d[2]) 
    
    ## here, a matrix inverse is the mathematical inversion of 
    ## a matrix, which is achieved through function solve()
    ## for this case, a matrix inverse requires a square
    ## so dim(data)[1] must be == dim(data)[2]
    m <- solve(data)
    x$setSolve(m)
}

