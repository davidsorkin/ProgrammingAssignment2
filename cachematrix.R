## This code follows the example provided in the assignment except
## that it caches a matrix and its inverse instead of a vector and a mean.
## It works by using the <<- operator to store and reference variable in the
## parent environment.
##
## David Sorkin 2015-02-19 R-Programming Assignment 2

## makeCacheMatrix - takes an optional matrix as a argument and returns a
##                   list of functions to support set, get for the matrix and
##                   its inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL # set default value of NULL for inverse
              # variable only referenced if setinverse not called
    set <- function(y) {
        x <<- y    # store the new matrix in parent envirnment x
        i <<- NULL # reset inverse to null, not yet computed for a new matrix
    }
    get <- function() x  # return matrix x 
                         # x is defined either in function env or parent env.
                         # it is in function environment if passed as
                         # argument to makeCacheMatrix
                         # it is in parent if set via set method
    setinverse <- function(inverse) i <<- inverse # store computed inverse
                                                  # in parent env i
    getinverse <- function() i # return previously stored inverse
                               # checks parent and then function environment
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse
    ) # return a list of name to function mappings
}


## cacheSolve - Takes as argument a list object x returned by makeCacheMatrix()
##              and returns the inverse of the matrix stored in x.
##              If inverse has been previously computed that value is
##              returned, otherwise it is computed and the value cached in x.
##              Optional arguments ... are passed to native solve()

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x' 
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get() # retreive the stored but unsolved matrix
    i <- solve(data, ...) # solve the matrix, use of ... allows for options
                          # such as LINPACK to be set
    x$setinverse(i)  # store the result for future retreival
    i
}

#Example:
#f <- matrix(c(1,0,0,0,1,-4,0,0,1), 3, 3)
#cm <- makeCacheMatrix(f)
#print(cacheSolve(cm))
#print(cacheSolve(cm))
#print(cacheSolve(cm))
#> source('~/.active-rstudio-document')
#[,1] [,2] [,3]
#[1,]    1    0    0
#[2,]    0    1    0
#[3,]    0    4    1
#getting cached data
#[,1] [,2] [,3]
#[1,]    1    0    0
#[2,]    0    1    0
#[3,]    0    4    1
#getting cached data
#[,1] [,2] [,3]
#[1,]    1    0    0
#[2,]    0    1    0
#[3,]    0    4    1


