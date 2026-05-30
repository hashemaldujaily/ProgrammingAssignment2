## This is a pair of two functions with the main goal to cache the inverse of a matrix.
## How it works: A matrix is first supplied to the first function, which stores it in a special object along with a placeholder for its inverse. 
## The second function is then used to either compute the inverse of this matrix or retrieve it from the cache if it has already been calculated
##  and the matrix has not changed.

## DESCRIPTION OF THE TWO FUNCTIONS:
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
                        matrixinverse <- NULL
                        set <- function(y) {
                                x <<- y
                                matrixinverse <<- NULL
                        }     
                        get <- function() x 
                        setinverse <- function(inv) matrixinverse <<- inv 
                        getinverse <- function() matrixinverse  
                        list(   set = set,  get = get,
                             setinverse = setinverse,
                             getinverse = getinverse)
                }

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        matrixinverse <- x$getinverse()
        if(!is.null(matrixinverse)) {message("getting cached data")
                                     return(matrixinverse)
                                    }
        data <- x$get()
        matrixinverse <- solve(data, ...)
        x$setinverse(matrixinverse)
                        matrixinverse
        }
        ## Return a matrix that is the inverse of 'x'
        ## I chose a 2x2 matrix with data of "1, 2, 3, 4". 
        ## This Should return the following result:
        ##     [,1] [,2]
        ## [1,] -2  1.5
        ## [2,]  1 -0.5
        m <- matrix(c(1,2,3,4), nrow = 2)
        makeCacheMatrix(m)
        cacheSolve(makeCacheMatrix(m))
        
