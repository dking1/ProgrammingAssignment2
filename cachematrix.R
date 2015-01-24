## Put comments here that give an overall description of what your
## functions do

## This is creating a dataframe with functions to set, get, setsolve, getsolve, for a matrix.

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL  # this removes any old solve 
        set <- function(y) {#this function sets the new matrix and removes the old cache solve
                x <<- y
                s <<- NULL
        }
        get <- function() {# this function returns the matrix
                return(x)
                
        }
        setsolve <- function(solve) {#This sets the solve value for the matrix
                s <<- solve
        }
        
        getsolve <- function() {# this gets the cached solve value for the matrix
                return(s)                 
        }
        # this actually creates the data set containing the functions
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

cacheSolve <- function(x, ...) {#this function caches the solve
        s <- x$getsolve() #this gets the cached solve value from the matrix that is contained in the data set
        if(!is.null(s)) {# this checks if there is a cached value, if there is it grabs and returns the value
                message("getting cached data")
                return(s)
        }
        data <- x$get()# if there is no cached value already this gets the value for the matrix
        s <- solve(data, ...)# this calculates the inverse of the matrix
        x$setsolve(s)# this caches the inverse of the matrix
        s
}