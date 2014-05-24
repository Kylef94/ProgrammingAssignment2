## The following functions use a special matrix object which takes
## a matrix as an argument and is capable of calculating the inverse
## of this matrix while caching the result to avoid repeat calculations



## makeCache matrix is responsible for creating the special matrix object
## and contains the various functions related to the matrix including calculating the inverse



makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL ## sets the cache to null each time the function is initialised
        
        
        
        set <- function(y) {  ##takes a matrix, puts it into x and empties the Cache
                x <<- y       ##emptying the cache ensures getinverse doesn't print a  
                inv <<- NULL  ##cache of an old matrix we have yet to calculate the inverse of.
                
        } 
        
        
        get <- function() x ## Prints the matrix to the console
        
        setinverse <- function(inverse) inv <<- inverse ## allows the user to set the inverse, or for it to be set up other functions
        
        inverse <- function(){ ## uses solve to calculate the inverse of the given matrix, then caches it
                
                j <- x
                j <- solve(j) 
                inv <<- j
        } 
        
        getinverse <- function() inv #prints the cached inverse we calculated
        
        list(set = set, get = get,
             inverse = inverse,
             getinverse = getinverse) #defines the list of functions created by makeCacheMatrix
}





## cachesolve first checks if the inverse has been cached, if so it simply prints the inverse,
## if not it calculates and caches it.



cacheSolve <- function(x, ...) { 
        
        inv <- x$getinverse() ##fills inv with the contents of makeCacheMatrix's cache
        
        
        if(!is.null(inv)) { ##if inv is filled with anything other than Null, it simply prints the cache's contents
                
                message("Retrieving cached data")
                return(inv)
                
        }
        
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv) ##if inv contains null, cacheSolve will calculate the inverse itself, then cache and print it.
        inv
        
}
