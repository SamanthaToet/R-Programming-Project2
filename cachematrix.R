## Matrix inversion is a costly computation, so we are going to write two 
# functions to cache the inverse of a matrix, rather than compute it repeatedly.
# These functions take advantage of Lexical Scoping rules in R and manipulate 
# them to preserve the state inside of an R object. This assignment uses the 
# scenario of needing to cache an inverted matrix as a way of illustrating how 
# this might be done with a special matrix object that stores its inverse as an 
# in-memory object.


## Make sure you're in the right working directory 
# (i.e. where you cloned the forked Repo):
setwd("C/Users/stoet/Desktop/Data Science")


## The first function we're going to write, makeCacheMatrix, creates a special 
# "matrix" object that can cache its inverse. This special "matrix" object is 
# actually a list containing four functions to:
# (1) set() sets the value of the matrix
# (2) get() gets the value of the matrix
# (3) setinverse() sets the value of the inverse matrix
# (4) getinverse() gets the value of the inverse matrix


makeCacheMatrix <- function(x = matrix()) {  #x is initialized as a function argument 
    i <- NULL                                #i is null obj. w/in makeCacheMatrix() envi. to be used later
    set <- function(y) {
        x <<- y                              #assigns y to x in parent envi. 
        i <<- NULL                           #clears previous cache (), assigns NULL to i in parent envi. 
    }
    get <- function() x                      #return matrix x (retrieved from parent envi.)
    setinverse <- function(inverse) i <<- inverse #i is defined in parent envi., but we need it after setmean()
                                             #so we assign the input argument to m in the parent envi.
                                             #setinverse() sets the inverse, i, to the variable, inverse
    getinverse <- function() i               #returns the inverse, i
    list(set = set,                          #stores named functions in a list (so we can use $ vs. [[]] later on)
         get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)
    
}                                           #returns the special "matrix" containing all functions defined


## The next function, cacheSolve, computes the inverse of the special "matrix" returned by
# makeCacheMatrix above. If the inverse has already been calculated (and the
# matrix has not changed), then the cachesolve should retrieve the inverse
# from the cache. 

cacheSolve <- function(x, ...) {            #retrieves inverse from obj. of type makeCacheMatrix
    i <- x$getinverse()                     #gets inverse of input, stores as i
    if (!is.null(i)) {                      #checks to see if i is null (makeCacheMatrix sets cached inverse to NULL)
        message("getting cached data")      #if i is not null (i.e. returns TRUE) we have valid cached inverse
        return(i)                           #and can return it to parent envi.
    }
    data <- x$get()                         #if i is null (i.e. returns FALSE) cacheSolve gets matrix from input
    i <- solve(data, ...)                   #calculates inverse, stores in i
    x$setinverse(i)                         #uses setinverse() on input obj. to set the inverse in input obj.
    i                                       #and then returns the value of inverse to parent envi,
}

## Note that cacheSolve() is the only place wehre solve() is executed, which is why
# makeCacheMatrix is incomplete without cacheSolve(). Woo hoo for Lexical Scoping!

## Testing:

# m is sample matrix, with inverse n:
m <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
n <- solve(m)

m1 <- makeCacheMatrix(m)
cacheSolve(m1) #should return inverse (i.e. be the same as n)


