## These 2 functions are used to store a matrix and cache its inverse. The first function 'makeCacheMatrix' 
## creates a list of four functions that we will use, and initializes the variable used to store the cache.
## The second function 'cacheSolve' checks the value of i, the cached inverse, and returns either what's already
## there or calculates the inverse and returns that

## This function creates a special list holding the functions we will need to call to retrieve(get) and store(set)
## the matrix and its inverse. It stores or caches the inverse in a variable i which is initialized to NULL

makeCacheMatrix <- function(x = matrix()) {
    
    ## Special variable to hold the cached inverse
    i <- NULL
    
    ## The 'set' function replaces x with a new matrix and resets the inverse to NULL, so that when 
    ## cacheSolve is called, i is NULL, so the inverse is recalculated with solve()
    ## we only need to call the 'set' function if we want to change the matrix
    ## of course we could also just run makeCacheMatrix again with a new matrix
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    ## This function simply returns the matrix passed to makeCacheMatrix using argument x
    get <- function() x
    
    ## This function replaces i (the stored inverse) with the argument 'inverse'
    setinverse <- function(inverse) i <<- inverse
    
    ## This function simply returns the stored inverse (in variable 'i')
    getinverse <- function() i
    
    ##Return a special list containing 4 functions get,set,getinverse and setinverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## This function gets the stored inverse from i (it either is NULL or contains a previously calculted matrix)
## If it is not NULL, we simply return the value i, If it is NULL, calculate the inverse of x using solve() and
## store it in i and return that value

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    i <- x$getinverse()
    
    if(!is.null(i)) {
        message("getting cached data")
        ## Exit function and return i (cached inverse)
        return(i)
    }
    
    ## Else, calculate the inverse, store it in i, and return that value
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
   
}

## In action...

## Create two invertible matrices
matrix1 <- matrix(c(4,3,3,2),2,2)
matrix2 <- matrix(c(1,0,5,2,1,6,3,4,0),3,3)

a <- makeCacheMatrix(matrix1)
cacheSolve(a)

## Call again to return cached inverse
cacheSolve(a)

## Change matrix using 'set' function
a$set(matrix2)

cacheSolve(a)

## Again, this time will return cached value
cacheSolve(a)
