## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Assign a matrix to be stored with the set function
## Use the get function to return stored matrix
## Use setinverse function to store another matrix
##   We are calling this the inverse, but there are no checks to validate
## Return the stored inverse matrix with the getinverse function
## The code: m = makeCacheMatrix(b), will store matrix b
makeCacheMatrix <- function(x = matrix()) {
    # We are not going to access x directly, and therefore it
    # will become a promise. Promises are evaluated the first time
    # and then accessed after evaluation
    m <- NULL
    
    set <- function(y) {
        # store matrix in x
        x <<- y
        # Have to reset m, or will return obsolete cache
        m <<- NULL
        message('Setting Matrix')
    }
    
    get <- function (){
        message('Getting Matrix')
        # simply return x (stored matrix)
        x
    }
    
    setinverse <- function(inverse){
        message('Setting inverse')
        # store the inverse of the matrix
        # this really simply stores any matrix
        # it is up to the calling function to assign correct matrix
        m <<- inverse
    }
    
    getinverse <- function(){
        message('Getting inverse')
        # return the matrix stored with the setinverse function
        m
    }
    
    list(set = set, 
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  
}


## Write a short comment describing this function
## Gets the inverse matrix stored in makeCacheMatrix.
## If no inverse is stored, then it calculates the inverse and uses
## makeCacheMatrix$setinverse to store the inverse, which be later be retrieve with getinverse()
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    # Gets inverse from above function
    m <- x$getinverse()
    print(m)
    # if it is not null, then it has been calculated before
    # simply return the inverse
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    # Has not been evaluated, so store the 
    # inverse in the above function scope
    #Get the matrix
    data <- x$get()
    # Calculate the inverse
    m <- solve(data)
    x$setinverse(m)
    m
}
