## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates functions to cache the matrix passed into it with set and get
makeCacheMatrix <- function(myMatrix = matrix()) {
        
        stored_matrix <- NULL            ## Initialize stored matrix to NULL
        
        set <- function(y) {
                myMatrix <<- y           ## Setting value of the matrix when function cacheSolve is called
                stored_matrix <<- NULL   ## Setting stored matrix in the parent environment to NULL
        }
        get <- function() {
                return(myMatrix)        ## Returns the stored matrix when called
        }
        
        setsolve <- function(new_Matrix){
                stored_matrix <<- new_Matrix   ## Set value of stored matrix
        }
        
        getsolve <- function(){
                return(stored_matrix)      
        } 
        ## Returns a list of functions from above with cached data
        list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
        
}


## Write a short comment describing this function
## cacheSolve() function works together with makeCacheMatrix() 
  ## First, we have to call makeCacheMatrix() and pass a matrix to it -> it will store a matrix (cached)
  ## Next, we call cacheSolve() for the first time and it will attempt to "find" a previously computed inverse matrix
     ## if there is no cached data, it will compute by solve()
  ## Lastly, when we call cacheSolve() for the 2nd time, it will return the cached result instead of computing it

## cacheSolve(my_matrix) Return a matrix that is the inverse of the matrix as the args passed into it

cacheSolve <- function(my_matrix, ...) {
        
        inverse_matrix <- my_matrix$getsolve() ## Calling 'getsolve' function to get matrix (if any)
        
        if(!is.null(inverse_matrix)) {        ## Checking if there is a cache matrix, skip calculations
                message("getting cached data")
                return(inverse_matrix)                    
        }
        
        data <- my_matrix$get()             ## Get the matrix
        
        inverse_matrix <- solve(data, ...)  ## computes inverse matrix using 'solve' function
        
        my_matrix$setsolve(inverse_matrix)  ## Set the computed inverse matrix, now it's cached, can just call it next time without computation
        
        return(inverse_matrix)              ## Returns computed inverse matrix
}
