exclude <-  function(blah) {
  "This function is used to add legible multi-line comments to the code"
}

exclude({
  "Coursera R Programming - rprog-011
  2015 February 
  Programming Assignment 2: Lexical Scoping

Testing the assignment functions
--------------------------------
After running this script, the code below can be pasted onto the console command line to test the functions in this assignment.

If you are using RStudio you can highlight the following statements and press CTRL-ENTER or click the Run button to run the tests.


sprintf('Testing the [makeCacheMatrix] and [cacheSolve] functions.')
sprintf('-------------------------------------------------------------------------------------------------------------------------')
sprintf('1. Calling [makeCacheMatrix] to create a random number matrix with 1,000 rows and 1,000 columns.')
system.time(mt <- makeCacheMatrix(matrix(rnorm(1000000),1000,1000)))
sprintf('---------------------------------------------------------')
sprintf('2. First call to [cacheSolve] to retrieve the inverse of the matrix.  Note the long execution time.')
system.time(mti <- cacheSolve(mt))
sprintf('---------------------------------------------------------')
sprintf('3. Second call to [cacheSolve] to retrieve the inverse of the matrix.  Note the reduced execution time.')
sprintf('   The lower execution time confirms that the matrix inverse is being retrieved from the function cache.')
system.time(mti <- cacheSolve(mt))
sprintf('---------------------------------------------------------')
sprintf('4. In this following step we are comparing the matrix inverse retrieved from cache to the inverse of the original matrix generated.')
sprintf('   When the [identical()] R function returns TRUE then we know all the elements in both the cached inverse matrix and the matrix inverse calculated on the fly are the same.')
system.time(compareResult <- identical(mti, solve(mt$get())))
sprintf('The comparison identical(mti, solve(mt$get())) returned --> [%s]',compareResult)
sprintf('-------------------------------------------------------------------------------------------------------------------------')
sprintf('5. In this step we change the value of the matrix generating a new random number matrix.')
sprintf('   This step verifies that after generating a new matrix, the function will recalculate the inverse instead of using the old cached copy.')
mt <- makeCacheMatrix(matrix(rnorm(1000000),1000,1000))
system.time(compareResult <- identical(cacheSolve(mt), solve(mt$get())))
sprintf('The comparison of the updated matrix inverse using identical(cacheSolve(mt), solve(mt$get())) returned --> [%s]',compareResult)
sprintf('---------------------------------------------------------')


Assignment Requirements Satisfied
---------------------------------
  
1. Author a function called [makeCacheMatrix:] This function creates a special matrix object that can cache its inverse.

2. cacheSolve: This function computes the inverse of the special matrix returned by makeCacheMatrix above. 
    If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

3. Computing the inverse of a square matrix can be done with the solve function in R. For example, if X is a square invertible matrix, then solve(X) returns its inverse.

Assumptions
-----------
  For this assignment, assume that the matrix supplied is always invertible."

})

# [makeCacheMatrix:] This function creates a special matrix object that can cache its inverse.
makeCacheMatrix <- function(m = matrix()) {
  #initialize the object that stores the inverse of the matrix to NULL when the matrix object is instantiated
  mi <- NULL
  
  set <- function(nm) {
    #assign a new value to the matrix object stored in makeCacheMatrix
    m <<- nm
    #initialize the cache of the matrix inverse to a blank matrix when the matrix  value is updated
    #  note that the inverse is not calculated since in R we like to do the work only when it is required
    mi <<- matrix()
  }
  
  #return the matrix stored in makeCacheMatrix
  get <- function() m 
  
  #store the value of the matrix inverse in the cache
  setinverse <- function(minverse) mi <<- minverse
  
  #retrieve the value of the matrix inverse from the cache.
  #If not yet stored then the initial value of NULL is returned.
  getinverse <- function() mi
  
  #return a list with the handle to the four functions used to manipulate this object
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#cacheSolve: This function computes the inverse of the special matrix returned by makeCacheMatrix above. 
cacheSolve <- function(m, ...) {
  #retrieve the current matrix inverse cached value
  mi <- m$getinverse()
  
  #if the cached value is not null then it means the inverse of the matrix was calculated and cached since it was last updated.
  if(!is.null(mi)) {
    ## Uncomment the following message when debugging to ensure cached data is being retried instead of being recalculated
    ## message("getting cached data")
    
    #return the cached matrix inverse
    return(mi)
  }
  #get the value of the matrix that was last stored in the makeCacheMatrix function
  data <- m$get()
  
  #calculate the inverse of the matrix.  this is the expensive operation that is only executed when the inverse is first requested via a call to this function
  mi <- solve(data, ...)
  
  #store the inverse of the matrix in the cache of makeCacheMatrix
  m$setinverse(mi)
  
  #return the inverse of the matrix that was just calculated and stored in the cache
  mi
}
