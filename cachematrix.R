## Put comments here that give an overall description of what your
## functions do

# These functions receive a user defined matrix, compute the inverse of the matrix 
# or grab it from the cache if has already been computed and assigned to the cache,
# then displays the inverse in the console. 

## Write a short comment describing this function
# makeCacheMatrix() creates functions and cached values that cacheSolve will 
# use to check for a cached value, calculate the inverse of the matrix
# then save the inverse of the matrix to the cache

makeCacheMatrix <- function(x = matrix()) {
  #initialize m 
  m <- NULL 
  
  # create get() function, which returns the value of x 
  get <- function() x
  
  #create the setinverse() function, with assigns the inverse to m
  setinverse <- function(inverse) m <<- inverse
  
  #create the getinverse() function, with returns the value of m
  getinverse <- function() m
  
  # make the values cached above using the <<- operator as well as   
  # the functions defined above available to the calling function, 
  # in this case, cacheSolve()
  list(get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## Write a short comment describing this function
# The cacheSolve() function uses functions and cached values defined by makeCacheMatrix()
# to apply the solve() function to the passed matrix, assign the value to the cache
# as well as display the inverse value in the console


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  # Call the getinverse() function from the passed function (in this case x = makeCacheMatrix())
  # and assign the value to m
  m <- x$getinverse()
  
  #test to see if the inverse has already been assigned to m. 
  #If so, show the message, return the value of m and end the function
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  #if the inverse has not already been assigned to m, then assign it to m
  
  #call the get() from makeCacheMatrix() to assignethe matrix values to data
  data <- x$get()
  
  #run the solve() against the matrix in data and assign to m within the scope of this function
  m <- solve(data, ...)
  
  #call the setinverse() to assign the inverse to the cached value of m
  x$setinverse(m)
  
  #display the value of m if was not previously cached
  m
  
}
