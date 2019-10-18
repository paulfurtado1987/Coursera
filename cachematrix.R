# The purpose of these functions is to cache the matrix inverse calculation 
# results for possible us in the future

# The functions do this by first creating a special "matrix" function which is a list containing
# functions to 1. set the value of the vector 2.get the value of the vector
# 3.set the value of the inverse and 4.get the value of the inverse. 

# The next function utilizes the results of the first matrix and checks if the inverse has been
# calculated. If it has been calculated, it will get the inverse from the cache and skip the 
# calculation. Otherwise it will calculate the inverse and set the value of the inverse in the 
# cache

## The function makeCacheMatrix accepts a square invertible matrix as an input and returns a 
# list containing functions to 
# 1.set the value of the vector 
# 2.get the value of the vector
# 3.set the value of the inverse 
# 4.get the value of the inverse.
# The function checks if the matrix passed to the function while calling the function or while
# using the inbuilt set function has already been stored in its cache

makeCacheMatrix <- function(x = matrix()) {
      
      if(!exists("x2")) x2 <<- matrix()    # check if cache variable x2 exists, else create a new placeholder
      
      if(!identical(x2,x)){                # check if the matrix passed to makeCacheMatrix is identical to the previous matrix  
        i<-matrix()                        # if not the initialize the empty inverse matrix(will be checked to make sure cache is empty) 
      }
      x2 <<- x                             # set the cache to the new matrix
      
      set <- function(y){                  # set function assignes new matrix to x and resets the i matrix
        if(!identical(y,x)){               # check if the matrix passed to makeCacheMatrix is identical to the previous matrix  
          i<<-matrix()                     # if not the initialize the empty inverse matrix(will be checked to make sure cache is empty)
        }
        x<<-y                              # set the cache to the new matrix
      }
      
      get <- function() x                  #returns the value of the x matrix
      
      setinverse <- function(inv) i<<-inv  #sets the inverse matrix to the variable i
      
      getinverse <- function() i           # returns the inverse matrix
      
      list(set = set, get = get,
           setinverse = setinverse, getinverse = getinverse)  #returns a list cotaining the function(set, get, setinverse, getinverse)
}

 
# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), then cacheSolve 
# should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      i <- x$getinverse()                      # retrieve the inverse matrix from makeCacheMatrix
      
      if(!all(is.na(i))){                      # check if cache is not empty
          message("getting cached data")       # output message 
          return(i)                            # return cached data
      }
      
      z <- x$get()                             # if cache is empty, get the matrix
      
      i <- solve(z)                            # calculate the inverse
      
      x$setinverse(i)                          # set the inverse into the cache
      
      i                                        # return the inverse
}
