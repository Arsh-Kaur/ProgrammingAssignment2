###############################################################
##                CACHING INVERSE OF MATRIX                  ##
## Uses two functions:                                       ##
##     1.makes a special matrix that can cache its inverse   ##
##     2.computes the inverse of special matrix or caches    ##
##       the inverse if already computed                     ##   
###############################################################

## makeCacheMatrix makes a special matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	  inv<-NULL               #set inv to NULL for future use
	  m<-x                    #store matrix in a variable m

          #getting value of matrix
	  get <- function() m

	  #set value of inverse (for cache)
          setinv <- function(inverse) inv <<- inverse

	  #get value of inverse (already cached)
          getinv<- function() inv

	  #This function returns a list
          list( get = get,setinv = setinv, getinv = getinv)
}


## This function computes the inverse of special matrix made by makeCacheMatrix
## It caches the inverse if inverse has already been computed

cacheSolve <- function(x, ...) {
          #get the value of inverse
	  inv <- x$getinv()

	  #if inverse already calculated, return cached value
          if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
          }

	  #else get the matrix into new variable and compute its inverse
          data <- x$get()
          inv <- solve(data, ...)

	  #cache the value of inverse 
          x$setinv(inv)

	  ## Return a matrix that is the inverse of 'x'
          inv
}

###        EXAMPLE USAGE
#  m<- matrix(c(2, 2, 4, 3), nrow = 2, ncol = 2, byrow = TRUE)
#  cm<- makeCacheMatrix(m)
#  cacheSolve(cm)
