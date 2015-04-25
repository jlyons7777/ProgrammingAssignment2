## These functions take a square matrix as an input and calculate the 
## inverse of the matrix and store that inverted matrix along with the 
## original for future use.

## This function takes a square numeric matrix as input and stores the input
## matrix along with the inversion of that matrix as a system variable for 
## future use

makeCacheMatrix <- function(x = matrix()) {
	m<-NULL
	set<-function(y)  {
		x<<-y
		m<<-NULL
	}
	get<-function() x
	setinv<- function(solve) m<<-solve
	getinv<- function() m
	list(set=set, get=get,
		setinv=setinv,
		getinv=getinv)

}


## This function uses the output of the makeCacheMatrix function to get the
## inverse of a matrix by first checking to see if the inverse has already 
## been calculated and then either returning the stored value if available
## or calculating the inverse if it has not been previously calculated.

cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinv(m)
        m
}