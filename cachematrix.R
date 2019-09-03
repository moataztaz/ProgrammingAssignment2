## these functions are used to cache and calculate the inverse of a matrix.


## This function creates an object that can cache the inverse of the matrix 

makeCacheMatrix <- function(x = matrix()) {
	##declare the inverse matrix 
	inv <- NULL
	##set is to set(or cache) the original matrix and declares the inverse matrix 
	set <- function(y){
		x <<- y
		inv <<- NULL
	}
	##get is to return the original matrix
	get <- function() x
	##setInverse is to set(or cache) the inverse matrix
	setInverse <- function(solveMatrix) inv <<- solveMatrix
	##getInverse is to return the inverse matrix
	getInverse <- function() inv
	##return a list of all the declared variable to use to get and cache the matrix 
	##and its inverse in the next function
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the special “matrix” returned by makeCacheMatrix above 
## If the inverse has already been calculated then retrieve the inverse from cache and return it

cacheSolve <- function(x, ...) {
	##get the inverse of x , if it exists in cache then inv is not null 
	##so the function will end with return(inv)
	inv <- x$getInverse()
	if(!is.null(inv)){
		return(inv)
	}
	##data recieves x
	data <- x$get()
	##inv recieves the inverse of x with the solve function
	inv <- solve(data)
	##cache the inverse of x
	x$setInverse(inv)
	##return the inverse of x
	inv      
}
