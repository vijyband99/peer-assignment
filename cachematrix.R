## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to create a special object that
## stores a matrix and caches its inverse.
## This function creates a special "matrix" object that can cache its inverse.

rm(list=ls())
ls()
makeCacheMatrix <- function(x = matrix()) {
        j <- NULL
        set <- function(y){
                x <<- y
                j <<- NULL
        }
        get <- function()x
        setInverse <- function(inverse) j <<- inverse
        getInverse <- function() j
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" created by
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then it should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'#############
        
        j <- x$getInverse()
        if(!is.null(j)){
                message("VOILA CATCH DATA IS AVAILABLE")
                return(j)
        }
        mat <- x$get()
        j <- solve(mat,...)
        x$setInverse(j)
        j
}


##########Checking the Solution#########################

my_matrix <- makeCacheMatrix(matrix(rnorm(16), 4, 4))
my_matrix$get()

#########my_matrix$getInverse() produces NULL value as no computations have been done##############

my_matrix$getInverse()

cacheSolve(my_matrix)

#######inverse stored in cache########

cacheSolve(my_matrix)

######### Even though no calculations are performed but because of using cache,########### 
#########my_matrix$getInverse()provides inverse stored in cache######

my_matrix$getInverse()

####### Another Example #############

my_matrix$set(matrix(c(5, 7, 9, 11), 2, 2))
my_matrix$get()

#########my_matrix$getInverse() produces NULL value as no computations have been done##############

my_matrix$getInverse()

cacheSolve(my_matrix)

#######inverse stored in cache########

cacheSolve(my_matrix)

######### Even though no calculations are performed but because of using cache,########### 
#########my_matrix$getInverse()provides inverse stored in cache######

my_matrix$getInverse()
