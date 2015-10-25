## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## It's a function that creates a matrix in cache with its inverse matirx. 
## Inside the function, there are 4 functions inside it, they help to set
## and get the original matrix and also creat the inverse matrix and get it.
makeCacheMatrix <- function(x = matrix()) {
    i<- NULL
  set<- function(y){
    x<<- y
    i<<- NULL
  }
  
  get<- function() x
  setinverse<- function(inverse) i<<- inverse 
  getinverse<- function() i
  list(set= set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
## This function gets the inverse matrix from the function above, if it finds out
## that there is no inverse matrix of the original one is stored in the cache, it 
## will creat one for that, and the created inverse matrix will be stored in the 
## cachematrix created from the function above
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i<- x$getinverse()
  if(!is.null(i)){
    message("getting cache data")
    print(i)
    return(i)
  }
  
  data<- x$get()
  i<- solve(data)
  x$setinverse(solve(data))
  i
  
}

## sample is used to create a series of integers
a<- sample(1:100,9)
## an integer matrix is formed
x<- matrix(a,3,3)
## create the cachematrix with the first function
t<- makeCacheMatrix(x)
## use the second function to get the inverse matrix in cache, if it can't find one
## it creates one and store it in the cachematrix
ca<- cacheSolve(t)
