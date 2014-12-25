makeCacheMatrix <- function(x = matrix()) {
  #Inicializar variable 
  m <- NULL
  #Asigna la matriz en cache 
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  #Devuelve la matriz inversa
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
  # Recibe la matriz inversa
  m <- x$getinverse()
  #valida si el calculo de la matriz inversa ya esta en cache
  if(!is.null(m)) {
    message("Matriz Inversa en cache")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
