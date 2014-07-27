#Takes a matrix and returns a list.
makeCacheMatrix <- function(cacheMe = matrix()) 
{
	cachedMatrix <- NULL
	
	#Workhorse functions in the object.
	set <- function (matrix)
	{
	  cacheMe <<- matrix
	  cachedMatrix <- NULL
	}

	get <- function ()
	{
	  return(cacheMe)
	}

	setInversion <- function (cache)
	{
	  cachedMatrix <<- cache
	}

	getInversion <- function ()
	{
	  return(cachedMatrix)
	}

	list(set = set, get = get, setInversion = setInversion, getInversion = getInversion)
}


#Checks to see if a function has already been cached. If it has, return the value. If not, cache it.
cacheSolve <- function (x, ...)
{
	result <- x$getInversion()
	if (!is.null(result))
	{
	  message("Obtaining cache")
	  return(result)
	}

	message("Generating cache.")
	result <- solve(x$get(), ...)
	x$setInversion(result)
	return(result)
}