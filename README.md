# This branch:

The idea is that the user can put R code in a file called "profile" in the directory `man-roxygen`, which is courced before executing any roclet.
The main purpose is to be able to register custom parsers either srcref or preref, which is useful for example:

```

mySetClass(Class, ...){
	setClass(Class, ..., contains='MySuperClass')
}
```

in `man-roxygen/profile`:

```
# register parser for `mySetClass` statements as an alias to the default one defined for `setClass` statements
register.srcref.parser('mySetClass', 'setClass')
register.srcref.parser('myOtherGenerator', function(call, env){ 
	# init result list
	res <- list()

	# fill res with default roxygen tags
	# e.g.,:
	res$name <- 'blabla'
	res$slot <- 'One common slot'

	# return res
	res
})
```

