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
# register parser for mySetClass
register.srcref.parser('mySetClass', roxygen2:::parse_class)
```

