source("s4-animal-utils.R")
library(methods)

setClass("animal",
  slots = c(
    name = "character",
    weight = "numeric",
    female = "logical"
  ),
  prototype = list(
    name = make_name(),
    weight = runif(1, min = 0, max = 300),
    female = c(TRUE, FALSE)[sample(1:2, 1)]
  )
)

setValidity("animal", function(object) {
  validity_checks <- character()
  if (object@weight < 0 | object@weight == Inf | is.na(object@weight)) {
    validity_checks <- append(validity_checks,
                              "@weight must be in [0, Inf] for class animal")
  }
  if (gsub("\\s", "", object@name) == "" | is.na(object@name)) {
    validity_checks <- append(validity_checks, "animals need a 'name'")
  }
  if (length(validity_checks) > 0) {
    return(paste(validity_checks, sep = "\n"))
  }
  TRUE
})


### prey ####
setClass("prey",
  contains = "animal",
  slots = c(hide = "numeric"),
  prototype = list(hide = runif(1, min = 0, max = 1))
)

setValidity("prey", function(object) {
  # check if object is valid animal first
  is_valid_animal <- validObject(new("animal",
    name = object@name, weight = object@weight, female = object@female
  ))
  if (is_valid_animal != TRUE) {
    return(is_valid_animal)
  }
  if (object@hide < 0 | object@hide > 1 | is.na(object@hide)) {
    return("@hide must be in [0, 1] for class prey")
  }
  TRUE
})

setMethod("show", "prey", function(object) {
  cat(is(object)[[1]], " '", object@name, "' ", ifelse(object@female, "(w)", "(m)"), "\n",
    "  weight: ", object@weight, "\n",
    "  hide:  ", object@hide, "\n",
    sep = ""
  )
})

### prey types ####
setClass("mouse",
  contains = "prey"
)
setClass("rabbit",
  contains = "prey"
)
setClass("deer",
  contains = "prey"
)

setValidity("mouse", function(object) {
  check_validity(object = object, animal = "mouse", type = "prey")
})
setValidity("rabbit", function(object) {
  check_validity(object = object, animal = "rabbit", type = "prey")
})
setValidity("deer", function(object) {
  check_validity(object = object, animal = "deer", type = "prey")
})

mouse <- function(hide = double(), name, weight, female) {
  arguments <- set_default_animal(
    animal = "mouse", type = "prey", hide = hide, name = name, weight = weight,
    female = female
  )
  do.call(new, arguments)
}
rabbit <- function(hide = double(), name, weight, female) {
  arguments <- set_default_animal(
    animal = "rabbit", type = "prey", hide = hide, name = name, weight = weight,
    female = female
  )
  do.call(new, arguments)
}
deer <- function(hide = double(), name, weight, female) {
  arguments <- set_default_animal(
    animal = "deer", type = "prey", hide = hide, name = name, weight = weight,
    female = female
  )
  do.call(new, arguments)
}


### predator ####
setClass("predator",
  contains = "animal",
  slots = c(seek = "numeric"),
  prototype = list(seek = runif(1, min = 0, max = 1))
)

setValidity("predator", function(object) {
  # check if object is valid animal first
  is_valid_animal <- validObject(new("animal",
    name = object@name, weight = object@weight, female = object@female
  ))
  if (is_valid_animal != TRUE) {
    return(is_valid_animal)
  }
  if (object@seek < 0 | object@seek > 1 | is.na(object@seek)) {
    return("@seek must be in [0, 1] for class predator")
  }
  TRUE
})

setMethod("show", "predator", function(object) {
  cat(is(object)[[1]], " '", object@name, "' ", ifelse(object@female, "(w)", "(m)"), "\n",
    "  weight: ", object@weight, "\n",
    "  seek:  ", object@seek, "\n",
    sep = ""
  )
})

### predator types ####
setClass("hawk",
  contains = "predator"
)
setClass("lynx",
  contains = "predator"
)

setValidity("hawk", function(object) {
  check_validity(object = object, animal = "hawk", type = "predator")
})
setValidity("lynx", function(object) {
  check_validity(object = object, animal = "lynx", type = "predator")
})

hawk <- function(seek = double(), name, weight, female) {
  arguments <- set_default_animal(
    animal = "hawk", type = "predator", seek = seek, name = name, weight = weight,
    female = female
  )
  do.call(new, arguments)
}
lynx <- function(seek = double(), name, weight, female) {
  arguments <- set_default_animal(
    animal = "lynx", type = "predator", seek = seek, name = name, weight = weight,
    female = female
  )
  do.call(new, arguments)
}


