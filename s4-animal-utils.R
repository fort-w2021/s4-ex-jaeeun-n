# make random name of a fix length
# input: integer length
# output: name string
make_name <- function(length = 7) {
  vowels <- c("a", "e", "i", "o", "u")
  consonants <- setdiff(letters, vowels)
  name <- character(length)
  name[1] <- sample(toupper(consonants), 1)
  name[seq(3, length, by = 2)] <-
    sample(consonants, size = ceiling(length / 2) - 1, replace = TRUE)
  name[seq(2, length, by = 2)] <-
    sample(vowels, size = floor(length / 2), replace = TRUE)
  paste(name, collapse = "")
}

# conditions for different animal definitions
animal_conditions <- list(
  mouse = list(weight = c(0.5, 1), hide = c(0.6, 1)),
  rabbit = list(weight = c(1, 5), hide = c(0.3, 0.8)),
  deer = list(weight = c(15, 30), hide = c(0.2, 0.7)),

  hawk = list(weight = c(3, 8), seek = c(0.6, 1)),
  lynx = list(weight = c(20, 60), seek = c(0.5, 0.9))
)

# set default values for class animal conditioned on the type of animal,
# (for attributes that are not set)
# input: animal: "which animal" as character
#        type: "prey" or "predator"
#        name: name of animal as character
#        weight: weight of animal as numeric
#        female: sex of animal as logical
#        hide: hide of prey as numeric
#        seek: seek of predator as numeric
#        conditions: list of conditions for different animal definitions
# output: list of attributes for the animal and the class
set_default_animal <- function(animal, type, name, weight, female,
                               hide = double(), seek = double(),
                               conditions = animal_conditions) {
  name <- ifelse(missing(name),
    make_name(),
    as.character(name)
  )
  female <- ifelse(missing(female),
    c(TRUE, FALSE)[sample(1:2, 1)],
    as.logical(female)
  )
  weight <- ifelse(missing(weight),
    set_random_attribute(animal = animal, attribute = "weight"),
    as.numeric(weight)
  )
  if (type == "prey") {
    hide <- ifelse(length(hide) == 0,
      set_random_attribute(animal = animal, attribute = "hide"),
      as.double(hide)
    )
    return(list(name = name, weight = weight, female = female, hide = hide, Class = animal))
  } else {
    seek <- ifelse(length(seek) == 0,
      set_random_attribute(animal = animal, attribute = "seek"),
      as.double(seek)
    )
    return(list(name = name, weight = weight, female = female, seek = seek, Class = animal))
  }
}

# set random value between given minimum and maximum
# input: animal: "which animal" as character
#        attribute: name of attribute to set
#        conditions: list of conditions for different animal definitions
# output: random value
set_random_attribute <- function(animal, attribute, conditions = animal_conditions) {
  min_value <- conditions[[animal]][[attribute]][1]
  max_value <- conditions[[animal]][[attribute]][2]
  runif(1, min_value, max_value)
}

# check if a object fulfills conditions of different animal classes
# (first check if object is valid object of class "prey"/"predator")
# input: object: of class "mouse", "deer", "rabbit", "hawk", or "lynx"
#        animal: class name as character
#        type: "prey" or "predator"
#        conditions: list of conditions for different animal definitions
# output: error messages or TRUE
check_validity <- function(object, animal, type, conditions = animal_conditions) {
  is_valid_type <- ifelse(type == "prey",
    validObject(new(
      "prey",
      name = object@name, weight = object@weight, female = object@female,
      hide = object@hide
    )),
    validObject(new(
      "predator",
      name = object@name, weight = object@weight, female = object@female,
      seek = object@seek
    ))
  )
  if (is_valid_type != TRUE) {
    return(is_valid_type)
  }

  validity_checks <- character()
  weight_min <- conditions[[animal]][["weight"]][1]
  weight_max <- conditions[[animal]][["weight"]][2]
  if (object@weight < weight_min | object@weight > weight_max | is.na(object@weight)) {
    weight_not_valid <- paste0(
      "@weight must be in [", weight_min, ", ", weight_max, "] for class ", animal
    )
    validity_checks <- append(validity_checks, weight_not_valid)
  }
  if (type == "prey") {
    hide_min <- conditions[[animal]][["hide"]][1]
    hide_max <- conditions[[animal]][["hide"]][2]
    if (object@hide < hide_min | object@hide > hide_max | is.na(object@hide)) {
      hide_not_valid <- paste0(
        "@hide must be in [", hide_min, ", ", hide_max, "] for class ", animal
      )
      validity_checks <- append(validity_checks, hide_not_valid)
    }
  }
  if (type == "predator") {
    seek_min <- conditions[[animal]][["seek"]][1]
    seek_max <- conditions[[animal]][["seek"]][2]
    if (object@seek < seek_min | object@seek > seek_max | is.na(object@seek)) {
      seek_not_valid <- paste0(
        "@seek must be in [", seek_min, ", ", seek_max, "] for class ", animal
      )
      validity_checks <- append(validity_checks, seek_not_valid)
    }
  }
  if (length(validity_checks) > 0) {
    return(paste(validity_checks, sep = "\n"))
  }
  TRUE
}


