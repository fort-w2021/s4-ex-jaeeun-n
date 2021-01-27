
# randomly decide which event occurs when 2 animals meet
# input: 2 animals from class "animal"
# output: string describing the event
setGeneric("meet",
  function(animal_1, animal_2) standardGeneric("meet"),
  signature = c("animal_1", "animal_2")
)

# randomly decide which event occurs when 2 preys meet conditioned on their sex
# input: 2 animals from class "prey"
# output: string describing the event
setMethod("meet", c("prey", "prey"), function(animal_1, animal_2) {
  if (identical(animal_1, animal_2)) {
    return(paste0(
      class(animal_1), " '", animal_1@name, "' gazes at her reflection in a puddle \n"
    ))
  }
  if (animal_1@female != animal_2@female) {
    event <- choose_event(
      events = c(
        "ignore each other",
        "sniff each others' butts",
        "make sweet, sweet love"
      ),
      probabilities = c(0.25, 0.25, 0.5)
    )
  }
  event <- choose_event(
    events = c("ignore each other", "sniff each others' butts"),
    probabilities = c(0.5, 0.5)
  )
  paste0(
    class(animal_1), " '", animal_1@name, "' & ",
    class(animal_2), " '", animal_2@name, "' ", event, "\n"
  )
})

# randomly decide which event occurs when 2 predator meet conditioned on their sex
# input: 2 animals from class "predator"
# output: string describing the event
setMethod("meet", c("predator", "predator"), function(animal_1, animal_2) {
  if (identical(animal_1, animal_2)) {
    return(paste0(
      class(animal_1), " '", animal_1@name, "' gazes at her reflection in a puddle"
    ))
  }
  if (animal_1@female != animal_2@female) {
    event <- choose_event(
      events = c("fight for territoranimal_2", "make sweet, sweet love"),
      probabilities = c(0.5, 0.5)
    )
  }
  event <- choose_event(
    events = c(
      "ignore each other", "sniff each others' butts",
      "fight for territoranimal_2"
    ),
    probabilities = c(0.3, 0.3, 0.3)
  )
  paste0(
    class(animal_1), " '", animal_1@name, "' & ",
    class(animal_2), " '", animal_2@name, "' ", event, "\n"
  )
})

# randomly decide which event occurs when a prey and a predator meet conditioned
# on their weights
# input: first animal from class "prey", second from "predator"
# output: string describing the event
setMethod("meet", c("prey", "predator"), function(animal_1, animal_2) {
  let_prey_and_predator_meet(prey = animal_1, predator = animal_2)
})

# randomly decide which event occurs when a prey and a predator meet conditioned
# on their weights
# input: first animal from class "predator", second from "prey"
# output: string describing the event
setMethod("meet", c("predator", "prey"), function(animal_1, animal_2) {
  let_prey_and_predator_meet(prey = animal_2, predator = animal_1)
})

# randomly choose the event that occurs when probabilities are given
# input: events : character vector of events
#        probabilities: numeric vector of probabilities
# output: string describing the event
choose_event <- function(events, probabilities) {
  which_event <- as.logical(
    rmultinom(n = 1, size = 1, prob = probabilities)
  )
  return(events[which_event])
}

# randomly decide which event occurs when a prey and a predator meet conditioned
# on their weights
# input: one animal from class "prey" and one from "predator"
# output: string describing the event
let_prey_and_predator_meet <- function(prey, predator) {
  is_killing_likely <- prey@weight > 0.05 * predator@weight &
    prey@weight < 0.7 * predator@weight

  if (is_killing_likely) {
    killing_probability <- min(1, max(0, 0.6 + predator@seek - prey@hide))
    escape_probability <- 1 - killing_probability

    event <- choose_event(
      events = c("kills and eats", "escapes from"),
      probabilities = c(killing_probability, escape_probability)
    )

    if (event == "kills and eats") {
      return(paste0(
        class(predator), " '", predator@name, "' kills and eats ",
        class(prey), " '", prey@name, "' \n"
      ))
    }
    return(paste0(
      class(prey), " '", prey@name, "' escapes from",
      class(predator), " '", predator@name, "' \n"
    ))
  }

  event <- choose_event(
    events = c("ignore each other", "sniff each others' butts"),
    probabilities = c(0.5, 0.5)
  )
  paste0(
    class(prey), " '", prey@name, "' & ",
    class(predator), " '", predator@name, "' ", event, "\n"
  )
}
