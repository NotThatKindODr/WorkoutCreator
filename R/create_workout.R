#' Create a workout
#'

create.workout =
  function(data, muscle_group){
  data %>%
      filter(muscle_group %in% muscle_group) %>%
      sample_n(5)
  }

create.week =
  function(){

  }

