#'Helper Function to Load Files Containing
#'
#'@description This function looks at the file extension to guess at what it should be loading
#'

load_data =
  function(source){
  ###Leverage a switch for multiple file formats
  ###Load in the underlying data to be made into a workout
    #find the up to the last six characters that arent a literal .
    suffix = str_extract(source, "(?<=\\.)[^\\.]{1,6}$")
  ###strive to support db, csv, txt, xls
    df = switch(suffix,
                "xlsx" = read_xlsx(source))
    class(df) = c("raw_exercise", class(df))
    return(df)
  }

#'Create a exercise list class system to leverage S3
#'
#'
#'

get_unique_values.raw_exercise =
  function(df, var){
    df[[var]] %>%
      str_split(., "/") %>%
      unlist() %>%
      unique
  }

construct_exercises =
  function(data, muscle_groups = character(), equipment_needs = character()){
    if(!"data.frame" %in% class(data)){
      stop("Exercises must be in a data frame")
    }
    muscle_groups = get_unique_values(data, "muscle_group")
    equipment_needs = get_unique_values(data, "equipment")

    x = structure(new_tibble(x = data,
                         nrow = nrow(data),
                         class = "exercises"),
                  muscles = muscle_groups,
                  equipment_needs = equipment_needs)

    return(x)

  }



#' Validate the constructor class
#'
#'

valid_exercises =
  function (data){

  if(nrow(data) == 0){
    stop("Dataframe is empty")
  }

  if(!"name" %in% names(data)){
    stop("No exercises found in the dataframe")
  }

  if(!"muscle_group" %in% names(data)){
    warning("No muscle_group column found, exercises will not be grouped by body part")
  }

  if(!"equipment" %in% names(data)){
    warning("No equipment column found, plans will not report requirements")
  }

  if(!"compound" %in% names(data)){
    warning("No compound column found, plans will not be randomized in intensity")
  }

  return(data)
}
