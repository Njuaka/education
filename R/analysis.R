#' high school dataset
#'
#' @param data data.table for analysis
#'
#' @returns selected data with for math subject
#' @export
#'

get_math_data <- function(data){
  data_ <- data[, school_program := fcase(
    stu_grant_code == 110, "French immersion",
    stu_grant_code == 101, "French",
    stu_grant_code == 112, "English"
  )]
  math_codes <- c("3908", "3905", "0080", "3940", "3903", "3000","3001", "3004", "3005","3006", "3007", "3008",
                  "3909", "3900", "3907","3923", "3918", "3919","3939")
  math_data <- data_[subject_code %in% math_codes]

  return(math_data)
}



#' statistical summary table for math subject programs
#'
#' @param data data.table including the school program
#'
#' @returns summarise statistical table
#' @export
#'
summarise_math_outcome <- function(data){

  data[, .(
    count = .N,
    avg_marks = mean(marks, na.rm = TRUE),
    sd_marks = sd(marks, na.rm = TRUE)
  ), by = school_program]

  #results <- aov(marks ~ school_program, data)
  #summary(results)

}




#' math marks by school program
#'
#' @param data data.table including the school program
#'
#' @returns comparative analysis
#' @export
#'
plot_math_outcome <- function(data){

  result <- ggplot(data, aes(x = school_program, y = marks, fill = school_program)) +
    geom_boxplot() +
    labs(
      title = "Math Marks by School Program",
      x = "School Program",
      y = "Marks"
    )

  return(result)
}

math_data <-get_math_data(high_school_data_sample)

summary <-summarise_math_outcome(math_data)
summary

plots <-plot_math_outcome(math_data)
plots


french_immersion_data <- function(data){

  data <- math_data[marks>=90 & marks<=120,]
  return(data)
}




