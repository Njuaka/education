#' high school academia data
#'
#' @param path directory of the files
#'
#' @returns data tables
#' @export
#'
#' @examples data_dt <- get_data("~/data.csv")
get_data <- function(path){
  if (path=="" || missing(path)) {
    warning("path must be provided")
  }

  data <- fread(path)
  return(data)

}

grant_data <- get_data("C:/Users/Dell/Documents/data/grant_data.csv")
senior_marks_data <- get_data("C:/Users/Dell/Documents/data/senior_marks.csv")


# exploratory analysis
glimpse(senior_marks_data)
glimpse(grant_data)

# data cleaning
# - check for missingness
sum(is.na(grant_data))
sum(is.na(senior_marks_data))

#- check duplicates across all columns and if present delete
#nrow(senior_marks_data[duplicated(senior_marks_data)])
#nrow(grant_data[duplicated(grant_data)])

# unique data
senior_marks_data_unique <- unique(senior_marks_data)


# join data with goal of keeping student info and enrich with program category

set.seed(2025)
high_school_data <- senior_marks_data_unique[grant_data, on = .(academic_yr, school_nbr_hashed, student_nbr_hashed), nomatch = 0]
#glimpse(high_school_data)

# convert numeric into strings and non numeric into NA(using suppress warning)
high_school_data[, marks:=suppressWarnings(as.integer(mark))]

high_school_data <- high_school_data[, -c("mark")]

# sample data for users
high_school_data_aca <- high_school_data[sample(.N, 3000)]
