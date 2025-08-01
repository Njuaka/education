#' high school academia data
#'
#' @param path character : directory of the files
#'
#' @returns data.tables containing the contents of csv files
#' @export
#'
#' @examples
#' data_dt <- get_data("~/data.csv")
get_data <- function(path){
  if (path=="" || missing(path)) {
    warning("path must be provided")
  }

  data <- fread(path)
  return(data)

}



#' clean and load high school education data
#'
#' @param grant_data_path Character. Path to the grant data CSV.
#' @param Character. Path to the senior marks data CSV.
#' @param sample_n Integer. Number of rows to sample for preview (optional).
#'
#' @returns A sampled subset of the data
#'
#' @export
#'
#' @examples
#' #' result <- load_high_school_data("grant.csv", "marks.csv", 3000)
load_high_school_data <- function(grant_data_path, senior_marks_data_path, sample_n=3000){

  #grant_data_path <- "C:/Users/Dell/Documents/data/grant_data.csv"
  #senior_marks_data_path <- "C:/Users/Dell/Documents/data/senior_marks.csv"

  grant_data <- get_data(grant_data_path)
  senior_marks_data <- get_data(senior_marks_data_path)

  # unique data
  senior_marks_data_unique <- unique(senior_marks_data)
  #senior_marks_data_unique <- as.data.table(senior_marks_data_unique)

  # join data with goal of keeping student info and enrich with program category
  high_school_data <- senior_marks_data_unique[grant_data, on = .(academic_yr, school_nbr_hashed, student_nbr_hashed), nomatch = 0]

  # convert numeric into strings and non numeric into NA(using suppress warning)
  if ("mark" %in% names(high_school_data)) {
    high_school_data[, marks := suppressWarnings(as.integer(mark))]
    high_school_data[, mark := NULL]
  }

  # sample data for users
  set.seed(2025)
  high_school_data_sample <- high_school_data[sample(.N, min(sample_n, .N))]

  return(high_school_data_sample)
}



# grant_data_path <- "C:/Users/Dell/Documents/data/grant_data.csv"
# senior_marks_data_path <- "C:/Users/Dell/Documents/data/senior_marks.csv"
#
# grant_data <- get_data(grant_data_path)
# senior_marks_data <- get_data(senior_marks_data_path)
#
# senior_marks_data_unique <- unique(senior_marks_data)
# str(senior_marks_data_unique)
# class(senior_marks_data_unique)
# str(grant_data)
# glimpse(grant_data)


# data cleaning
# - check for missingness
#sum(is.na(grant_data))
#sum(is.na(senior_marks_data))

# check duplicates across all columns and if present delete
# (senior_marks_data[duplicated(senior_marks_data)])
# (grant_data[duplicated(grant_data)])


#glimpse(high_school_data)

# convert numeric into strings and non numeric into NA(using suppress warning)
#high_school_data[, marks:=suppressWarnings(as.integer(mark))]
#high_school_data <- high_school_data[, mark := NULL]


# sample data for users
#high_school_data_aca <- high_school_data[sample(.N, 3000)]
#high_school_data_aca <- high_school_data[sample(.N, min(3000, .N))]
