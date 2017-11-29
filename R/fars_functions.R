#' fars_read() read and convert to dataset
#' 
#' This function check if the file exists and then create a data frame 
#' 
#' An error will be generated if the file does not exist.
#'
#' @param filename a charater string indicates name of the input file 
#'
#' @return this function returns a data frame which is converted from input file
#'
#' @examples
#' \dontrun{
#'    fars_read(inputfile)
#' }
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @export
fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}

#' make_filename() generates a string
#' 
#' This function use year as an input and return a string "accident_year.csv.bz2", where year is
#' the input year
#'
#' @param year the input year will be used to generate the file name, it is a string or integer
#'
#' @return a string "accident_year.csv.bz2", where year is the input year
#'
#' @examples
#' \dontrun{
#'    make_filename(2013)
#' }
#'
#' @export
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#' fars_read_years() generate a list of data frame
#' 
#' This function is to read a vector of years, and load all the files "accident_year.csv.bz2", where 
#' years is the input year, and then return a list of data frame that only include the MONTH and year
#' which is from input
#' 
#' An error will occure if the input year does not match the years in the file name
#'
#' @param years A vector of years, can be string or integer
#'
#' @return a list of data frames consists of MONTH and year columns
#'
#' @examples
#' \dontrun{
#'    fars_read_years(c(2013:2015))
#' }
#'
#' @importFrom dplyr mutate select %>%
#'
#' @export
fars_read_years <- function(years) {
        lapply(years, function(year) {
                file <- make_filename(year)
                tryCatch({
                        dat <- fars_read(file)
                        dplyr::mutate(dat, year = year) %>% 
                                dplyr::select(MONTH, year)
                }, error = function(e) {
                        warning("invalid year: ", year)
                        return(NULL)
                })
        })
}

#' fars_summarize_years() generate a data frame to summary month incidence
#' 
#' This function reads a vector of year and then first generate data frame with column MONTH and year
#' through function fars_read_years(), and then return a data frame shows the incidence for each month
#'
#' @param years A vector of years, can be string or integer
#'
#' @return a data frame to summary the incidence for each month
#'
#' @examples
#' \dontrun{
#'    fars_read_years(c(2013:2015))
#' }
#'
#'
#' @importFrom dplyr bind_rows group_by summarize %>%
#' @importFrom tidyr spread
#'
#' @export
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>% 
                dplyr::group_by(year, MONTH) %>% 
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}

#' fars_map_state() plot the incidents in a state for a year
#' 
#' This function read a number of a state and a year, and then plot a map of 
#' the accidents happened in that state in that year
#' 
#' An error will occur if the state number is wroing
#'
#' @param state.num a string or integer to indicate a state
#' @param year a string or integer to indicate a year
#'
#' @return return a map indicates total number of accidents from the input state number and input year
#'
#' @examples
#' \dontrun{
#'    fars_map_state(45, 2013)
#' }
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @export
fars_map_state <- function(state.num, year) {
        filename <- make_filename(year)
        data <- fars_read(filename)
        state.num <- as.integer(state.num)

        if(!(state.num %in% unique(data$STATE)))
                stop("invalid STATE number: ", state.num)
        data.sub <- dplyr::filter(data, STATE == state.num)
        if(nrow(data.sub) == 0L) {
                message("no accidents to plot")
                return(invisible(NULL))
        }
        is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
        is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
        with(data.sub, {
                maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
                          xlim = range(LONGITUD, na.rm = TRUE))
                graphics::points(LONGITUD, LATITUDE, pch = 46)
        })
}
