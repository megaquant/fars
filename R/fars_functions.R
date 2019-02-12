#' Read in a FARS file
#'
#' This function reads in a file that has been downloaded from the US National Highway
#' Traffic Safety Administration's Fatality Analysis Reporting System (FARS).
#'
#' @references \url{https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars}
#'
#' @note If the file does not exist, or if filename incorrectly specified, this function will return an error.
#' @note This function imports \link{read_csv} and \link{tbl_df}
#'
#' @param filename The name of the file as a string
#'
#' @return Returns a tibble (also a data.frame)
#'
#' @examples
#' fars_read("accident_2013.csv.bz2")
#' fars_read(make_filename(2015))
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

#' Make the FARS filename
#'
#' This function constructs the FARS filename for the datafile of a selected year. The returned string
#' can be used as an input to \link{fars_read}.
#'
#' @note If the FARS system changes their file naming convention this function will need to be updated
#' @note Be sure that the datafile resides in the current working directory (use \code{setwd()})
#'
#' @param year The year as an integer
#'
#' @return Returns the FARS filename as a string
#'
#' @examples
#' make_filename(2017)
#'
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' Aggregate FARS data across years
#'
#' This function reads in one of more years of FARS data and aggregates the data into a list. Each
#' list element is a tibble for a specific year. Only the month and year columns are retained.
#'
#' @note If there is no FARS file for a specific year this function will return a warning and a NULL list element
#' @note Imports functions from the extenal packages dplyr and magrittr
#'
#' @param years An integer, vector, or list of year(s)
#'
#' @return A list containing tibbles
#'
#' @examples fars_read_years(2013:2015)
#'
#' @importFrom dplyr select mutate
#' @importFrom magrittr %>%
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

#' Fatality counts
#'
#' This function will count the nunber of motor vehicle fatal injuries for each month of each year
#'
#' @details The yearly data is first aggregated into one list by using \link{fars_read_years}. Next, this
#' function row binds the list elements together into a tibble. Finally, dply and tidyr functions are used
#' to summarize and spread the data in order to generate the final output.
#'
#' @note Imports functions from the extenal packages dplyr, tidyr, and magrittr
#'
#' @inheritParams fars_read_years
#'
#' @return Returns a tibble of counts by month for each of the seleted years
#'
#' @examples fars_summarize_years(2013:2015)
#'
#' @importFrom dplyr bind_rows group_by summarize
#' @importFrom tidyr spread
#' @importFrom magrittr %>%
#'
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' Graphical map of state fatalities
#'
#' This function produces a graphical map of the state and shows the location of motor vehicle
#' fatalities in the year. The map is then auto plotted to the plot window.
#'
#' @details The data for the year is first read in and a check is made to determine if state data
#' exists. If this data is available it is filtered and, if accidents have occurred, their locations
#' are plotted on the state map. If NA's exist in the LONGITUD or LATITUDE columns they
#' are imputed.
#'
#' @note Year cannot be vectorized or this function will produce an error
#' @note If the state number is invalid or doesn't exist in the data this function will produce an error
#' @note If there are no accidents in the state, a message will be produced and no plot will be rendered
#' @note Imports functions from the extenal packages dplyr and maps
#'
#' @param state.num The state number, either an integer or a character to be coerced to integer
#' @inheritParams make_filename
#'
#' @return A plot rendered and shown
#'
#' @examples fars_map_state(1,2015)
#'
#' @importFrom dplyr filter
#' @importFrom maps map
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
