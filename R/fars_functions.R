#' Read fars data fars_read
#'
#' This function accepts a csv file and returns a tbl_df of the data
#' An error message will be printed if the file does not exist
#' @param filename  a csv file that may or may not exist
#'
#' @return a dplyr tbl_df of the data that came from the csv file.
#'     If the file does not exist, an error message will be printed.
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @source fars_data/data/accident_year.csv.bz2
##'
#' @examples
#' \dontrun{
#' x <- fars_read('myfile.csv')
#' }

fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}
#' Make filename make_filename
#'
#' This function accepts year and returns the file name of the associated year.
#'
#' @param year the year we need to get the data file
#'
#' @return name of the data file associated with the year
#' @examples
#' \dontrun{
#' x<- make_filename('1979')
#' }

make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}
#' Read years fars_read_years
#'
#' This function accepts year and returns data of the associated year.
#'
#' @param years the year we need to get the data file
#'
#' @return Creates datasets based on year.  Returns NULL if there is an error
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr %>%
#' @source fars_data/data/accident_year.csv.bz2
#'
#' @examples
#' \dontrun{
#'     fars_read_years(c(2013, 2014))
#'     fars_read_years(c("2013", "2014"))
#'           }
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
#' Summarize Years fars_summarize_years
#'
#' This function accepts year and summarize data of the associated year.
#'
#' @param years the year we need to get the data file
#'
#' @return summary of the data  associated with the year
#'
#' @importFrom dplyr bind_rows
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom tidyr spread
#' @importFrom dplyr %>%
#'
#'
#' @examples
#'   \dontrun{
#'     fars_summarize_years(c(2013, 2014))
#'     fars_summarize_years(c("2013", "2014"))
#'   }
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}
#' State map of the year data fars_map_state
#'
#' Accepts a state number and year
#' Makes the filename using make_filename function
#' gets a data frame from fars_read()
#'
#' Error checks to make sure the state number exists
#' If so, uses maps and graphics to create plots based on latitude and longitude from the data file
#'
#' Uses make_filename(year)
#'      fars_read(filename)
#'
#' @param state.num Number of a state
#' @param year The year
#'
#' @return A plot from the data file
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @examples
#' \dontrun{
#' fars_map_state(1, '2013')
#' }
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
