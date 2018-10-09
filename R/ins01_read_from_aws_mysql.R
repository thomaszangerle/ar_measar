#' ins01_read_from_aws_mysql
#'
#' Get the ins01 dataset from a mysql located on A&R MySql cloud server
#' @param table Table name.
#' @return A tibble with the ins01 dataset.
#' @import tidyverse
#' @export


ins01_read_from_aws_mysql <- function(table){

    # getting the connection to the cloud server
    conn <- RMySQL::dbConnect(
        RMySQL::MySQL(),
        host = 'tza.cnrpivfveujc.eu-west-3.rds.amazonaws.com',
        username = 'thomaszangerle',
        password = 'AutoRobo322323',
        dbname = 'tzadb'
    )

    # reading table
    ins01 <- RMySQL::dbReadTable(conn, "ins01_201809_ess_rr") %>%

        as.tibble() %>%

        # parsing INS to double
        mutate_at(vars(matches("INS")), parse_double) %>%

        # parsing datetime
        mutate(DATETIME = lubridate::ymd_hms(DATETIME))

    ins01

}

