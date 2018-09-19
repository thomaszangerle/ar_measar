
library(tidyverse)
library(lubridate)


#' ins01_parse_from_char function to parse
#' a ins01 tibble with character columns
#'
#' @param ins01_char The ins01 tibble with character columns
#'
#' @return The ins01 tibble with the proper format
#' @import tidyverse, lubridate

ins01_parse_from_char <- function(ins01_char){
    ins01_char %>%

        # checking variables and reordering
        dplyr::select(LSET, MODEL, SN, RUN, DATETIME, JOB, EYE, LTYPE,
                      INSPRSPH, INSPRCYL, INSPRAX, INSPRPRVM, INSPRPRVA,
                      INSDRSPH, INSDRCYL, INSDRAX, INSDRPRVM, INSDRPRVA,
                      INSNRSPH, INSNRCYL, INSNRAX, INSNRPRVM, INSNRPRVA,
                      INSCTHK, INSDIA,
                      INSGMC, INSAV, INSDOT, INSDBPH, INSDBPV, INSDBPA,
                      FLAG, COMMENT
        ) %>%

        # parsing each variables correctly
        dplyr::mutate(
            LSET = as.character(LSET),
            MODEL = as.character(MODEL),
            SN = stringr::str_pad(SN, width = 3, pad = "0"),
            RUN = as.character(RUN),
            DATETIME = lubridate::ymd_hms(DATETIME),
            JOB = as.character(JOB),
            EYE = ifelse(str_detect(EYE, "[RL]"), EYE, ""),
            LTYPE = as.character(LTYPE),
            FLAG = as.character(FLAG),
            COMMENT = as.character(COMMENT)
        ) %>%

        dplyr::mutate_at(vars(matches("^INS.*")), parse_double)
}
