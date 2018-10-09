#' Convert INS01 tibble to INS03 tibble
#'
#' @param ins01 Standard ins01 tibble
#' @import tidyverse
#' @return ins03 tibble

ins01_to_ins03 <- function(ins01){

    insname_levels <- c(
        "INSDRSEQ", "INSDRCP", "INSDRCC", "INSDRPRVH", "INSDRPRVV",
        "INSNRSEQ", "INSNRCP", "INSNRCC", "INSNRPRVH", "INSNRPRVV",
        "INSPRSEQ", "INSPRCP", "INSPRCC", "INSPRPRVH", "INSPRPRVV",
        "INSGMC", "INSAV", "INSDOT", "INSDBPH", "INSDBPV",
        "INSCTHK", "INSDIA"
    )

    ins02 <- ins01 %>%
        ins01_to_ins02()


    ins03 <- ins02 %>%

        # gathering INS values in INSNAME, INSVAL
        gather(starts_with("INS"), key = "INSNAME", val = "INSVAL") %>%

        # defining INSNAME as ordered factor
        mutate(INSNAME = factor(INSNAME, levels = insname_levels)) %>%

        # arranging
        arrange(LSET, MODEL, SN, RUN, JOB, EYE, INSNAME)
}

