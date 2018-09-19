#' Generate range plot from INS03 dataset
#'
#' @param ins03
#' @param xmax Value to be used to filter the ranges to lower values
#'
#' @return a plot

ins03_range_plot <- function(ins03, xmax = Inf){

    ins03 %>%

        # compute the range per job
        group_by(LSET, MODEL, SN, RUN, JOB, EYE, INSNAME) %>%
        mutate(RANGE = max(INSVAL, na.rm = TRUE) - min(INSVAL, na.rm = TRUE)) %>%
        mutate(RANGE = ifelse(is.infinite(RANGE), NA, RANGE)) %>%

        # removing values above ymax
        filter(RANGE <= xmax) %>%

        # filtering on main variables for the plot
        filter(
            INSNAME %in% c(
                "INSDRSEQ", "INSDRCP", "INSDRCC",
                "INSPRPRVH", "INSPRPRVV",
                "INSNRSEQ",
                "INSGMC", "INSAV", "INSDOT"
            )
        ) %>%

        # plotting
        ggplot(aes(RANGE)) +
        geom_histogram(bins = 50) +
        facet_wrap(~ INSNAME) +

        # theme
        theme_light()
}
