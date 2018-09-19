#' ins01_to_ins02
#'
#' Transform the power and prism values to linear components.
#' SPH/CYL/AX are transformed to the Jackson Cross Cylinder components
#' SEQ/CP/CC.
#' PRVM/A is converted to horizontal and vertical components PRVH/V.
#' @param ins01 A tibble with a standard ins01 dataset.
#' @return A tibble with the linear ins02 dataset
#' @import tidyverse

# Transforming standard measurement dataset to linear values
ins01_to_ins02 <- function(ins01) {

    ins01 %>%
        mutate(
            SEQ = round(INSDRSPH + 0.5*INSDRCYL),
            INSPRAX = ifelse(INSPRCYL > 0, INSPRAX, INSPRAX+90),
            INSPRCYL = abs(INSPRCYL),
            INSPRSEQ = INSPRSPH + 0.5*INSPRCYL,
            INSPRCP = 0.5*INSPRCYL * cos(2*INSPRAX*pi/180),
            INSPRCC = 0.5*INSPRCYL * sin(2*INSPRAX*pi/180),
            INSPRPRVH = INSPRPRVM * cos(INSPRPRVA*pi/180),
            INSPRPRVV = INSPRPRVM * sin(INSPRPRVA*pi/180),
            INSDRAX = ifelse(INSDRCYL > 0, INSDRAX, INSDRAX+90),
            INSDRCYL = abs(INSDRCYL),
            INSDRSEQ = INSDRSPH + 0.5*INSDRCYL,
            INSDRCP = 0.5*INSDRCYL * cos(2*INSDRAX*pi/180),
            INSDRCC = 0.5*INSDRCYL * sin(2*INSDRAX*pi/180),
            INSDRPRVH = INSDRPRVM * cos(INSDRPRVA*pi/180),
            INSDRPRVV = INSDRPRVM * sin(INSDRPRVA*pi/180),
            INSNRAX = ifelse(INSNRCYL > 0, INSNRAX, INSNRAX+90),
            INSNRCYL = abs(INSNRCYL),
            INSNRSEQ = INSNRSPH + 0.5*INSNRCYL,
            INSNRCP = 0.5*INSNRCYL * cos(2*INSNRAX*pi/180),
            INSNRCC = 0.5*INSNRCYL * sin(2*INSNRAX*pi/180),
            INSNRPRVH = INSNRPRVM * cos(INSNRPRVA*pi/180),
            INSNRPRVV = INSNRPRVM * sin(INSNRPRVA*pi/180)
        ) %>%

        select(-(INSPRSPH:INSNRPRVA)) %>%

        select(LSET:LTYPE, SEQ, INSPRSEQ:INSNRPRVV, INSCTHK:COMMENT)

}
