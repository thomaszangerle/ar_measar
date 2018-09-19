# test_ins01_to_ins01lin

test_that("Test conversion from ins01 to ins01lin",{
    table <- "ins_201809_ess_rr"
    ins01 <- ins01_read_from_aws_mysql(table)
    ins01lin <- ins01_to_ins01lin(ins01)
    expect_true(ins01lin %>% nrow() > 1)
    expect_true(ins01lin %>% pull(INSPRSEQ) %>% length > 1)
})
