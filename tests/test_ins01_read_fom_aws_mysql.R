# test_ins01_read_fom_aws_mysql

test_that("Test read from aws 1",{
    table <- "ins_201809_ess_rr"
    ins01 <- ins01_read_from_aws_mysql(table)
    expect_true(ins01 %>% nrow() > 1)
})
