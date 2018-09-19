library(testthat)
library(tidyverse)

source("R/ins01_read_from_aws_mysql.R")
source("R/ins01_to_ins01lin.R")

test_results <- test_dir("tests/", reporter="summary")
