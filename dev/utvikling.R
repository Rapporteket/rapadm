
devtools::install("../rapbase/.")
devtools::install(upgrade = FALSE)

source("dev/env.R")

Sys.setenv(MYSQL_HOST="localhost") # for mobilt kontor

rapadm::run_app(browser = TRUE)
