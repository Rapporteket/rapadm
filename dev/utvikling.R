
devtools::install("../rapbase/.")

devtools::install(upgrade = FALSE)

source("dev/env.R")
rapadm::run_app(browser = TRUE)
