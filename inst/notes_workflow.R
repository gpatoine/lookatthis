library(devtools)
use_description()
use_namespace()
use_git()
use_package("dplyr")
use_package("leaflet")
use_package("lavaan")
use_gpl3_license(name = "Guillaume Patoine")

soil_data <- readRDS("data-raw/package_data.rds")
use_data(soil_data)

use_r("point_map")
usethis::use_vignette(name = "overview")

use_r("plot_lavaan")

getAnywhere("slug")

use_data_raw()

use_data()

# add magrittr pipe
use_pipe()


# Exer 4. build, check ------------------------------------------------------------

document()
check()
spell_check()
build()
tidy_dir("/R")
source("https://install-github.me/MangoTheCat/goodpractice")
remotes::install_github("MangoTheCat/goodpractice", force = TRUE)
library(goodpractice)
gp()

checkmate::assertString()

use_testthat()
use_test("plot_psem")


