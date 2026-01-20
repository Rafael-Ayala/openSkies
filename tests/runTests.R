pkgname = "openSkies"
library(pkgname, character.only = TRUE, quietly = TRUE)
library("RUnit", quietly = TRUE)
RUnit_opts <- getOption("RUnit", list())
RUnit_opts$verbose <- 0L
RUnit_opts$silent <- TRUE
RUnit_opts$verbose_fail_msg <- TRUE
oopt <- options(RUnit = RUnit_opts)
on.exit(options(oopt))
suite <- RUnit::defineTestSuite(name = paste(pkgname, "RUnit Tests"),
                                dirs = file.path(path.package(pkgname), "tests"),
                                testFileRegexp = "test_all.R",
                                rngKind = "default",
                                rngNormalKind = "default")
