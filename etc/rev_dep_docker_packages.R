# Copyright 2019 Rob Carnell

args <- commandArgs(trailingOnly = TRUE)
# test if there is at least one argument: if not, return an error
if (length(args) == 0) {
  stop("At least one argument must be supplied (target_package)")
} else if (length(args) == 1) {
  target_package <- args[1]
} else {
  cat("Too many arguments supplied\nUsing the first.")
  target_package <- args[1]
}

################################################################################
require(devtools)
get_rev_deps <- function(x) tools::package_dependencies(packages = x, which = "most")
exclusions_list <- c("ExtDist")

rev_dep_req_install <- function(rev_req)
{
  # install if not done already
  rev_depends_ureq <- unique(unlist(rev_req))
  ind <- which(rev_depends_ureq %in% exclusions_list)
  if (length(ind) > 0) rev_depends_ureq <- rev_depends_ureq[-ind]
  for (i in seq_along(rev_depends_ureq))
  {
    # don't need to check or install the target_pacakge
    if (rev_depends_ureq[i] == target_package)
      next
    # print separators to make scanning easier
    print("---------------------------------------------------------------------")
    print(paste(":::::::", rev_depends_ureq[i], ":::::::::::::::::::::::::::::::"))
    if (!require(rev_depends_ureq[i], character.only = TRUE))
    {
      install.packages(rev_depends_ureq[i], dependencies = TRUE)
    }
    # after installation check the result and error if not working
    if (!require(rev_depends_ureq[i], character.only = TRUE))
    {
      stop(paste("Installation of ", rev_depends_ureq[i], " failed"))
    }
  }
}

################################################################################

# get all the reverse dependencies of the target_package
pack_rev_depends <- devtools::revdep(pkg = target_package, dependencies = "Depends")
pack_rev_imports <- devtools::revdep(pkg = target_package, dependencies = "Imports")
pack_rev_suggests <- devtools::revdep(pkg = target_package, dependencies = "Suggests")

# get all the necessary packages that they depend on so tney can operate fully
#   in the reverse dependency checks
pack_rev_depends_req <- lapply(pack_rev_depends, get_rev_deps)
pack_rev_imports_req <- lapply(pack_rev_imports, get_rev_deps)
pack_rev_suggests_req <- lapply(pack_rev_suggests, get_rev_deps)

#all_unique <- unique(c(unlist(pack_rev_depends_req),
#                     unlist(pack_rev_imports_req),
#                     unlist(pack_rev_suggests_req)))
#all_unique <- all_unique[-which(all_unique == target_package)]
#cat(paste(paste("r-cran-", all_unique, sep = ""), collapse = " \\ \n"))

rev_dep_req_install(pack_rev_depends_req)
rev_dep_req_install(pack_rev_imports_req)
rev_dep_req_install(pack_rev_suggests_req)

