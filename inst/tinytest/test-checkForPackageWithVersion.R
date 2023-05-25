# File created by roxut; edit the function definition file, not this file

# Test found in checkForPackageWithVersion.R:17 (file:line)
  

# helper function
mmVers <- function(string) {
  mm <- sub("-.+", "", string) # remove anything beyond a "-"
  mm <- sub("([0-9]+\\.[0-9]+)\\..*", "\\1", mm)
  as.numeric(mm)
}

expect_equal(mmVers(mmVers("0.3.55")), 0.3)
expect_equal(mmVers(mmVers("0.99-20180627")), 0.99)

# get the installed version of pkg guaranteed to be available
ivers <- mmVers(getNamespaceVersion("utils"))
expect_true(checkForPackageWithVersion("utils", ivers - 0.1))
expect_true(checkForPackageWithVersion("utils", ivers))
expect_false(checkForPackageWithVersion("utils", ivers + 0.1))
