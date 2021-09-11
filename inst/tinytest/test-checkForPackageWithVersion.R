# File created by roxut; edit the function definition file, not this file

# Test found in checkForPackageWithVersion.R:16 (file:line)
  

# helper function
mmVers <- function(string) {
  mm <- sub("-.+", "", string) # remove anything beyond a "-"
  mm <- sub("([0-9]+\\.[0-9]+)\\..*", "\\1", mm)
  as.numeric(mm)
}

# get the installed version of pkg guaranteed to be available
ivers <- mmVers(getNamespaceVersion("utils"))
expect_true(checkForPackageWithVersion("utils", ivers - 0.1))
expect_error(checkForPackageWithVersion("utils", ivers + 0.1))
