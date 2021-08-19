# File created by roxut; edit the function definition file, not this file

# test found in chkGraphicsOpt.R:16 (file:line)
 

# check for 'base'
options(ChemoSpecGraphics = "base")
expect_equal(chkGraphicsOpt(), "base")

# check for 'ggplot2'
options(ChemoSpecGraphics = "ggplot2")
expect_equal(chkGraphicsOpt(), "ggplot2")

# check for invalid mode
options(ChemoSpecGraphics = "xyz")
expect_equal(chkGraphicsOpt(), "base")