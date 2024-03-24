# Prevent R CMD check from complaining about some vars
# a) standard data.table variables
utils::globalVariables(c("why"),
                       utils::packageName())
