#' @import gDRcomponents
#' @import gDRutils
#' @import shiny
#' @import shinydashboard
#' @import magrittr
#' @importFrom data.table as.data.table data.table :=
#' @importFrom lobstr obj_size
NULL

# Prevent R CMD check from complaining about some vars
# a) standard data.table variables
utils::globalVariables(c("why"),
                       utils::packageName())
