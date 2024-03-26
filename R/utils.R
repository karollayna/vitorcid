#' make a backup of the environmental variables
#'
#' @param v character vector with environemntal variables to be backed-up
#'
#' @keywords internal
b_env_v <- function(v = NULL) {
  ev <- Sys.getenv()
  ev_n <- names(ev)
  
  out_l <- list(set = NULL, unset = NULL)
  
  if (is.null(v)) {
    out_l[["set"]] <- ev
  } else {
    s_v <- intersect(ev_n, v)
    u_v <- setdiff(v, ev_n)
    
    if (length(s_v)) {
      out_l[["set"]] <- ev[s_v]
    }
    if (length(u_v)) {
      out_l[["unset"]] <- u_v
    }
  }
  out_l
}

#' restore the environmental variables
#'
#' @param rl list with two elements: 'set' and 'unset'
#' Environmental variables to be set 
#' are provided via 'set' element which is a 'Dlist'
#' Environmental variables to be unset 
#' are provided via 'unset' element which is a acharacter vector
#'
#' @keywords internal
r_env_v <- function(rl) {
  checkmate::assert_list(rl)
  checkmate::assert_class(rl[["set"]], "Dlist", null.ok = TRUE)
  checkmate::assert_character(rl[["unset"]], null.ok = TRUE)
  
  if (!is.null(rl[["unset"]])) {
    Sys.unsetenv(rl[["unset"]])
  }
  
  if (!is.null(rl[["set"]]))  {
    do.call(Sys.setenv, as.list(rl[["set"]]))
  }
}
