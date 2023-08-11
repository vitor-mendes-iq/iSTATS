
globalVariables(c("shiny", "Cairo", "shinyWidgets","ggplot2","gtools", "iSTATS","up","rstudioapi","data.table","plotly"))

#' @title A Graphical Interface to Perform STOCSY analyses on NMR Data
#'
#' @description Statistical TOtal Correlation SpectroscopY (STOCSY) is a method developed to analyze 1D Nuclear
#'              Magnetic Resonance (NMR) data, with many applications in metabolomic science, as to help the
#'              identification of molecules in complex mixture. Although STOCSY is promising method, its use
#'              requires some programming language skills. To overcome this challenge we developed the interactive
#'              STATistical Spectroscopy (iSTATS) package, based on 'shiny', in which it is possible to perform STOCSY
#'              analyses in a full interactive way, from 1D NMR matrix construction to select specifical
#'              regions to apply STOCSY methods more accurately.
#'
#'
#' @import shiny Cairo shinyWidgets shinyBS rstudioapi data.table ggplot2 gtools
#'
#' @rawNamespace import(plotly, except = last_plot)
#'
#' @examples
#'
#' if(interactive()){iSTATS::iSTATS()}
#'
#' @export
iSTATS <- function() {
  message("Starting iSTATS ...")
  if (!"package:iSTATS" %in% search()) {
    if (!suppressMessages(require(iSTATS)))
      stop("Calling iSTATS start function but iSTATS is not installed.")
  }

   shiny::runApp(system.file("app", package = "iSTATS"), launch.browser = TRUE)

}

