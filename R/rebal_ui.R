#' @export
rebal_ui <- function(id, con) {
	ns <- shiny::NS(id)

	bslib::nav_panel(
		title = "Rebalancing",
		reactable::reactableOutput(ns("tbl"))
	)
}
