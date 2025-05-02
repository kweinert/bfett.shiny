#' @export
rebal_ui <- function(id, con) {
	ns <- shiny::NS(id)

	bslib::nav_panel(
		title = "Rebalancing",
		bslib::card(
			reactable::reactableOutput(ns("tbl"))
		)
	)
}
