#' @export
overview_ui <- function(id) {
	ns <- shiny::NS(id)
	
	bslib::nav_panel(
		title = "\u00DCberblick",
		bslib::layout_columns(
		  col_widths = c(9,3,12),
		  bslib::card(
			bslib::card_header("Zeitreihe"),
			plotly::plotlyOutput(ns("barchart")),
			full_screen = TRUE
		  ),
		  shiny::div(
			bslib::value_box(title="Wert", value=shiny::textOutput(ns("value")), showcase = shiny::icon("chart-line")),
			bslib::value_box(title="Zinssatz", value=shiny::textOutput(ns("rate")), showcase = shiny::icon("percent")),
			bslib::value_box(title="Sharpe-Ratio", value="N/A", showcase = shiny::icon("balance-scale"))
		  )
		)
	)
}
