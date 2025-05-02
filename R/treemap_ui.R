#' @export
treemap_ui <- function(id, con) {
	ns <- shiny::NS(id)

	bslib::nav_panel(
		title = "Treemap",
		bslib::card(
			bslib::card_header(
				shiny::div(
					style = "display: flex; justify-content: space-between; align-items: center;",
					shiny::span(shiny::textOutput(ns("title"))),
					bslib::input_switch(ns("week_buy"), label = "seit Kauf / seit letzter Woche", value = TRUE) 
				)
			),
			plotly::plotlyOutput(ns("treemap"))
		)
	)
}
