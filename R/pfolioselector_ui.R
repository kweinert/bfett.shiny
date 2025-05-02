#' @export
pfolioselector_ui <- function(id, con) {
	ns <- shiny::NS(id)
	pfolio <- DBI::dbGetQuery(con, "select distinct portfolio from transactions;")[, "portfolio"]
	bslib::nav_item(
		shiny::selectInput(
		  inputId = ns("portfolio"),
		  label = NULL,  # Kein Label, um Platz zu sparen
		  choices = pfolio,
		  selected = if("nert" %in% pfolio) "nert" else pfolio[1],
		  width = "200px"
		)
	)
}

