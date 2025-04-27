#' @export
test_menu <- function() {
	ui <- bslib::page_navbar(
	  bslib::nav_spacer(),
	  bslib::nav_item(
		shiny::selectInput(
		  inputId = "portfolio_select",
		  label = NULL,  # Kein Label, um Platz zu sparen
		  choices = c("nert", "elvis"),
		  selected = "nert",
		  width = "200px"  # Breite der Dropdown-Box
		)
	  )
	)

	server <- function(input, output) {
	  shiny::observeEvent(input$portfolio_select, message(input$portfolio_select))
	}
	
	shiny::shinyApp(ui, server)
}
