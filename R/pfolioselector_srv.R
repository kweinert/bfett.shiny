#' @export
pfolioselector_srv <- function(id, r, verbose=FALSE) 
	shiny::moduleServer(id=id, function(input, output, session) {
  
	shiny::observeEvent(input$portfolio, {
		r$portfolio <- input$portfolio
	})
  
})
