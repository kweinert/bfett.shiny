#' @export
rebal_srv <- function(id, r, verbose=FALSE) 
	shiny::moduleServer(id=id, function(input, output, session) {

	output$tbl <- reactable::renderReactable({
		#shiny::req(
		reactable::reactable(
			curr_dat,
			columns = list(
			isin = colDef(name = "ISIN"),
			name = colDef(name = "Name"),
			category = colDef(name = "Kategorie"),
			calendar_week = colDef(name = "KW"),
			buy_in = colDef(
			  name = "Kaufwert",
			  format = colFormat(suffix = " \u20ac", digits = 2, separators = TRUE),
			  cell = function(value) format(value, big.mark = ".", decimal.mark = ",")
			),
			previous_close_value = colDef(
			  name = "Wert letzte Woche",
			  format = colFormat(suffix = " \u20ac", digits = 2, separators = TRUE),
			  cell = function(value) format(value, big.mark = ".", decimal.mark = ",")
			),
			close_value = colDef(
			  name = "Aktueller Wert",
			  format = colFormat(suffix = " \u20ac", digits = 2, separators = TRUE),
			  cell = function(value) format(value, big.mark = ".", decimal.mark = ",")
			),
			prct_chg_buy = colDef(
			  name = "\u00c4nderung seit Kauf [%]",
			  format = colFormat(percent = TRUE, digits = 2),
			  cell = function(value) sprintf("%.2f%%", value * 100)
			),
			prct_chg_week = colDef(
			  name = "\u00c4nderung zur Vorwoche [%]",
			  format = colFormat(percent = TRUE, digits = 2),
			  cell = function(value) sprintf("%.2f%%", value * 100)
			)
			),
			defaultColDef = colDef(
				align = "left",
				minWidth = 100
			),
			defaultPageSize = 10,
			striped = TRUE,
			highlight = TRUE,
			bordered = TRUE
		)
	})
})
