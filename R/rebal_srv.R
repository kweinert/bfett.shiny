#' @export
rebal_srv <- function(id, r, verbose=FALSE) 
	shiny::moduleServer(id=id, function(input, output, session) {
	
	curr_value <- shiny::reactive({
		val_pfoliovalue(con=r$con, portfolio=r$portfolio)[["value"]] |>
		sum()
	})
	
	apos <- shiny::reactive({
		shiny::req(curr_value())
		DBI::dbReadTable(r$con, "active_positions_weekly") |>
			subset(calendar_week==max(calendar_week) & portfolio==r$portfolio, c(isin, name, category, close_value)) |>
			aggregate(close_value ~ isin+name+category, data=_, FUN=sum) |>
			transform(curr_share = close_value / curr_value())
	})
	
	ideas <- shiny::reactive({
		shiny::req(curr_value())
		shiny::req(apos())
		DBI::dbReadTable(r$con, "ideas") |>
			merge(aggregate(curr_share ~ category, data=apos(), FUN=sum), by="category", all=TRUE)
	})

	output$tbl <- reactable::renderReactable({
		shiny::req(apos())
		shiny::req(ideas())
		the_ideas <- ideas()
		reactable::reactable(
			data=the_ideas,
			pagination = FALSE,
			striped = TRUE,
			highlight = TRUE,
			columns = list(
				category = reactable::colDef(
				  name = "Idee",
				  align = "left"
				),
				target_share = reactable::colDef(
				  name = "Zielanteil",
				  format = reactable::colFormat(
					percent = TRUE,
					digits = 2,
					locales = "de-DE"
				  ),
				  na = "-",
				  align = "right"
				),
				curr_share = reactable::colDef(
				  name = "Aktueller Anteil",
				  format = reactable::colFormat(
					percent = TRUE,
					digits = 2,
					locales = "de-DE"
				  ),
				  align = "right"
				)
			),
			details = function(index) {
				subdat <- subset(
					apos(), 
					category==the_ideas[index,"category"],
					c(isin, name, curr_share)
				)
				subdat <- subdat[order(subdat$curr_share, decreasing=TRUE),]
				shiny::div(style = "padding: 1rem",
					reactable::reactable(
						subdat, 
						outlined = TRUE,
						columns = list(
							isin = reactable::colDef(
							  name = "ISIN",
							  align = "left"
							),
							name = reactable::colDef(
							  name = "Name",
							  align = "left"
							),
							curr_share = reactable::colDef(
							  name = "Aktueller Anteil",
							  format = reactable::colFormat(
								percent = TRUE,
								digits = 3,
								locales = "de-DE"
							  ),
							  align = "right"
							)
						)
					)
				)
			}
		)
	})
})
