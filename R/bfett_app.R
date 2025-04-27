#' bfett app
#'
#' Prompt: Please write an R Shiny app as follows. Use the `bslib` package, specifically `bslib::page_navbar`.
#' Add a `nav_panel` titled "Overview". This should display a plotly output `overview_barchart` and a table `overview_tbl`.
#' A second `nav_panel` titled "Active Positions" should include a toggle "since last week / since purchase", a dynamic heading, 
#' a plotly output `apos_treemap`, and a table `apos_tbl`.
#' On the right side, a menu should be displayed with the entries "nert" and "elvis".
#' 
#'
#' @param con database connection
#' @return shinyApp object
#' @export
bfett_app <- function(con=NULL) {
	if(is.null(con)) con <- DBI::dbConnect(
		duckdb::duckdb(),
		dbdir="~/Dbtspace/bfett/database/bfett_dev.duckdb",
		read_only=TRUE
	)

	ui <- bslib::page_navbar(
	  title = "bfett",
	  id = "nav",
	  shiny::tags$style(
		  shiny::HTML("
			.nav-item #portfolio {
			  height: 30px; 
			  padding-top: 2px; 
			  padding-bottom: 2px;
			  vertical-align: middle;
			  margin-top: 0;
			  margin-bottom: 0;
			}
			.nav-item .form-group.shiny-input-container {
			   margin-top: 0;
			   margin-bottom: 0;
			   padding-top: 0;
			   padding-bottom: 0;
			}
		  ")
      ),
	  bslib::nav_panel(
		title = "\u00DCberblick",
		bslib::layout_columns(
		  col_widths = 12,
		  plotly::plotlyOutput("overview_barchart"),
		  shiny::tableOutput("overview_tbl")
		)
	  ),
	  bslib::nav_panel(
		title = "Aktive Positionen",
		bslib::layout_columns(
		  col_widths = c(4,8,12),
		  shiny::radioButtons(
			inputId = "apos_switch",
			label = NULL,
			choices = c("seit letzter Woche"="lastweek", "seit Kauf"="buyin"),
			inline = TRUE
		  ),
		  shiny::h3(shiny::textOutput("apos_title")),
		  plotly::plotlyOutput("apos_treemap"),
		  shiny::tableOutput("apos_tbl")
		)
	  ),
	  bslib::nav_spacer(),
	  bslib::nav_item(
		shiny::selectInput(
		  inputId = "portfolio",
		  label = NULL,  # Kein Label, um Platz zu sparen
		  choices = c("nert", "elvis"),
		  selected = "nert",
		  width = "200px"
		)
	  )
	)

	# Define server logic required to draw a histogram ----
	server <- function(input, output) {
		all_apos <- shiny::reactive(DBI::dbReadTable(con, "active_positions_weekly"))
		all_cash <- shiny::reactive(DBI::dbReadTable(con, "cash_weekly"))
		
		apos <- shiny::reactive({
			shiny::req(input$portfolio)
			shiny::req(all_apos())
			subset(all_apos(), portfolio==input$portfolio)
		})
		
		cash <- shiny::reactive({
			shiny::req(input$portfolio)
			shiny::req(all_cash())
			subset(all_cash(), portfolio==input$portfolio)
		})
		
		the_kw <- shiny::reactive({
			shiny::req(apos())
			max(apos()$calendar_week)
		})
		
		curr_apos <- shiny::reactive({
			shiny::req(apos())
			subset(apos(), calendar_week==the_kw()) |>
			transform(prct_chg_week = close_value / previous_close_value - 1) |> 
			transform(prct_chg_buy = close_value / buy_in - 1) 
		})
		
		output$overview_barchart <- plotly::renderPlotly({
			shiny::req(apos())
			shiny::req(cash())
			dat <- apos()
			data.table::setDT(dat)
			start_kw <- "2025-12"
			dat_ts <- dat[calendar_week>=start_kw] |>
				data.table::dcast(calendar_week ~ category, value.var="close_value", fun.aggregate=sum)
			the_cash <- subset(cash(), calendar_week>=start_kw)
			series_cols <- colnames(dat_ts)
			series_cols <- series_cols[series_cols != "calendar_week"]
			p <- plotly::plot_ly()
			for (col in series_cols) {
			  p <- plotly::add_trace(p,
				x = dat_ts[["calendar_week"]],
				y = dat_ts[[col]],
				type = "bar",
				name = col
			  )
			}
			p <- plotly::add_trace(p,
				x = the_cash[["calendar_week"]],
				y = the_cash[["cash"]],
				type = "bar",
				name = "Cash"
			)
			plotly::layout(p,
				barmode = "stack",
				xaxis = list(title = "Woche"),
				yaxis = list(title = "Wert am Ende der Woche")
			) |>
			plotly::config(displayModeBar = FALSE)
		})
		
		output$apos_title <- shiny::renderText({
			shiny::req(input$apos_switch)
			shiny::req(curr_apos())
			curr_dat <- curr_apos()
			pval <- sum(curr_dat$close_value)
			cmp_val <- if(input$apos_switch=="lastweek")
				sum(curr_dat$previous_close_value)
			else if(input$apos_switch=="buyin")
				sum(curr_dat$buy_in)
			else stop("input$apos_switch=", input$apos_switch)
			sprintf('Aktive Postionen: %.2f \u20AC (%+.2f \u20AC bzw. %+.2f%%)', pval, pval-cmp_val, (pval/cmp_val-1)*100)
		})
		
		output$apos_treemap <- plotly::renderPlotly({
			shiny::req(curr_apos())
			curr_dat <- curr_apos()
			labels <- c(unique(curr_dat$category), curr_dat$name)
			parents <- c(rep("", length(unique(curr_dat$category))), curr_dat$category)
			values <- c(rep(0, length(unique(curr_dat$category))), curr_dat$close_value)
			cmp_cn <- if(input$apos_switch=="lastweek")
				"prct_chg_week"
			else if(input$apos_switch=="buyin")
				"prct_chg_buy"
			else stop("input$apos_switch=", input$apos_switch)
			pos_colors <- ifelse(curr_dat[[cmp_cn]] < -0.10, "#D7191C",
				ifelse(curr_dat[[cmp_cn]] < -0.01, "#FDAE61",
				ifelse(curr_dat[[cmp_cn]] > 0.10, "#1A9641",
				ifelse(curr_dat[[cmp_cn]] > 0.01, "#A6D96A", "#FFFFBF")))
			)
			colors <- c(rep("#FFFFFF", length(unique(curr_dat$category))), pos_colors) # white for categories
            text_values <- c(rep("", length(unique(curr_dat$category))), 
                 sprintf("%.2f%%", curr_dat[[cmp_cn]] * 100))  # Formatierung als Prozent
			plotly::plot_ly(
			  type = "treemap",
			  labels = labels,
			  parents = parents,
			  values = values,
			  marker = list(colors = colors),
			  text = text_values,  
			  textinfo = "label+value+text",
			  domain = list(x = c(0, 1), y = c(0, 1))
			)
		})
		
	}

	# Create Shiny app ----
	shiny::shinyApp(ui = ui, server = server)
}

