
shinyUI(fluidPage(

	titlePanel("Classification and proportion of inferences"),
		withMathJax(),
		  tags$div(HTML("
                ")),
		fluidRow(
			column(4,
				wellPanel(
					uiOutput("nPH1"),
					uiOutput("cT1a"),
					uiOutput("sT2b")
				)
			),
			column(4,
					plotOutput("PPV",height=300),
					plotOutput("PPVlegend",height=20)
			),
			column(4, style = "background-color:#aaaaaa;",
					uiOutput("comments")
				)
		),
		fluidRow(
			column(6,
				h2("")
			),
			column(6,
				h2("")
			)
		)
	)
)