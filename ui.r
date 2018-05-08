
shinyUI(fluidPage(

	titlePanel("Classification and proportion of inferences"),
		fluidRow(
			column(4,
				wellPanel(
					uiOutput("nPH1"),
					uiOutput("cT1a"),
					uiOutput("sT2b")
				)
			),
			column(4,
					plotOutput("PH1")
			),
			column(4,
					plotOutput("PPV")
			)
		),
		fluidRow(
			column(6,
				uiOutput("comments")
			),
			column(6,
				wellPanel(				)
			)
		)
	)
)