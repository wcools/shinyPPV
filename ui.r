
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
					plotOutput("PH1",height=300),
					plotOutput("PH1legend",height=20)
			),
			column(4,
					plotOutput("PPV",height=300),
					plotOutput("PPVlegend",height=20)
			)
		),
		fluidRow(
			column(6,
				uiOutput("comments")
			),
			column(6,
				h2("")
			)
		)
	)
)