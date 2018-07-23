
# install required packages not yet installed
pckgs <- c('shiny','reshape2','ggplot2','plyr','arm')
todo <- pckgs[!is.element(pckgs, names(installed.packages()[,"Package"]))]
if(length(todo) > 0) install.packages(todo)  #,repos="http://cran.us.r-project.org")

# load in the packages
library(shiny)
library(ggplot2)  	# ggplot2 -> plotting
library(dplyr)     	# ddply -> data manipulation


drawPlot <- function(v4){
	v4 <- round(10000*round(v4,5)) 
	dta <- expand.grid(x=1:100,y=1:101)
	lbls <- c("TP","FN","TN","FP","xx")
	cols <- c("green", "red", "cyan", "grey","black")
	lc <- setNames(cols,lbls)
	ss <- lbls[round(c(rep(1,v4[1]),rep(2,v4[2]),rep(5,100),rep(3,v4[3]),rep(4,v4[4])))]
	dta$ss <- ss
	dta$ss <- factor(dta$ss,labels=lbls,levels=lbls)

	p <- ggplot(dta, aes(y=x, x=y, fill=ss)) + geom_tile() + scale_fill_manual(values = lc) + theme_void() + theme(legend.position="none")
	print(p)
}

drawLegend <- function(v4){
	v4 <- round(1000*round(v4,3)) 
	dta <- expand.grid(x=1:10,y=1:100)
	lbls <- c("TP","FN","TN","FP")
	cols <- c("green", "red", "cyan", "grey")
	lc <- setNames(cols,lbls)
	ss <- lbls[round(c(rep(1,v4[1]),rep(2,v4[2]),rep(3,v4[3]),rep(4,v4[4])))]
	dta$ss <- ss
	dta$ss <- factor(dta$ss,labels=lbls,levels=lbls)

	p <- ggplot(dta, aes(y=x, x=y, fill=ss)) + geom_tile() + scale_fill_manual(values = lc) + theme_void() + theme(legend.position="none")
	p <- p + annotate("text", label = "True Positive", x=10,y=5,colour = "black")
	p <- p + annotate("text", label = "False Negative", x=36,y=5,colour = "black")
	p <- p + annotate("text", label = "True Negative", x=62,y=5,colour = "black")
	p <- p + annotate("text", label = "False Positive", x=88,y=5,colour = "black")
	print(p)
}

# server file
shinyServer(function(input,output,session){

	setPH1 <- reactive({
		nPH1 <- as.numeric(as.character((input$nPH1)))
		cT1a <- as.numeric(as.character((input$cT1a)))
		sT2b <- as.numeric(as.character((input$sT2b)))	
		list(nPH1=nPH1,cT1a=cT1a,sT2b=sT2b)
	})
	plotPH1 <- function(){
		inc <- setPH1()
		p1 <- inc$nPH1
		p0 <- 1-inc$nPH1
		drawPlot(c(p0,0,p1,0))
	}
	plotPH1legend <- function(){
		inc <- setPH1()
		p1 <- inc$nPH1
		p0 <- 1-inc$nPH1
		drawLegend(c(.5,0,.5,0))
	}
	plotPPV <- function(){
		inc <- setPH1()
		.green <- inc$sT2b * inc$nPH1			# true positive
		.red <- (1-inc$sT2b) * inc$nPH1			# false negative
		.grey <- inc$cT1a * (1-inc$nPH1)		# false positive
		.blue <- (1-inc$cT1a) * (1-inc$nPH1)	# true negative
		v4 <- c(.green,.red,.blue,.grey)
		drawPlot(c(.green,.red,.blue,.grey))
	}
	plotPPVlegend <- function(){
		inc <- setPH1()
		p <- drawLegend(c(.25,.25,.25,.25))
	}
	output$nPH1 <- renderUI({
		sliderInput("nPH1","probability effect exists",min=0.1,max=.9,value=0.5)
	})
	output$sT2b <- renderUI({
		sliderInput("sT2b",paste("power"),min=0.5,max=.95,value=.8)
	})
	output$cT1a <- renderUI({
		selectInput("cT1a","type I error:", c(".01"=".01",".05"=".05",".1"=".1",".2"=".2",selected=".05"))
	})
	output$PPV <- renderPlot({
		if(any(is.null(input$nPH1),is.null(input$sT2b),is.null(input$cT1a))){return()}
		plotPPV()
	})
	output$PPVlegend <- renderPlot({
		if(any(is.null(input$nPH1),is.null(input$sT2b),is.null(input$cT1a))){return()}
		plotPPVlegend()
	})
	output$comments <- renderText({
		inc <- setPH1()
		.green <- inc$sT2b * inc$nPH1
		.red <- inc$cT1a * (1-inc$nPH1)
		.blue <- (1-inc$cT1a) * (1-inc$nPH1)
		.grey <- (1-inc$sT2b) * inc$nPH1
		paste("probability effect = <font color=\"#00FF00\" size=3>&diams;</font> + <font color=\"#FF0000\">&diams;</font> = <font color=\"#00FF00\">P(true +)</font> + <font color=\"#FF0000\">P(false -)</font><br>",
		"power = <font color=\"#00FF00\" size=3>&diams;</font> / [<font color=\"#00FF00\">&diams;</font> + <font color=\"#FF0000\">&diams;</font>] = <font color=\"#00FF00\">P(true +)</font> / [<font color=\"#00FF00\">P(true +)</font> + <font color=\"#FF0000\">P(false -)</font>]<br>",
		"alpha = <font color=\"#808080\" size=3>&diams;</font> / [<font color=\"#808080\">&diams;</font> + <font color=\"#00FFFF\">&diams;</font>] = <font color=\"#808080\">P(false +)</font> / [<font color=\"#808080\">P(false +)</font> + <font color=\"#00FFFF\">P(true -)</font>]<br>",
		"<br>Positive Predictive Value <br>PPV = <font color=\"#00FF00\" size=3>&diams;</font> / [<font color=\"#00FF00\">&diams;</font> + <font color=\"#808080\">&diams;</font>] = <font color=\"#00FF00\">P(true +)</font> / [<font color=\"#00FF00\">P(true +)</font> + <font color=\"#808080\">P(false +)</font>]<br>",
		"<br>Negative Predictive Value <br>NPV = <font color=\"#00FFFF\" size=3>&diams;</font> / [<font color=\"#00FFFF\">&diams;</font> + <font color=\"#FF0000\">&diams;</font>] = <font color=\"#00FFFF\">P(true -)</font> / [<font color=\"#00FFFF\">P(true -)</font> + <font color=\"#FF0000\">P(false -)</font>]<br>")
	})
})