
# install required packages not yet installed
pckgs <- c('shiny','reshape2','ggplot2','plyr','arm')
todo <- pckgs[!is.element(pckgs, names(installed.packages()[,"Package"]))]
if(length(todo) > 0) install.packages(todo)  #,repos="http://cran.us.r-project.org")

# load in the packages
library(shiny)
library(ggplot2)  # ggplot2 -> plotting
library(dplyr)     # ddply -> data manipulation


drawPlot <- function(v4){
	if(sum(1000*round(v4,3))==1000){ 
		v4 <- round(1000*round(v4,3)) 
		dta <- expand.grid(x=1:10,y=1:100)
	}
	else if(sum(10000*round(v4,5))==10000){ 
		v4 <- round(10000*round(v4,5)) 
		dta <- expand.grid(x=1:100,y=1:100)
	}
	lbls <- c("TN","FP","TP","FN")
	cols <- c("green", "red", "cyan", "grey")
	lc <- setNames(cols,lbls)
	ss <- lbls[round(c(rep(1,v4[1]),rep(2,v4[2]),rep(3,v4[3]),rep(4,v4[4])))]
	dta$ss <- ss
	dta$ss <- factor(dta$ss,labels=lbls,levels=lbls)

	p <- ggplot(dta, aes(y=x, x=y, fill=ss)) + geom_tile() + scale_fill_manual(values = lc) + theme_void() + theme(legend.position="none")
	print(p)
}

drawLegend <- function(v4){
	set1 <- set2 <- FALSE
	if(v4[1]==.5) set1 <- TRUE
	if(v4[2]==.25) set2 <- TRUE
	if(sum(1000*round(v4,3))==1000){ 
		v4 <- round(1000*round(v4,3)) 
		dta <- expand.grid(x=1:10,y=1:100)
	}
	else if(sum(10000*round(v4,5))==10000){ 
		v4 <- round(10000*round(v4,5)) 
		dta <- expand.grid(x=1:100,y=1:100)
	}
	lbls <- c("TN","FP","TP","FN")
	cols <- c("green", "red", "cyan", "grey")
	lc <- setNames(cols,lbls)
	ss <- lbls[round(c(rep(1,v4[1]),rep(2,v4[2]),rep(3,v4[3]),rep(4,v4[4])))]
	dta$ss <- ss
	dta$ss <- factor(dta$ss,labels=lbls,levels=lbls)

	p <- ggplot(dta, aes(y=x, x=y, fill=ss)) + geom_tile() + scale_fill_manual(values = lc) + theme_void() + theme(legend.position="none")
	if(set1){
		p <- p + annotate("text", label = "NULL", x=10,y=5,colour = "black")
		p <- p + annotate("text", label = "Effect Present", x=63,y=5,colour = "black")
	}
	if(set2){
		p <- p + annotate("text", label = "True Positive", x=10,y=5,colour = "black")
		p <- p + annotate("text", label = "False Positive", x=36,y=5,colour = "black")
		p <- p + annotate("text", label = "True Positive", x=62,y=5,colour = "black")
		p <- p + annotate("text", label = "False Negative", x=88,y=5,colour = "black")
	}
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
		p11 <- 1-inc$sT2b
		p00 <- 1-inc$cT1a
		p10 <- inc$cT1a
		p01 <- inc$sT2b
		p1 <- inc$nPH1
		p0 <- 1-inc$nPH1
		v4 <- c(p00*p0,p10*p0,p11*p1,p01*p1)
		drawPlot(c(p00*p0,p10*p0,p11*p1,p01*p1))
	}
	plotPPVlegend <- function(){
		inc <- setPH1()
		p11 <- 1-inc$sT2b
		p00 <- 1-inc$cT1a
		p10 <- inc$cT1a
		p01 <- inc$sT2b
		p1 <- inc$nPH1
		p0 <- 1-inc$nPH1
		v4 <- c(p00*p0,p10*p0,p11*p1,p01*p1)
		p <- drawLegend(c(.25,.25,.25,.25))
	}
	getPreal <- function(){
		inc <- setPH1()
		p11 <- 1-inc$sT2b	# true positive
		p00 <- 1-inc$cT1a	# true negative
		p10 <- inc$cT1a		# false negative
		p01 <- inc$sT2b		# false positive
		p1 <- inc$nPH1
		p0 <- 1-inc$nPH1
		return(p11*p1/(p11*p1+p10*p0))
	}
	output$nPH1 <- renderUI({
		sliderInput("nPH1","probability effect exists",min=0.1,max=.9,value=0.5)
	})
	output$sT2b <- renderUI({
		sliderInput("sT2b",paste("Type II error"),min=0.05,max=.5,value=.8)
	})
	output$cT1a <- renderUI({
		selectInput("cT1a","type I error:", c(".001"=".001",".01"=".01",".05"=".05",".1"=".1",".2"=".2",selected=".05"))
	})
	output$PH1 <- renderPlot({
		if(any(is.null(input$nPH1),is.null(input$sT2b),is.null(input$cT1a))){return()}
		plotPH1()#plotOutput()
	})
	output$PPV <- renderPlot({
		if(any(is.null(input$nPH1),is.null(input$sT2b),is.null(input$cT1a))){return()}
		plotPPV()#plotOutput()
	})
	output$PH1legend <- renderPlot({
		if(any(is.null(input$nPH1),is.null(input$sT2b),is.null(input$cT1a))){return()}
		plotPH1legend()#plotOutput()
	})
	output$PPVlegend <- renderPlot({
		if(any(is.null(input$nPH1),is.null(input$sT2b),is.null(input$cT1a))){return()}
		plotPPVlegend()#plotOutput()
	})
	output$comments <- renderText({
		inc <- setPH1()
		p11 <- 1-inc$sT2b	# true positive
		p00 <- 1-inc$cT1a	# true negative
		p10 <- inc$cT1a		# false negative
		p01 <- inc$sT2b		# false positive
		p1 <- inc$nPH1
		p0 <- 1-inc$nPH1
		# if(any(is.null(inc$nPH1),is.null(inc$sT2b),is.null(inc$sT1a))){return()}
		#,"<br>","probability effect exists given a significant test statistic: ",round(getPreal(),3))
		paste("PPV: ",round(p11*p1/(p11*p1+p10*p0),3)," --- NPV: ",round(p00*p0/(p00*p0+p01*p1),3),"<br><br><br>POWER: ",1-inc$sT2b)
	})
# 2*(4^4+4^4)/(4^2+4^2)^2
})