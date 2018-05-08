
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
	getPreal <- function(){
		inc <- setPH1()
		p11 <- 1-inc$sT2b
		p00 <- 1-inc$cT1a
		p10 <- inc$cT1a
		p01 <- inc$sT2b
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
		selectInput("cT1a","type I error:", c(".001"=".001",".01"=".01",".05"=".05",".1"=".1",selected=".05"))
	})
	output$PH1 <- renderPlot({
		if(any(is.null(input$nPH1),is.null(input$sT2b),is.null(input$cT1a))){return()}
		plotOutput(plotPH1())
	})
	output$PPV <- renderPlot({
		if(any(is.null(input$nPH1),is.null(input$sT2b),is.null(input$cT1a))){return()}
		plotOutput(plotPPV())
	})
	output$comments <- renderText({
		inc <- setPH1()
		# if(any(is.null(inc$nPH1),is.null(inc$sT2b),is.null(inc$sT1a))){return()}
		paste("POWER: ",1-inc$sT2b,"<br>","probability effect exists given a significant test statistic: ",round(getPreal(),3))
	})
# 2*(4^4+4^4)/(4^2+4^2)^2
})