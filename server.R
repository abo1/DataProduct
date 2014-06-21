library(datasets)
library(shiny)
library(shinyapps)

tmp=rbind(Titanic[,,1,1], Titanic[,,2,1],Titanic[,,1,2],Titanic[,,2,2])
tmp2=c(tmp[,1], tmp[,2])
tmp3=cbind(c(rep("Male", 16), rep("Female", 16)), c(rep("Child", 4), rep("Adult", 4), 
  rep("Child", 4), rep("Adult", 4)), c(row.names(tmp), row.names(tmp)), tmp2)
colnames(tmp3)=c("Sex", "Age", "Class", "Num.Survived")
row.names(tmp3)=NULL
Titanic2=as.data.frame(tmp3)
Titanic2[,4]=as.numeric(Titanic2[,4])
Class.Num=rep(1:4, 8)
Titanic2=cbind(Titanic2, Class.Num)

Class=(1:4)
Children=rep(0,4)
Adults=rep(0,4)
Men=rep(0,4)
Women=rep(0,4)
Child.F=rep(0,4)
Child.M=rep(0,4)
Adult.F=rep(0,4)
Adult.M=rep(0,4)

for(i in 1:4) {
  Children[i]=sum(Titanic[i,,1,2])/sum(Titanic[i,,1,])
  Adults[i]=sum(Titanic[i,,2,2])/sum(Titanic[i,,2,])
  Men[i]=sum(Titanic[i,1,,2])/sum(Titanic[i,1,,])
  Women[i]=sum(Titanic[i,2,,2])/sum(Titanic[i,2,,])
  Child.F[i]=sum(Titanic[i,2,1,2])/sum(Titanic[i,2,1,])
  Child.M[i]=sum(Titanic[i,1,1,2])/sum(Titanic[i,1,1,])
  Adult.F[i]=sum(Titanic[i,2,2,2])/sum(Titanic[i,2,2,])
  Adult.M[i]=sum(Titanic[i,1,2,2])/sum(Titanic[i,1,2,])
}

Titanic.Rates=cbind(Class, Children, Adults, Men, Women, Child.F, Child.M, Adult.F, Adult.M)
tmp=as.table(Titanic.Rates)
colnames(tmp)=c("Class", "Children", "Adults", "Men", "Women", "Female.Children", 
  "Male.children", "Adult.Females", "Adult.Males")
row.names(tmp)=c("First.Class", "Second.Class", "Third.Class", "Crew")


Class2=(1:2)
Children2=rep(0,2)
Adults2=rep(0,2)
Men2=rep(0,2)
Women2=rep(0,2)
Child.F2=rep(0,2)
Child.M2=rep(0,2)
Adult.F2=rep(0,2)
Adult.M2=rep(0,2)

for(i in 1:2) {
  Children2[i]=sum(Titanic[,,1,i])
  Adults2[i]=sum(Titanic[,,2,i])
  Men2[i]=sum(Titanic[,1,,i])
  Women2[i]=sum(Titanic[,2,,i])
  Child.F2[i]=sum(Titanic[,2,1,i])
  Child.M2[i]=sum(Titanic[,1,1,i])
  Adult.F2[i]=sum(Titanic[,2,2,i])
  Adult.M2[i]=sum(Titanic[,1,2,i])
}

Titanic.Counts=cbind(Class2, Children2, Adults2, Men2, Women2, Child.F2, Child.M2, Adult.F2, Adult.M2)
row.names(Titanic.Counts)=c("Died", "Survived")
colnames(Titanic.Counts)=c("Class", "Children", "Adults", "Men", "Women", "Female.Children", 
  "Male.children", "Adult.Females", "Adult.Males")
Titanic.Counts=as.table(Titanic.Counts)

shinyServer(function(input, output) {

   output$Instructions <- renderText({	

	"This shiny app shows the death rates of survivors in the 
different classes of passengers on the Titanic. The app has 4 tabs: 
instructions, plots, tables, and chi-square tests. Each tab has a 
sidebar that allows the user to see survival rates by gender, age,
or by both gender and age at the same time. The chi-square tab is 
a little different than the plot and table tab because it does not 
take class into account; only the counts each selected category 
are in the table. 

The user can click on the tab he or she chooses and then click on 
the radio button for the desired comparison. The plot, table, or 
chi-square test should update as soon as the button is checked."
    })

   output$titanicPlot <- renderPlot({

	x <- Titanic.Rates[,1]
	y <- Titanic.Rates[,5]
	plot(x, y, xlab="Class of Passenger", ylab="Survival Rate", ylim=c(0,1.3), type="n", xaxt="n")
	axis(1, at=c(1:4), labels=c("1st Class", "2nd Class", "3rd Class", "Crew"))

  	if(input$Type==1) {
	  lines(Titanic.Rates[,1], Titanic.Rates[,4], col="blue4")
	  points(Titanic.Rates[,1], Titanic.Rates[,4], col="blue4", pch=16)
	  lines(Titanic.Rates[,1], Titanic.Rates[,5], col="firebrick3")
	  points(Titanic.Rates[,1], Titanic.Rates[,5], col="firebrick3", pch=16)
	  legend("topright", legend=c("Female", "Male"), col=c("firebrick3", "blue4"), lty=1)
	}
	if(input$Type==2) {
	  lines(Titanic.Rates[,1], Titanic.Rates[,2], col="cornflowerblue", lty=1)
	  points(Titanic.Rates[,1], Titanic.Rates[,2], col="cornflowerblue", pch=16)
	  lines(Titanic.Rates[,1], Titanic.Rates[,3], col="springgreen4", lty=1)
	  points(Titanic.Rates[,1], Titanic.Rates[,3], col="springgreen4", pch=16)
	  legend("topright", legend=c("Child", "Adult"), col=c("cornflowerblue", "springgreen4"), lty=1)
	}
	if(input$Type==3) {
	  lines(Titanic.Rates[,1], Titanic.Rates[,6], col="firebrick3", lty=2)
	  points(Titanic.Rates[,1], Titanic.Rates[,6], col="firebrick3", pch=16)
	  lines(Titanic.Rates[,1], Titanic.Rates[,7], col="blue4", lty=2)
	  points(Titanic.Rates[,1], Titanic.Rates[,7], col="blue4", pch=16)
	  lines(Titanic.Rates[,1], Titanic.Rates[,9], col="blue4")
	  points(Titanic.Rates[,1], Titanic.Rates[,9], col="blue4", pch=16)
	  lines(Titanic.Rates[,1], Titanic.Rates[,8], col="firebrick3")
	  points(Titanic.Rates[,1], Titanic.Rates[,8], col="firebrick3", pch=16)
	  legend("topright", legend=c("Female Children", "Male Children", 
	    "Female Adults", "Male Adults"), col=c("firebrick3", "blue4", "firebrick3", "blue4"), 
	    lty=c(2,2,1,1))
	}	

    })

   output$titanicTable <- renderTable({	

	if(input$Type==1) {
	  x=c(4,5) }
	if(input$Type==2) {
	  x=c(2,3) }
	if(input$Type==3) {
	  x=c(7,6,9,8) }

	tmp[,x]
    })

    output$Chisquare <- renderTable({	

	if(input$Type==1) {
	  x=c(4,5) }
	if(input$Type==2) {
	  x=c(2,3) }
	if(input$Type==3) {
	  x=c(7,6,9,8) }

	Titanic.Counts[,x]
    })

    output$summary <- renderPrint({	

	if(input$Type==1) {
	  x=c(4,5) }
	if(input$Type==2) {
	  x=c(2,3) }
	if(input$Type==3) {
	  x=c(7,6,9,8) }

	chisq.test(Titanic.Counts[,x])
    })


})

