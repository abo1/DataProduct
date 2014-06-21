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


shinyUI(fluidPage(
  titlePanel("Survival Rates of Titanic Passengers by Class"),

  sidebarLayout(position = "right",
    sidebarPanel(
	radioButtons("Type", label=h4("Category"), list('Gender'=1, 'Age'=2, 'Both'=3))
  ), 

    mainPanel(
	tabsetPanel(type="tabs",
	  tabPanel("Instructions", h4("Instructions for Shiny App About Titanic Survival"), 
	    align = "left", verbatimTextOutput("Instructions")),
	  tabPanel("Plot", h4("Graph of Survival Rates by Specified Category"), 
	    align = "center", plotOutput("titanicPlot")),
	  tabPanel("Table", h4("Survival Rates by Specified Category"), 
	    align = "center", tableOutput("titanicTable")),
	  tabPanel("Chi-Square Tests", h4("Chi-Square Tests of Survival"),
  	    align = "center", tableOutput("Chisquare"), verbatimTextOutput("summary"))
  )))
))
