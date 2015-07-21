### Developing Data Products

##########                 SHINY ##############################
## Shiny##########################################################
# Platform for creating interactive R programs embedded into a webpage
## Tutorial at http://shiny.rstudio.com/tutorial/
#install.packages("shiny")
library(shiny)
?shiny

# 01-kmeans-app

palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
          "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))

library(shiny)

ui <- fluidPage(
  headerPanel('Iris k-means clustering'),
  sidebarPanel(
    selectInput('xcol', 'X Variable', names(iris)),
    selectInput('ycol', 'Y Variable', names(iris),
                selected = names(iris)[[2]]),
    numericInput('clusters', 'Cluster count', 3,
                 min = 1, max = 9)
  ),
  mainPanel(
    plotOutput('plot1')
  )
)

server <- function(input, output) {
  
  selectedData <- reactive({
    iris[, c(input$xcol, input$ycol)]
  })
  
  clusters <- reactive({
    kmeans(selectedData(), input$clusters)
  })
  
  output$plot1 <- renderPlot({
    par(mar = c(5.1, 4.1, 0, 1))
    plot(selectedData(),
         col = clusters()$cluster,
         pch = 20, cex = 3)
    points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
  })
  
}

shinyApp(ui = ui, server = server)



###### Shiny App template
ui = fluidPage()
server = function(input,output){}
shinyApp(ui=ui, server=server)


###
ui = fluidPage("Hello World")
server = function(input,output){}
shinyApp(ui=ui, server=server)
###

ui = fluidPage(
  sliderInput(inputId = "num",label = "Numbers",value=25,min = 0,max = 100),
  plotOutput(outputId="hist") 
)
server = function(input,output){
  output$hist = renderPlot({
    hist(rnorm(input$num))
         })
}
shinyApp(ui=ui, server=server)

###
library(ggplot2)
str(diamonds)
ggplot(data=diamonds,aes(x=carat,color=cut))+
  geom_density(size=1.1)

ui = fluidPage(
  radioButtons(inputId="variable",label = "Pick a variable",choices=c("cut","color","clarity"),inline = TRUE),
  plotOutput(outputId="densityPlot")
  )
server = function(input,output){
  output$densityPlot = renderPlot({
    ggplot(data=diamonds,aes(x=carat,color=cut))+
      geom_density(size=1.1)+ggtitle(paste("Density Plot of Carat for different values of",input$variable))
  })
}
shinyApp(ui=ui, server=server)

### Shiny app of density plot on diamonds data
ui = fluidPage(
  sliderInput(inputId = "min",label = "Select lower end of carat",min = 0,max=1.95,value = 0,step=0.01),
  sliderInput(inputId = "max",label = "Select upper end of carat",min = 0.25,max=2,value = 2,step=0.01),
  plotOutput(outputId = "densityPlot")
)
server = function(input,output){
  output$densityPlot = renderPlot({
    library(ggplot2)
    ggplot(data=diamonds[diamonds$carat>input$min & diamonds$carat<input$max,],aes(x=price,color=cut))+
      geom_density(size=1.1)    
  })
}
shinyApp(ui=ui, server=server)

## to run the app directly, place it in a folder with the name app.r 
## then run the runApp function as shown below
runApp(appDir = "c:/myfiles/learning/r/dataanalysis/7sharedata/shinyapp")

## Apps can be hosted on shiny servers as shown here
## 
ui = fluidPage(
  titlePanel("Cool Page"),
  sidebarLayout(
    sidebarPanel(),
    mainPanel(
      h1("Rohan is Cool"),
      h2("Nikhil is awesome"),
      h3("Rohan Rocks"),
      h4("Nikhil is a clown face",align="center"),
      p("Nikhil is a monkey",align="justify")
      ))
  )
server = function(input,output){}
shinyApp(ui, server)

h1("mypanel")
p("mypanel")


## OpenCPU: but needs web programming

summary(diamonds[diamonds$carat>input$min & diamonds$carat<input$max,]$carat)



## rCharts
#remove.packages("RCurl"); install.packages("RCurl")
#require(devtools); install_github('rCharts', 'ramnathv')
require(rCharts)
temp = as.data.frame(HairEyeColor)
str(temp)
n1 = nPlot(Freq~Hair,group="Eye", type="multiBarChart",data=subset(temp,Sex=="Male"))
n1$save("n1.html",cdn=TRUE)

cat('<iframe src = "n1.html" width=100%, height=600</iframe>')
n1$html


names(iris) = gsub(pattern="\\.",replacement = "",names(iris))
r1 = rPlot(SepalLength~SepalWidth|Species,data=iris,color="Species", type = "point")
r1
r1$save("r1.html",cdn=TRUE)
cat("<iframe src = 'r1.html' width=100%, height=600 </iframe>")


r2 = rPlot(SepalLength~SepalWidth,data=iris,color="Species", type = "point")
r2
r2$save("r2.html",cdn=TRUE)
cat("<iframe src = 'r2.html' width=100%, height=600 </iframe>")



map = Leaflet$new()
map$setView(c(51,0),zoom=13)
map$save("map.html",cdn=TRUE)
map


########### GoogleVis

#install.packages("googleVis")
library(googleVis)
?googleVis
vignette("googleVis")
demo(googleVis)      # mind blowing demos of googleVis
str(Fruits)
chart = gvisMotionChart(Fruits,idvar = "Fruit", timevar = "Year")
str(chart)
chart$type
chart$html
print(chart,tag="header")
print(chart)
names(chart$html$chart)
chart$html$chart
print(chart,tag="chart")
plot(chart)
Fruits

### gvisMerge
g = gvisGeoChart(Exports,locationvar = "Country",colorvar = "Profit",options=list(width=200,height=100))
t = gvisTable(Exports,options=list(width=200,height=270))
m = gvisMotionChart(Fruits,"Fruit","Year",options=list(width=400,height=370))
gt = gvisMerge(g,t,horizontal=FALSE)
gtm = gvisMerge(gt, m, horizontal=TRUE,tableOptions="bgcolor=\"#CCCCCC\" cellspacing=10")
plot(gtm)
g = gvisGeoChart(Exports,locationvar = "Country",colorvar = "Profit",options=list(width=400,height=400))
plot(g)




#### manipulate

library(ggplot2)
data(package="ggplot2")
str(diamonds)


f = function(x,y){
  plot(x,y)
}
str(mtcars)
f = function(min,max){
  ggplot(data=mtcars,aes(x=hp,y=mpg))+
    geom_point()+
    xlim(min,max)
}
f(0,200)
manipulate(f,min=slider(0,100,initial = 0),max=slider(200,300,initial = 300))

manipulate(ggplot(data=mtcars,aes(x=hp,y=mpg))+
             geom_point()+xlim(min,max),
           min=slider(0,100,initial = 0),max=slider(200,300,initial = 300))



manipulate(
  plot(cars, xlim=c(x.min,x.max)), 
  x.min=slider(0,15), 
  x.max=slider(15,30))

manipulate(plot(1:x), x = slider(5, 10))
manipulate(
  plot(pressure, type = type), 
  type = picker("points" = "p", "line" = "l", "step" = "s"))

f(100,200)


f = function(t){
  ggplot(data=diamonds,aes(x=t,y=price))+
    geom_point()
}
f(carat)


plottingFunction(t=carat)

manipulate(plottingFunction(z),z=c("carat","cut","color"))


manipulate(ggplot(data=diamonds,aes(x=carat,y=price))+geom_point()+geom_smooth(method=x),
           x = checkbox(initial = TRUE,"loess"))

manipulate(ggplot(data=diamonds,aes(x=carat,y=price,color=x))+geom_point(), picker(cut))


manipulate({
  #define plotting function 
  ggplot(data,aes(x=carat,y=price)) +
    geom_smooth(method="loess",span=span.val) +
    geom_point()},
  #define variable that will be changed in plot
  span.val=slider(0.1,1)
)



ggplot(data=diamonds,aes(x=carat,y=price))+
  geom_bar(stat="identity")

library(manipulate)
x = manipulate(scatterplot,manipColor=c("D",))

?manipulate

str(cars)

manipulate(ggplot(data=cars,aes(x=speed,y=dist))+
  geom_point()+
  xlim(c(x.min,x.max)), x.min=slider(0,15),x.max=slider(15,30))
p = 
manipulate(plot(cars,xlim=c(x.min,x.max)),x.min=slider(0,15),x.max=slider(15,30))
plot(y)


#################### PLOTLY
#library(devtools)
#install_github("ropensci/plotly")
library(plotly)
#set_credentials_file("vishal.lala", "zvkmhe6vk8")
str(mtcars)

x = ggplot(data=mtcars,aes(x=mpg,y=hp)) +  geom_point()+geom_smooth(formula=y~ns(x,15),method="lm",se=F,size=1.1,color="violetred3")
plotly()$ggplotly(x)
plotly()$ggplotly(x)$response$url ## url where chart is posted

# Slidify

require(devtools)
install_github("slidify", "ramnathv")
install_github("slidifyLibraries", "ramnathv")
library(slidify)





author("mydeck")



slidify("index.Rmd")
publish(user = "USER", repo = "REPO")



install.packages("slidify")
library(slidify)
