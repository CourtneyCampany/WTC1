
library(shiny)

treeC <- read.csv("data/Cflux_day_trt.csv")
  treeC$Date <- as.Date(treeC$Date)
  treeC$bolebranch <- with(treeC, boleC+branchC)
  treeC$treatment <- with(treeC, paste(CO2_treatment,Water_treatment, sep="-"))

roots <- read.csv("data/roots_wtc1.csv")
  roots$Date <- as.Date(roots$Date)


#plot bits
xAT <- seq.Date(from=as.Date("2008-4-1"), length=13, by="month")
LWD <- 2 
dayClab <- c("Carbon Flux", "Bole", "+Branch", " +Leaf and Litter", "  +Root")

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
   
   # Application title
  titlePanel(""),
   
   # Sidebar with a slider input for number of bins 
  sidebarLayout(position="right",
      sidebarPanel(
        style = "border-color: #cc2904; background-color: white; border-style:dotted;border-width:thick",
        radioButtons("whichtreatment", "Pick a treatment:",c("aCO2-dry" = "ambient-dry", "aCO2-wet" = "ambient-wet", "eCO2-dry" = "elevated-dry", "eCO2-wet" = "elevated-wet"), selected="ambient-wet"),
        width=2
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("plot1"),width=4
      )
   )
))

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {

  
  flux_ss<- reactive({
    subset(treeC, treatment %in% input$whichtreatment)
  })

  roots_ss<- reactive({
    subset(roots, treatment %in% input$whichtreatment)
  })
   
  ##make plot
  output$plot1 <- renderPlot({  
  
  par(mar=c(3,5,1,1))        
  plot(fluxC ~ Date, data = treeC,axes=FALSE, xlab="", ylab="Carbon  (g)", ylim=c(0, 25000),type='n')
    points(flux_ss()[[8]] ~ flux_ss()[[1]],lwd=LWD,type = 'l',lty=1)
    points(flux_ss()[[4]] ~ flux_ss()[[1]],lwd=LWD,type = 'l',lty=3)
    points(flux_ss()[[10]] ~ flux_ss()[[1]], lwd=LWD,type = 'l',lty=2)
    points(flux_ss()[[9]] ~ flux_ss()[[1]], lwd=LWD,type = 'l',lty=5)
    points(roots_ss()[[5]] ~ roots_ss()[[3]], pch = 21, bg = "grey", cex=2)
  box()
  axis.Date(1, at = xAT, labels = TRUE)
  axis(2, labels=TRUE)
  legend(x=13975, y=25300, dayClab, lty = c(1, 3, 2, 5, -1), pch = c(-1, -1, -1, -1, 21),lwd=LWD,  
         bty='n', pt.cex=1.5,cex=1.2, pt.bg=c(-1, -1, -1, -1,"grey"))

})
})

# Run the application 
shinyApp(ui = ui, server = server)

