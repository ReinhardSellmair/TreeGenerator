library(shiny)

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
   
   # Application title
   titlePanel("Tree Generator"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
          numericInput("nLevel", "Level:", value = 14, min = 1, max = 20, step = 1), 
          sliderInput("splitProb", "Probability of splitting branches:", value = 0.9, min = 0, max = 1, step = 0.05), 
          sliderInput("relLength", "Relative length of sub branch:", value = c(0.8, 1), min = 0, max = 1, step = 0.05),
          sliderInput("relAngle", "Relative angel of sub branch [deg]:", value = c(-40, 40), min = -90, max = 90, step = 5),
          submitButton(icon("refresh"))
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         h5("This App generates trees. Random numbers are used to iteratively generate branches. Each branch is connected with the end of a branch generated at the previous iteration. The level defines how many iterations shall be done. The split probability describes the probability of splitting one branch into two at the next iteration. The relative sub branch length and angle parameter describe the length and angle of the next branch with respect to the branch of the previous iteration. Press the refresh button whenever you changed the parameters to generate a new tree."), 
         plotOutput("tree")
      )
   )
))

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
    
   output$tree <- renderPlot({
       
       ## Calculation
       
       # convert angle range to rad
       angleRange <- input$relAngle / 360 * 2* pi
       
       # first branch
       branch <- data.frame(xStart = 0, yStart = 0, xEnd = 0, yEnd = 1, angle = 0, length = 1, level = 1)
       
       # generate next branches
       if (input$nLevel > 1){
           for (i in 2:input$nLevel){
               # add new branches to end of previous level branches
               
               # get branches of previous level
               parentBranch <- branch[branch$level == i - 1, ]
               # number of parent branches
               nParent <- dim(parentBranch)[1]
               
               # generate number of new branches to each branch
               nChildBranch <- sample(1:2, nParent, replace = TRUE, prob = c(1 - input$splitProb, input$splitProb))
               
               # get number of new branches 
               nBranchNew <- sum(nChildBranch)
               
               # initialise vectors
               xStart <- rep(NA, nBranchNew)
               yStart <- rep(NA, nBranchNew)
               length <- rep(NA, nBranchNew)
               angle <- rep(NA, nBranchNew)
               xEnd <- rep(NA, nBranchNew)
               yEnd <- rep(NA, nBranchNew)
               level <- rep(i, nBranchNew)
               
               # branch counter
               iNewBranch <- 0
               
               # generate child branches for each parent branch
               for (iParent in 1:nParent){
                   for (iChild in 1:nChildBranch[iParent]){
                       iNewBranch <- iNewBranch + 1
                       
                       # get start point from parent branch
                       xStart[iNewBranch] <- parentBranch$xEnd[iParent]            
                       yStart[iNewBranch] <- parentBranch$yEnd[iParent]
                       
                       # generate length and angle of new branch
                       length[iNewBranch] <- parentBranch$length[iParent] * runif(1, input$relLength[1], input$relLength[2])
                       angle[iNewBranch] <- runif(1, parentBranch$angle[iParent] + angleRange[1], parentBranch$angle[iParent] + angleRange[2])
                   }
               }
               # calculate end points of new branches
               xEnd <- xStart + length * sin(angle)
               yEnd <- yStart + length * cos(angle)
               
               # create data fram with new branches
               newBranch <- data.frame(xStart, yStart, xEnd, yEnd, angle, length, level)
               # add new branches
               branch <- rbind(branch, newBranch)
           }
       }
       
       ## plot
       
       # define axis limits
       xlim <- max(abs(branch$xEnd))
       ylim <- max(branch$yEnd)
       
       plot(NA, xlim = c(-xlim, xlim), ylim = c(0, ylim), asp = 1, axes = FALSE, xlab = "", ylab = "")
       segments(branch$xStart, branch$yStart, branch$xEnd, branch$yEnd, lwd = 7/sqrt(2*branch$level))
   })
})

# Run the application 
shinyApp(ui = ui, server = server)

