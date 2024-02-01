library(shinyjs)
ui <- fluidPage(
 shinyjs::useShinyjs(),
  
  tags$head(
    tags$style(
      HTML("
        body {
          background-size: cover;
          background-repeat: no-repeat;
          background-color: linear-gradient(rgba(255, 255, 255, 0.5), rgba(255, 255, 255, 0.5)), url('https://knowhy.bookofmormoncentral.org/sites/default/files/knowhy-img/2016/3/main/hand.jpg');
        }
      ")
    )
  ),
  titlePanel("Mutation Generator"),
  sidebarLayout(
    sidebarPanel(
      textAreaInput("newick_tree", "Enter Newick Tree:", "(((A:0.1,B:0.2):0.3,(C:0.4,D:0.5):0.6):0.7,(E:0.8,F:0.9):1.0);"),
      numericInput("plus", "Plus Rate:", 1),
      numericInput("minus", "Minus Rate:", 1),
      numericInput("special", "Special Rate:", 0.5),
      numericInput("m_start", "Mutation Start:", 10),
      numericInput("avg", "Average Value:", 10),
      numericInput("variance", "Variance:", 3),
      actionButton("submit", "Generate Mutations"),
      actionButton("close", "Close")
    ),
    mainPanel(
      plotOutput("phy_tree_plot"), 
      verbatimTextOutput("repeat_info"),
      tableOutput("mutation_table")
    )
  )
)
