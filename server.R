library(shiny)
library(ape)
library(ggtree)
library(shinyjs)



tablegenerator <- function(newick_tree){
  phy_tree <- read.tree(text = newick_tree)
  k <- data.frame(phy_tree$edge)
  k$length <- phy_tree$edge.length
  ntip <- length(phy_tree$tip.label)
  nodes = ntip+(ntip-1)
  for(i in ntip+1:nodes){
    k$index[k$X1==i] <-  i-(ntip+1)
  }
  k
  return(list("data_frame"=k, "nodes"=nodes,"ntip" = ntip))
}


event = function(plus,min,spec,length,start,avg,variance){
  k = 1
  plu_counter <- 0
  min_counter <- 0
  spec_counter <- 0
  total_rate = plus + min + spec
  while(k !=0){
    k <- rexp(1,total_rate)
    if(k >= length){
      k = 0
    }
    if(k < length){
      length = length - k 
      l <- sample(c(1,2,3),1,prob=c(plus/total_rate,min/total_rate,spec/total_rate))
      if(l == 1){
        plu_counter <- plu_counter+1
        start  = start + 1
      }
      if(l == 2){
        min_counter <- min_counter+1
        start = start - 1
      }
      if(l == 3){
        spec_counter <- spec_counter+1
        start = rnorm(1,mean=avg,sd=sqrt(variance))
      }
    }
  }
  m <- c(plu_counter,min_counter,spec_counter)
  return(list("counts" = m,"length"=start))
}



mutation_generator = function(newick_tree,plus,min,spec,m_start,avg,variance,node_name){
  k = tablegenerator(newick_tree)
  ntip = k$"ntip"
  nodes = k$"nodes"
  k= k$"data_frame"
  for(i in (ntip+1):nodes){
    for(j in 1:length(k$index)){
      if(k$X1[j]==i){
        if(i == (ntip+1)){
          even <- event(plus,min,spec,k$length[j],m_start,avg,variance)
          k$num_repeat[j] <- even$"length"
          k$plus[j] <- even$"counts"[1]
          k$minus[j] <- even$"counts"[2]
          k$special[j] <- even$"counts"[3]
        }
        else{
          previous_length <-  k$num_repeat[k$X2 == i]
          even <- event(plus,min,spec,k$length[j],previous_length,avg,variance)
          k$num_repeat[j] <-  even$"length"
          k$plus[j] <- even$"counts"[1]
          k$minus[j] <- even$"counts"[2]
          k$special[j] <- even$"counts"[3]
        }
      }
    }
  }
  repeat_data <- data.frame(
    Tip = character(ntip),
    Num_Repeats = numeric(ntip),
    stringsAsFactors = FALSE  )
  repeat_info <- character(ntip)
  for(i in 1:ntip){
    repeat_info[i] <- paste0("The number of repeats for ", node_name[i], " ", k$num_repeat[k$X2==i])
    repeat_data$Tip[i] = node_name[i]
    repeat_data$Num_Repeats[i] = k$num_repeat[k$X2==i]
  }
  return(list("Data Frame"=k,"Tip Info" = repeat_info,"Repeat Data"= repeat_data))
  
}

plot_fancy_tree <- function(phy_tree,repeat_data) {
  
  p <- ggtree(phy_tree, layout = "rectangular") +
    geom_tiplab(align = TRUE, size = 10, color = "white", linetype = "solid") +
    geom_tree(aes(color = branch.length), size = 2, color = "aquamarine3", linetype = "solid") +
    geom_point(aes(color = branch.length), size = 10, shape = 1) +
    theme_tree("lightgrey") +
    geom_tippoint(color = "#FDAC4F", shape = 8, size = 3) +
    geom_nodepoint(color = "#b5e521", alpha = 1/4, size = 10) +
    theme(legend.position = "none")
  
  print(p)
}




server <- function(input, output) {
  observeEvent(input$submit, {
    newick_tree <- input$newick_tree
    plus <- input$plus
    minus <- input$minus
    special <- input$special
    m_start <- input$m_start
    avg <- input$avg
    variance <- input$variance
    phy_tree <- read.tree(text = newick_tree)
    node_name <- phy_tree$tip.label
    result <- mutation_generator(newick_tree, plus, minus, special, m_start, avg, variance,node_name)
    repeat_data <- result$`Repeat Data`
    mutation_log <- result$`Data Frame`
    output$phy_tree_plot <- renderPlot({
      plot_fancy_tree(phy_tree,repeat_data)
    })
    output$mutation_table <- renderTable(mutation_log)
    
    repeat_info <- result$`Tip Info`
    
    output$repeat_info <- renderText({
      paste(repeat_info, collapse = "\n")
    })
    shinyjs::runjs(sprintf(
      "document.body.style.backgroundImage = 'linear-gradient(rgba(255, 255, 255, 0.5), rgba(255, 255, 255, 0.5)), url(%s)';",
      'https://knowhy.bookofmormoncentral.org/sites/default/files/knowhy-img/2016/3/main/hand.jpg'
    ));
  })
  observeEvent(input$close, {
    stopApp()
  })
}
