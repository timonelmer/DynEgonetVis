library(shiny)
library(dplyr)
library(rlang)
library(tidygraph)
library(ggplot2)

developing = T
if(developing){
dataset <- read.csv("test_dat_ecrvema.csv")
id <- sample(unique(dataset$participantID),1)
id.varname <- "participantID"
alter.varname <- "initials_clean"
timepoint <- 4
#egonet_plot(dataset, id.varname, id, alter.varname, timepoint)

}

egonet_plot <- function(dataset, id.varname = "participantID", id, 
                        layout.l = NULL, output = "momentary", seed = 56,
                        alter.varname = "initials_clean", timepoint = 3) {
  
  require(igraph)
  require(ggraph)
  dat <- dataset[dataset[,id.varname] %in% id,]
  sub <- dat[1:timepoint,]
  alters <- c("Ego",unique(dat[,alter.varname]))
  
  emptynet <- matrix(0, length(alters),length(alters))
  rownames(emptynet) <- alters
  colnames(emptynet) <- alters
  
  net <- layoutnet <-emptynet
  
  layoutnet[1,-1] <- 1
  set.seed(seed)
  if(is.null(layout.l)) layout.l <- create_layout(graph =layoutnet, layout = 'fr')
  
  for(row in 1:nrow(sub)){
    tmp <- sub[row,]
    tmp.net <- emptynet
    
    
    if(nrow(tmp) > 0){
      new.tie <- rep(0, length(alters))
      if(is.na(tmp[,alter.varname])) next()
      new.tie[which(tmp[,alter.varname] == alters)] <- 1
      
      tmp.net[1,] <- as.matrix(new.tie)
      net[1,] <- net[1,]  + as.matrix(tmp.net[1,])
    }
  }
  
  #cols.int <- colorRampPalette(c("orange", "forestgreen"))
  #int.C.col <- cols.int(100)[tmp$IntIGselfC_int]
  vertex.border <- 0
  vertex.color <- ifelse(colnames(tmp.net) == "Ego","blue","forestgreen")
  vertex.color[colSums(tmp.net, na.rm = T) < 1 & !(colnames(tmp.net) == "Ego")] <- "gray90"
  
  
  as_tbl_graph(tmp.net, coord = F)
  
  
  tmp.graph <- graph.adjacency(tmp.net, mode = "undirected")
  l.graph <- layout.l[,1:2]

  egonet <-  ggraph(tmp.graph, scale(l.graph))+ 
    geom_edge_link(alpha = 0.6, width = 2)+ 
    geom_node_point(color = vertex.color,alpha = 0.6, size = 4)+
    geom_node_text(aes(label = alters, vjust = 1.5#, family = "Times"
                       ), size = 3) +
    ggtitle(paste0("Momentary Egocentric Network \nDate: ", tmp$date)) +
    theme_graph() 
  
    egonet.aggr <-  ggraph(graph.adjacency(net, mode ="undirected", weighted = T), scale(l.graph))+ 
    #geom_edge_link(alpha = 0.6, width = 2)+ 
    geom_edge_link(alpha = .15, aes(width = weight, color = weight, label = weight), family = "Times") +
    geom_node_point(color = vertex.color,alpha = 0.6, size = 4)+
    geom_node_text(aes(label = alters, vjust = 1.5#, family = "Times"
    ), size = 3) +
    ggtitle(paste0("Aggregated Egocentric Network ")) +
    theme_graph()  +
    theme(legend.position = "none")
  
  if(output == "aggregated"){return(egonet.aggr)}else{
    return(egonet)
  }
  
}

if(developing){
  egonet_plot(dataset, id = "125630", layout.l = NULL, timepoint = 1)
  egonet_plot(dataset, id = "125630", layout.l = NULL, timepoint = 2)
  egonet_plot(dataset, id = "125630", layout.l = NULL, timepoint = 3)
  egonet_plot(dataset, id = "125630", layout.l = NULL, timepoint = 4)
  egonet_plot(dataset, id = "125630", layout.l = NULL, timepoint = 4, output = "aggregated")
}


egonet_layout <- function(dataset,id.varname = "participantID", id, seed = 56, alter.varname = "initials_clean"){
  require(igraph)
  require(ggraph)
  set.seed(seed)
  dat <- dataset[dataset[,id.varname] %in% id,]
  sub <- dat[1:timepoint,]
  alters <- c("Ego",unique(dat[,alter.varname]))
  
  emptynet <- matrix(0, length(alters),length(alters))
  rownames(emptynet) <- alters
  colnames(emptynet) <- alters
  
  emptynet[1,-1] <- 1
  return(layout.fruchterman.reingold(graph.adjacency(emptynet)))
}


if(developing){
  layout.test <- egonet_layout(dataset, id = "125630")
  egonet_plot(dataset, id = "125630", layout.l = layout.test, output = "momentary", timepoint = 1)
  egonet_plot(dataset, id = "125630", layout.l = layout.test, output = "momentary", timepoint = 2)
}


importUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fileInput(ns("file1"), "Choose CSV File", accept = ".csv"),
    checkboxInput(ns("header"), "Header", TRUE),
    tableOutput(ns("contents"))
  )
  
}

importSE <- function(id) {
  moduleServer(id, 
               function(input, output, session) {
                 
                 dtreact <- reactive({
                   file <- input$file1
                   if (is.null(file))
                     return(NULL)
                   read.csv(file$datapath, header = input$header)
                 })
                 
                 
                 output$contents <- renderTable({
                   #dtreact()
                 })
                 
                 return(dtreact)
               }
  )
  
}

varselect_ui <- function(id) {
  ns <- NS(id)
  var_choices <- ""
  tagList(selectInput(ns("id.varname"), "Select ID variable", choices = var_choices, selected = NULL),
          selectInput(ns("id"), "Select ID", choices = var_choices, selected = NULL),
          #selectInput(ns("timepoint"), "Select timepoint", choices = 1:15, selected = 1))
          sliderInput(ns("timepoint"), "Select timepoint", min = 1, max  = 1:40, value = 1, round = T))
}

varselect_server <- function(id, dataset) {
  moduleServer(id,
               function(input, output, session) {
                 observeEvent(dataset(), {
                   updateSelectInput(session,
                                     "id.varname",
                                     choices = names(dataset()))
                   updateSelectInput(session,
                                     "id",
                                     choices = unique(dataset()$participantID))
                   updateSelectInput(session,
                                     "timepoint",
                                     choices = 1:100)
                   updateSliderInput(session,
                                     "timepoint", "Select timepoint",
                                     min = 1, max = 50
                                     )
                 })
                 
                 return(
                   list(
                     id = reactive({input$id}),
                     timepoint = reactive({input$timepoint})
                   )
                 )
               }
  )
}

egonet_ui <- function(id) {
  ns <- NS(id)
  #plotOutput(ns("plot1"))
  plotOutput(ns("plot2"))
}

egonet_server <- function(id, dataset, plot1vars, plot2vars) {
  moduleServer(id, 
               function(input, output, session) {
                 
                 plot1_obj <- reactive({
                   req(dataset())
                   set.seed(123)
                   layout <- egonet_layout(dataset = dataset(), id = plot1vars())
                   p <- egonet_plot(dataset(), 
                                    layout.l = layout, 
                                    id = plot1vars(), timepoint = plot2vars())
                   return(p)
                 })
                 
                 plot2_obj <- reactive({
                   req(dataset())
                   set.seed(123)
                   layout <- egonet_layout(dataset = dataset(), id = plot1vars())
                   p <- egonet_plot(dataset(), 
                                    output = "aggregated",
                                    layout.l = layout, 
                                    id = plot1vars(), timepoint = plot2vars())
                   return(p)
                 })
                 
                 output$plot1 <- renderPlot({
                   plot1_obj()
                 })
                 output$plot2 <- renderPlot({
                   plot2_obj()
                 })
               }
  )
}






ui <- fluidPage(
  importUI("import"),
  varselect_ui("select"),
  egonet_ui("scatter")

)



server <- function(input, output, session) {
  dataset <- importSE("import")
  plotvars <- varselect_server("select", dataset = dataset)
  egonet_server("scatter", dataset = dataset, plot1vars = plotvars$id,
                     plot2vars = plotvars$timepoint)
  
}

shinyApp(ui, server)
