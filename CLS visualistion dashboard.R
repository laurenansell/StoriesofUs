library(shiny)
library(shinydashboard)
library(plotly)
library(leaflet)
library(leafpop)
library(ggplot2)
library(dplyr)
library(rinat)
library(data.table)
library(networkD3)
library(lubridate)
library(likert)
library(igraph)
library(ggraph)
library(fmsb)

## Read in the data
data<-read.csv("../Data for Explainer Pack/cls_dashboard_data.csv")
asset_data<-read.csv("../Data for Explainer Pack/asset_data.csv")


ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(disable = TRUE),
  dashboardBody(fluidRow(
          box(
            title = "Select Location",
            selectInput("location", "Select Location", choices=list("Hastings"=113,"Plymouth"=186))
          )
        ),
        fluidRow(
          box(
            title = "People in this neighbourhood pull together to improve the neighbourhood",
            plotOutput("plot1")
          ),
          box(
            title = "Unpaid Help Network",
            plotOutput("plot2")
          )
        ),
        fluidRow(
          box(
            title = "Asset Data",
            plotOutput("plot3")
          )
        )
      )
)

server <- function(input, output, session) {
  
  output$plot1<-renderPlot({
    
    dashboard_data |> filter(lad25cd==as.numeric(input$location)) |> select(SPull) |> group_by(SPull) |> count() |> 
      ggplot(aes(x=SPull,y=n))+geom_bar(stat = "identity")

  })
  
  output$plot2<-renderPlot({
    
    unpaid_data<-dashboard_data |> filter(lad25cd==as.numeric(input$location)) |> 
      select(FUnPd1A,FUnPd1B,FUnPd1C,FUnPd1D,FUnPd1E,FUnPd1F,FUnPd1G,FUnPd1H,FUnPd1I,
             FUnPd1J,FUnPd1K,FUnPd1L,FUnPd1M,FUnPd1N)
    
    unpaid_data<-unpaid_data[complete.cases(unpaid_data),]
    
    help_type<-c("Raising or handling money/taking part in sponsored events",
                 "Leading a group/member of a committee",
                 "Getting other people involved",
                 "Organising or helping to run an activity or event",
                 "Visiting people",
                 "Befriending or mentoring people",
                 "Giving advice/information/counselling",
                 "Secretarial, admin or clerical work",
                 "Providing transport/driving",
                 "Representing",
                 "Campaigning",
                 "Other practical help (e.g. helping out at school, shopping)",
                 "Any other help",
                 "None of the above")
    
    labels <- c(
      FUnPd1A = "Raising money",
      FUnPd1B = "Committee member",
      FUnPd1C = "Encouraging others",
      FUnPd1D = "Helping to run",
      FUnPd1E = "Visiting people",
      FUnPd1F = "Mentoring",
      FUnPd1G = "Giving advice",
      FUnPd1H = "Admin",
      FUnPd1I = "Transport",
      FUnPd1J = "Representing",
      FUnPd1K = "Campaigning",
      FUnPd1L = "Other practical help",
      FUnPd1M = "Other help"
    )
    
    groups <- unpaid_data[,1:13]
    
    co_mat <- t(as.matrix(groups)) %*% as.matrix(groups)
    
    diag(co_mat) <- 0
    
    edges <- as.data.frame(as.table(co_mat)) |>
      rename(from = Var1,
             to = Var2,
             weight = Freq) |>
      filter(weight > 0)
    
    edges <- edges |>
      rowwise() |>
      mutate(pair = paste(sort(c(from, to)), collapse = "_")) |>
      ungroup() |>
      distinct(pair, .keep_all = TRUE) |>
      select(-pair)
    
    nodes <- data.frame(
      name = colnames(groups),
      freq = colSums(groups)
    )
    
    g <- graph_from_data_frame(
      d = edges,
      vertices = nodes,
      directed = FALSE
    )
    
    edges <- edges |>
      filter(weight >= 3)
    
    nodes<-cbind(labels,nodes)
    
    nodes<-nodes |> select(labels,freq)
    
    # Plot
    ggraph(g, layout = "fr") +
      geom_edge_link(aes(width = weight),
                     alpha = 0.5,
                     colour = "steelblue") +
      geom_node_point(aes(size = freq),
                      colour = "orange") +
      geom_node_text(aes(label = labels),
                     repel = TRUE) +
      scale_edge_width(range = c(0.5, 5)) +
      theme_void()
  
    
  })
  
  output$plot3<-renderPlot({
    
    asset_data_filtered<-asset_data |> filter(Location==as.numeric(input$location))
    
    asset_data_filtered<-asset_data_filtered |> select(-Location)
    
    rownames(asset_data_filtered) <- c(
      "General shop",
      "Pub/Bar",
      "Park",
      "Library",
      "Restaurant",
      "Community Hall",
      "Sports Facility",
      "GP",
      "Chemist",
      "Post Office",
      "Place of worship",
      "Transport Links" )
    
    radar_data <- rbind(
      rep(100, ncol(asset_data_filtered)),
      rep(0, ncol(asset_data_filtered)),
      asset_data_filtered
    )
    
    radarchart(
      radar_data,
      axistype = 1,
      
      # Colours for each ward
      pcol = c("red", "blue", "darkgreen","gold","orange","pink","purple","black","lightblue",
               "lightgreen","yellow","darkorange"),
      pfcol = c(
        rgb(1,0,0,0.2),
        rgb(0,0,1,0.2),
        rgb(0,0.5,0,0.2)
      ),
      plwd = 2,
      plty = 1,
      
      cglcol = "grey",
      cglty = 1,
      axislabcol = "black",
      vlcex = 0.9
    )
    
    legend(
      "topright",
      legend = rownames(radar_data[3:14,]),
      col = c("red", "blue", "darkgreen","gold","orange","pink","purple","black","lightblue",
               "lightgreen","yellow","darkorange"),
      lty = 1,
      lwd = 2,
      bty = "n"
    )
      
      })
  
  
  
}

shinyApp(ui, server)


