pacman::p_load(jsonlite, tidygraph, ggraph, visNetwork, graphlayouts, ggforce,skimr,tidytext, tidyverse,igraph, topicmodels,tm, stringr, ldatuning, shiny, shinythemes, tools, ggiraph, plotly, crosstalk)

mc3_nodes <- read_rds("data/mc3_nodes.rds")
mc3_edges <- read_rds("data/mc3_edges.rds")

###########################################################

Noofcompanies <- mc3_edges %>%
  group_by(target, source, type) %>%
  filter(type == "Beneficial Owner") %>%
  summarise(count=n()) %>%
  group_by(target)%>%
  summarise(count_of_companies=sum(count))%>%
  arrange(desc(count_of_companies))


combined <- left_join(mc3_nodes,mc3_edges,
                      by=c("id"="source"))

combined$revenue_omu[is.na(combined$revenue_omu)] <- 0

Noofcompanies1 <- combined %>%
  group_by(target)%>%
  summarise(revenue = sum(revenue_omu))

Noofcompanies <- inner_join(Noofcompanies,Noofcompanies1,
                            by=c("target"="target"))

d <- highlight_key(Noofcompanies) 
p <- ggplot(d, 
            aes(count_of_companies, 
                revenue,
                text = target)) + 
  geom_point(size=1) +
  coord_cartesian()

gg <- highlight(ggplotly(p),
                tooltip = c("text"),
                "plotly_selected") 

###########################################################

id1 <- mc3_edges %>%
  select(source) %>%
  mutate(type = "Company")%>%
  rename(id = source)
id2 <- mc3_edges %>%
  select(target,type) %>%
  rename(id = target)
mc3_nodes1 <- rbind(id1, id2) %>%
  distinct() %>%
  left_join(mc3_nodes,
            unmatched = "drop")

mc3_graph <- tbl_graph(nodes = mc3_nodes1,
                       edges = mc3_edges,
                       directed = FALSE)

mc3_graph <- mc3_graph %>% 
  activate(nodes) %>%
  mutate(group_id = components(mc3_graph)$membership)

#create function for filtered groupid
createNE_by_Group <- function(groupid) {
  
  relevant_entities <- mc3_graph %>%
    activate(nodes) %>%
    as.tibble()%>%
    arrange(id) %>%
    filter(group_id %in% groupid)
  
  
  relevant_edges <- mc3_edges %>%
    filter(source %in% relevant_entities$id| target %in% relevant_entities$id) %>%
    rename(from = source) %>% 
    rename(to = target) %>% 
    mutate(title = type)
  
  # extract the seafood_nodes records using the edge information
  Cid1 <- relevant_edges %>%
    select(from) %>%
    rename(id = from)
  
  Cid2 <- relevant_edges %>%
    select(to) %>%
    rename(id = to)
  
  # Get unique nodes from source and target columns of edge records
  # left join with the mc3_nodes_cleaned3 dataset to get the attributes for the nodes
  relevant_nodes <- rbind(Cid1, Cid2) %>%
    left_join(mc3_nodes1,by=c('id')) %>%
    distinct(id, .keep_all = TRUE)%>%
    arrange(id) %>%
    mutate(label = id) %>%
    mutate(group = type)
  
  title = paste("Subsidiary Group ID",groupid)
  
  list(relevant_edges = relevant_edges, relevant_nodes = relevant_nodes, title = title)
  
}

#create function for filtered id
createNE_by_id <- function(entityid) {
  
  # obtain the subgraph group id of the entity
  groupid <- mc3_graph %>% 
    activate(nodes) %>%
    filter(id == entityid) %>%
    select(group_id) %>%
    as.tibble()
  
  # assign the group id to a variable
  groupid <- groupid$group_id[]
  
  # pass the group id into the createNE_by_Group() function to generate the subgraph
  output <- createNE_by_Group(groupid)
  
  # return the nodes and edges data frames for graphing
  list(relevant_edges = output$relevant_edges,relevant_nodes = output$relevant_nodes,title = entityid)
  
}

#create Network Graph
createGraph <- function(r_nodes , r_edges,title) {
  
  visNetwork(nodes = r_nodes,   # Visualize the nodes
             edges = r_edges,   # Visualize the edges
             main = paste("Network graph of", title),
             height = "500px", width = "100%") %>%
    visIgraphLayout(layout = "layout_nicely") %>%
    visEdges(smooth = list(enables = TRUE, type = 'straightCross'),  # Customize the appearance of edges
             shadow = FALSE,
             dash = FALSE) %>%
    visOptions(highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE),  # Enable highlighting of nearest nodes on hover
               nodesIdSelection = TRUE,
               selectedBy = "group") %>%  # Enable node selection by group
    visInteraction(hideEdgesOnDrag = TRUE) %>%  # Hide edges while dragging nodes
    visLegend() %>%  # Display legend
    visLayout(randomSeed = 123)  # Set a random seed for consistent layout
}


###########################################################

ui <- navbarPage(
  title = "Countering Illegal Fishing Activities",
  fluid = TRUE,
  theme = shinytheme("flatly"),
  navbarMenu("Question 1",
             tabPanel("Beneficial Owner's company count and revenue and Network Graph"),
                        mainPanel(
                          width = 9,
                          bscols(plotlyOutput("gg"),               
                                DT::datatable(d))

                        ),
             tabPanel("Network graph of selected target"),
             sidebarLayout(
               sidebarPanel(
                 textInput("Target","Enter Target Name (*case-senstive):", value = "Michael Johnson")
               ),
               mainPanel(
                 visNetworkOutput("networkGraph")
               )

                        
                      )
             
             
             )
                      
             )

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$gg <- renderPlotly({gg})
  
  output$networkGraph <- renderVisNetwork({
    result<-createNE_by_id(input$Target)
    createGraph(result$relevant_nodes,result$relevant_edges,result$title)
  })

}

# Run the application 
shinyApp(ui = ui, server = server)



