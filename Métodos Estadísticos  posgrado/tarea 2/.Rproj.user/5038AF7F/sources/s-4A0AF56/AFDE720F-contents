
library(readr)
library(data.tree)
library(dplyr)
library(stringr)
library(DiagrammeR)
prob_data <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQWc07o1xTNCJcGhw-tWYAnD3xPCjS0_jE4CIBR-rp5ff3flVGJQf2K24bJ5FE-DauQvLrtB8wWJNuc/pub?gid=0&single=true&output=csv")

prob_data <- prob_data %>%  mutate(tree_level = str_count(string = pathString, pattern = "/") + 1,
                                   tree_group = str_replace(string = pathString, pattern = "/.*", replacement = ""),
                                   node_type = "decision_node"
)

max_tree_level <- max(prob_data$tree_level, na.rm = T) 


parent_lookup <- prob_data %>% distinct(pathString, prob) # get distinct probabilities to facilitate finding parent node probability

for (i in 1:(max_tree_level -  1)) { # loop through all tree layers to get all immidiate parent probabilities (to calculate cumulative prob)
  
  names(parent_lookup)[1] <-paste0("parent",i)
  names(parent_lookup)[2] <-paste0("parent_prob",i)
  
  for (j in 1:i) {
    
    if (j == 1)  prob_data[[paste0("parent",i)]] <- sub("/[^/]+$", "", prob_data$pathString)
    else if (j  > 1) prob_data[[paste0("parent",i)]] <- sub("/[^/]+$", "", prob_data[[paste0("parent",i)]])
  }
  
  prob_data <- prob_data %>% left_join(parent_lookup, by = paste0("parent",i))
  
}


prob_data$overall_prob <- apply(prob_data %>% select(contains("prob"))  , 1, prod, na.rm = T)  # calculate cumulative probability   

terminal_data <- prob_data %>%  filter(tree_level == max_tree_level) %>% # create new rows that will display terminal/final step calulcations on the tree
  mutate(node_type = 'terminal',
         pathString = paste0(pathString, "/overall"),
         prob = NA,
         tree_level = max_tree_level + 1)

start_node <- "weather" # name the root node

prob_data = bind_rows(prob_data, terminal_data) %>%  # bind everything together 
  mutate(pathString = paste0(start_node,"/",pathString),
         overall_prob = ifelse(node_type == 'terminal', overall_prob, NA),
         prob_rank = rank(-overall_prob, ties.method = "min", na.last = "keep"))

prob_data = bind_rows(prob_data, data.frame(pathString = start_node, node_type = 'start', tree_level = 0)) %>% # add one new row to serve as the start node label
  select(-contains("parent"))

make_my_tree <- function(mydf, display_level = NULL, show_rank = FALSE, direction = "LR") {
  
  if (!is.null(display_level) ) {
    mydf <- mydf %>% filter(tree_level <= display_level)
    
  }
  
  mytree <- as.Node(mydf) 
  
  GetEdgeLabel <- function(node) switch(node$node_type, node$prob)
  
  GetNodeShape <- function(node) switch(node$node_type, start = "box", node_decision = "circle", terminal = "none")
  
  
  GetNodeLabel <- function(node) switch(node$node_type, 
                                        terminal = ifelse(show_rank  == TRUE, paste0("Prob: ", node$overall_prob,"\nRank: ", node$prob_rank),
                                                          paste0("Prob: ", node$overall_prob)),
                                        node$node_name)
  
  SetEdgeStyle(mytree, fontname = 'helvetica', label = GetEdgeLabel)
  
  SetNodeStyle(mytree, fontname = 'helvetica', label = GetNodeLabel, shape = GetNodeShape)
  
  SetGraphStyle(mytree, rankdir = direction) 
  
  plot(mytree)
  
}

make_my_tree(prob_data)


win.graph()
make_my_tree(prob_data)
