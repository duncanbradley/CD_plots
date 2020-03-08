library(tidyverse)

# the following function takes the parameters of a dataset and uses them to 
# build a dataframe. 
create_df <- function(my_df){
  set.seed(1234)
  y <- c(my_df$y_value1, my_df$y_value2, my_df$y_value3)
  x <- c(my_df$x_category1, my_df$x_category2, my_df$x_category3)
  df <- as_tibble(cbind(y, x)) %>%
    mutate(y = as.double(y)) %>%
    mutate(x = as.factor(x)) 
  
}

#ggplot(aes(y = y,
#           x = x),
#       data = df) +
#  geom_col()

# creating a wrapper function to wrap the title text
wrapper <- function(x, ...) 
{
  paste(strwrap(x, ...), collapse = "\n")
}

my_bar_graph <- function(df, labx, laby, title) {
  set.seed(1234)
  df %>%
    ggplot(aes(x = x, y = y)) +
    geom_col(position = "dodge2") +
    labs(x = labx, y = laby, title = title) +
    theme(text = element_text(size = 18),
          panel.grid = element_blank()) +
    expand_limits(y = 0) +
    ggtitle(wrapper(title, width = 30)) +
    theme(panel.background = element_rect(fill = "white"),
          plot.title = element_text(colour = "white"))
}

save_neutral_graph <- function(current_graph){
  ggsave(paste0("graphs/graph_neutral_", index, ".jpg"), current_graph, width = 5, height = 5.6, units = "in")
}  

save_slant_graph <- function(current_graph){
  ggsave(paste0("graphs/graph_slant_", index, ".jpg"), current_graph, width = 5, height = 5.6, units = "in")
}  

# this reads in the .csv file that contains the parameters of the graphs to be
# generated
my_graphs <- read_csv("graphs_book.csv")

# build bar graphs
for(index in my_graphs$graph_id) {
  
  build_this_one <- my_graphs %>%
    filter(graph_id == index) %>%
    create_df() 
  
  my_bar_graph(build_this_one, 
               my_graphs[my_graphs$graph_id == index,]$x_label, 
               my_graphs[my_graphs$graph_id == index,]$y_label, 
               my_graphs[my_graphs$graph_id == index,]$title_neutral) %>%
    save_neutral_graph()
  
  my_bar_graph(build_this_one, 
               my_graphs[my_graphs$graph_id == index,]$x_label, 
               my_graphs[my_graphs$graph_id == index,]$y_label, 
               my_graphs[my_graphs$graph_id == index,]$title_slant) %>%
    save_slant_graph()
}

