library(tidyverse)
library(MASS)

create_mult_df <- function(my_df){
  set.seed(1234+index)
  
  mean1 <- my_df$mean_x
  mean2 <- my_df$mean_y
  sd1 <- my_df$sd_x
  sd2 <- my_df$sd_y
  rel <- my_df$relationship
  
  mu <- c(mean1, mean2) 
  
  myr <- rel * sqrt(sd1) * sqrt(sd2)
  
  mysigma <- matrix(c(sd1, myr, myr, sd2), 2, 2) 
  
  my_data <- NULL
  sample_size <- 50
  
  my_data <- data.frame(mvrnorm(sample_size, mu, mysigma, empirical = TRUE))
  
  colnames(my_data) <- c(my_df$'x-label', my_df$'y-label')
  
  return(my_data)
}

# creating a wrapper function to wrap the title text
wrapper <- function(x, ...) 
{
  paste(strwrap(x, ...), collapse = "\n")
}


my_scatter_graph <- function(df, labx, laby, title) {
  set.seed(1234+index)
  colnames(df) <- c("x", "y")
  df %>%
    ggplot(aes(x = x, y = y)) +
    geom_point() +
    labs(x = labx, y = laby) +
    theme(text = element_text(size = 18),
          panel.grid = element_blank(),
          panel.background = element_rect(fill = "white"),
          panel.border = element_rect(colour = "black", fill = NA)) +
    ggtitle(wrapper(title, width = 43))

}

save_graph <- function(current_graph){
  ggsave(paste0("graphs/graph_", index, ".jpg"), current_graph)
  
}  


# MAIN CODE ####
# this reads in the .csv file that contains the parameters of the graphs to be
# generated
my_graphs <- read_csv("graphs_book.csv")

# this loops through the my_graphs which contains the paramters of the 
# graphs to be generated.  It runs once per each unique graph_id 
for(index in my_graphs$graph_id) {
  
  build_this_one <- my_graphs %>%
    filter(graph_id == index) %>%
    create_mult_df() 
  
  my_scatter_graph(build_this_one, 
                   my_graphs[my_graphs$graph_id == index,]$'x_label', 
                   my_graphs[my_graphs$graph_id == index,]$'y_label', 
                   my_graphs[my_graphs$graph_id == index,]$title_slant) %>%
    save_graph()
  
}
