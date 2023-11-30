# Create a simple function to make boxplots
plot_simple_boxplot <- function(column) {
  
  column <- sym(column)
  
  ggplot(drwho, aes(fct_reorder(!!column, rating), rating)) + 
    geom_boxplot()
  
}