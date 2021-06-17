library(tidyverse)
library(stringr)
library(data.table)
library(ggplot2)
library(gridExtra)

#CSV data loading
df <-  population40900

#or

df <-  population106900

# filter to keep only 100 times evaluated lines 
fully_evaluated_lines <-  df %>%  filter(`evolution$samples` == 50)

df$evolution.samples

# function that build ggplot histogramm for a given line and column name 
# if the values are all zeros, NULL is returned 
do_the_histogram <- function(line_index, fully_evaluated_lines, column_name){
  
  myline <-  fully_evaluated_lines[line_index,column_name]

  # conversion of the line into values 
  
  #remove brackets
  myline <- myline%>% str_remove(pattern="\\[") %>% str_remove(pattern="\\]") 
  
  list_of_strings  <-  str_split(myline,pattern = ",") 
  #flatten the list
  vector_of_strings <-  unlist(list_of_strings)
  # conversion to numeric
  vector_of_values <-  as.numeric(vector_of_strings)
  
  # no histogramm if all_values are zeros 
  if(all(vector_of_values == 0)){
   return(NULL)
  }else{
  values_df <-  as.data.frame(vector_of_values)
  names(values_df) <-  column_name
  # here aes_string is important instead of aes, because we provide the column name as a quoted string
  myplot <- ggplot(values_df, aes_string(column_name))+ 
    geom_histogram() +
    theme(text = element_text(size = 22))
  return(myplot)
  }
}

do_the_density <- function(line_index, fully_evaluated_lines, column_name){
  
  myline <-  fully_evaluated_lines[line_index,column_name]
  
  # conversion of the line into values 
  
  #remove brackets
  myline <- myline%>% str_remove(pattern="\\[") %>% str_remove(pattern="\\]") 
  
  list_of_strings  <-  str_split(myline,pattern = ",") 
  #flatten the list
  vector_of_strings <-  unlist(list_of_strings)
  # conversion to numeric
  vector_of_values <-  as.numeric(vector_of_strings)
  
  # no histogramm if all_values are zeros 
  if(all(vector_of_values == 0)){
    return(NULL)
  }else{
    values_df <-  as.data.frame(vector_of_values)
    names(values_df) <-  column_name
    # here aes_string is important instead of aes, because we provide the column name as a quoted string
    myplot <- ggplot(values_df, aes_string(column_name)) + 
      geom_density(color="darkblue", fill="lightblue") +
      theme(text = element_text(size = 22))
    return(myplot)
  }
}



############################"
#Plotting one particular line 
##########################"
do_the_histogram(line_index = 1, fully_evaluated_lines, column_name = "feedingBudget_1")
do_the_histogram(line_index = 1, fully_evaluated_lines, column_name = "travellingBudget")
do_the_histogram(line_index = 1, fully_evaluated_lines, column_name = "restingBudget_1")

do_the_density(line_index = 1, fully_evaluated_lines, column_name = "feedingBudget_1")
do_the_density(line_index = 1, fully_evaluated_lines, column_name = "travellingBudget")
do_the_density(line_index = 1, fully_evaluated_lines, column_name = "restingBudget_1")


##############################
# Build a list of ggplot object for every  line of a given column
#################################"

list_of_plots <-  list()
for(l in 1:nrow(fully_evaluated_lines)){
    plot <-  do_the_histogram(l, fully_evaluated_lines, "travelDistance")
    list_of_plots[[l]] <-  plot
}
#filter the NULL plots (because of all zero values) using the  discard function  of tidyverse package 
list_of_plots <- discard(list_of_plots,is.null)

# function to rearange the plots (package gridextra)
#might take a long time if many many plots 
# width and height values have to be adjusted by hand (they are in inches )
finalplot <- grid.arrange(grobs = list_of_plots, ncol = 4) ## display the combined plot
ggsave("~/tmp/Combined_histograms.png",plot = finalplot,device = "png",width = 8, height=12)



