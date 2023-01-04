library(gtools)

vec <- LETTERS[5:7]


## Set the number of observations and outcomes
combinations <- permutations(n=4, r=2, v =vec)

# Perform a combination of n observations and r outcomes
com_esi <- function(n, r){ 
  # Create a dataframe with 10 observations and 10 outcomes
  df <- data.frame(matrix(nrow = n, ncol = r))
  
  # Add the combinations to the dataframe
  df[] <- permutations(n,r)
  
  df <- mutate(data.frame(df))
  
}

df_new <- com_esi(10,10)


as <- rep("y", 10)

bs <- rep("n", 10)

df[] <- data.frame(matrix(nrow = as, ncol = ))


#Tidyverse

library(tidyverse)


x_df = rep("Y", 10)
y_df = rep("N", 10)

esi_df <- expand.grid(x_df,
                   y_df )

esi_df <- crossing(x_df = c("Y", "Y", "Y"),
                   y_df = c("N", "N", "N"), .name_repair = "minimal")


^([^\\s]+) (.*s)


output_df <- expand.grid(data.frame(replicate(10, c("Y", "N"))))
