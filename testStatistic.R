library(tidyverse)
library(daewr)

# -------- Computing a vector of Z_ij's --------

# Use this function on a vector of quantities and a vector of assiciated subgroup IDs,
# e.g. columns in a data frame with headers that look like: | quantity | ID |
compute_Z_ij <- function(quantity, ID)
{
  Z_ij <- ID %>%                                        # Take the IDs
    lapply(function(x) median(quantity[ID == x])) %>%   # Make a vector that contains the median of the quantities with each ID
    unlist() %>%                                        # Drop the outside nested list
    - quantity %>%                                      # Subtract each quantity (vectorized)
    abs()                                               # Take the magnitude
  return(Z_ij)
}


# -------- Computing a vector of Z^bar_i's --------

# Use this function on a vector of Z_ij data (computed with the compute_Z_ij function) and
# a vector of associated subgroup IDs (Same ID vector as used in compute_Z_ij)
compute_Zbar_i <- function(Z_ij, ID)
{
  Zbar_i <- ID %>%                                # Take the IDs
    unique() %>%                                  # Remove duplicates
    lapply(function(x) mean(Z_ij[ID == x])) %>%   # Take the mean of the Z_ij's with each unique ID
    unlist()                                      # Drop the outside nested list
  return(Zbar_i)
}


# -------- Computing an extended vector of Zbar_i's --------

# Use this function on a vector of Z_ij data (computed with the compute_Z_ij function) and
# a vector of associated subgroup IDs (Same ID vector as used in compute_Z_ij). This function
# is exactly the same as the one before except it repeats each Zbar_i as many times as data
# values in each subgroup
compute_Zbar_i_ext <- function(Z_ij, ID)
{
  Zbar_i_ext <- ID %>%                            # Take the IDs
    lapply(function(x) mean(Z_ij[ID == x])) %>%   # Take the mean of the Z_ij's with each unique ID
    unlist()                                      # Drop the outside nested list
  return(Zbar_i_ext)
}


# -------- Proof of concept for Z_ij using simple data --------
# quantity <- c(1, 2, 0, 3, 1, 11, 15, 15, 13, 11, 21, 22, 28, 23, 21, 26, 32, 32, 35, 35, 30)
# ID <- c(0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3)
# df <- cbind.data.frame(quantity, ID)
# test_Z_ij <- compute_Z_ij(df$quantity, df$ID)
# print(test_Z_ij)
# 
# # -------- Proof of concept for Zbar_i using the test data --------
# test_Zbar_i <- compute_Zbar_i(test_Z_ij, df$ID)
# print(test_Zbar_i)
# 
# # -------- Proof of concept for Zbar_i_ext using the test data --------
# test_Zbar_i_ext <- compute_Zbar_i_ext(test_Z_ij, df$ID)
# print(test_Zbar_i_ext)



# -------- Proof of concept from start to finish using the test data --------

# n <- length(df$quantity)       # Number of observations
# I <- length(unique(df$ID))     # Number of subgroups
# n_i <- df$ID %>%               # Number of observations in each subgroup
#   unique() %>%
#   lapply(function(x) length(df$ID[df$ID == x])) %>%
#   unlist
#
#
# Z_ij <- compute_Z_ij(df$quantity, df$ID)
# Zbar_i <- compute_Zbar_i(Z_ij, df$ID)
# Zbar_i_ext <- compute_Zbar_i_ext(Z_ij, df$ID)
#
# Zbar.. <- mean(Z_ij)
#
# numerator <- (n - I) * sum(n_i * ((Zbar_i - Zbar..)^2))
#
# denominator <- (I - 1) * sum((Z_ij - Zbar_i_ext)^2)
#
# result <- numerator / denominator
# print(result)

# -------- Using the same procedure on our data --------

# Loading data
wd <- "C:/Users/Student/Documents/PLU/Spring 2020/Capstone B/FoodProject"
setwd(wd)
leveneData <- read.csv("leveneData.csv", header = TRUE, stringsAsFactors = FALSE)

n <- length(leveneData$Quantity)       # Number of observations
I <- length(unique(leveneData$ID))     # Number of subgroups
n_i <- leveneData$ID %>%               # Number of observations in each subgroup
  unique() %>%
  lapply(function(x) length(leveneData$ID[leveneData$ID == x])) %>%
  unlist

Z_ij <- compute_Z_ij(leveneData$Quantity, leveneData$ID)
Zbar_i <- compute_Zbar_i(Z_ij, leveneData$ID)
Zbar_i_ext <- compute_Zbar_i_ext(Z_ij, leveneData$ID)

Zbar.. <- mean(Z_ij)

numerator <- (n - I) * sum(n_i * ((Zbar_i - Zbar..)^2))

denominator <- (I - 1) * sum((Z_ij - Zbar_i_ext)^2)

result <- numerator / denominator
print(result)

print(paste("N - I is ", n - I))
print(paste("I - 1 is ", I - 1))

# Critical F-distribution values at 40 and 1000 degrees of freedom:")
# 1.30, 1.41, 1.50, 1.61, 1.87
# Corresponding p:
# .100, .050, .025, .010, .001
Fcrit(0.05, 39, 1444)

# -------- Getting diffs from median for boxplot --------

boxplot_par <- function(quantity, ID)
{
  parameter <- ID %>%                                   # Take the IDs
    lapply(function(x) median(quantity[ID == x])) %>%   # Make a vector that contains the median of the quantities with each ID
    unlist() %>%                                        # Drop the outside nested list
    - quantity                                    # Subtract each quantity (vectorized)
  return(parameter)
}

boxplot_pars <- boxplot_par(leveneData$Quantity, leveneData$ID)
boxplot_df <- cbind.data.frame(boxplot_pars, leveneData$ID)
names(boxplot_df) <- c("Parameter", "ID")

# Boxplots for difference from the median for each group
op <- par(cex = 1.2)
boxplot(Parameter ~ ID, data = boxplot_df,
        outline = FALSE,
        frame.plot = FALSE,
        boxlwd = 0.1,
        medlwd = 0.1,
        main = "Difference from each Subgroup Median",
        ylab = "% Low Access")

# Zoomed in on display error
boxplot(Parameter ~ ID, data = boxplot_df,
        outline = FALSE,
        frame.plot = FALSE,
        boxlwd = 0.1,
        medlwd = 0.1,
        xlim = c(0, 5), ylim = c(-5, 5),
        xaxt = "n", xlab = NA,
        ylab = "% Low Access")

# Boxplots for diff from the median for the groups w/ 5 smallest Zbar_i
small_IDs <- append(seq(1, 77, 2), 0)[order(Zbar_i)[1:5]]
boxplot_small <- boxplot_df[boxplot_df$ID %in% small_IDs,]
boxplot(Parameter ~ ID, data = boxplot_small,
        outline = FALSE,
        frame.plot = FALSE,
        boxlwd = 0.1,
        medlwd = 0.1,
        main = expression(bold("Difference from the Subgroup Median: Smallest 5 Z"[i])),
        ylab = "% Low Access")

# W > F_critical at .999 confidence level
# REJECT THE NULL HYPOTHESIS THAT THE VARIANCES ARE EQUAL.
# 
# # Now let's try again without the display error data:
# leveneWout <- leveneData[leveneData$ID != 0,]
# 
# n <- length(leveneWout$Quantity)       # Number of observations
# I <- length(unique(leveneWout$ID))     # Number of subgroups
# n_i <- leveneWout$ID %>%               # Number of observations in each subgroup
#   unique() %>%
#   lapply(function(x) length(leveneWout$ID[leveneWout$ID == x])) %>%
#   unlist
# 
# Z_ij <- compute_Z_ij(leveneWout$Quantity, leveneWout$ID)
# Zbar_i <- compute_Zbar_i(Z_ij, leveneWout$ID)
# Zbar_i_ext <- compute_Zbar_i_ext(Z_ij, leveneWout$ID)
# 
# Zbar.. <- mean(Z_ij)
# 
# numerator <- (n - I) * sum(n_i * ((Zbar_i - Zbar..)^2))
# 
# denominator <- (I - 1) * sum((Z_ij - Zbar_i_ext)^2)
# 
# result <- numerator / denominator
# print(result)
# 
# print(paste("N - I is ", n - I))
# print(paste("I - 1 is ", I - 1))
# Fcrit(0.001, 38, 1406)