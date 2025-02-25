library(tidyverse)

##############
# Automation #
##############

# Map a function to each element of a vector/list
days_of_the_week <- c("Mon", "Tues", "Wed", "Thur", "Fri", "Sat", "Sun")
map(days_of_the_week, toupper) #Default output is a list

days_of_the_week <- c("Mon", "Tues", "Wed", "Thur", "Fri", "Sat", "Sun")
map_chr(days_of_the_week, toupper) #Use map_* to specify the output type
map_chr(days_of_the_week, ~ paste("Day: ", .x)) #Use in-line fns like in across()
#.x means every element of vector, paste adds character to each elements putting "Day: " before the .x
map_lgl(days_of_the_week, ~ str_detect(.x, "M"))

preds <- c("cyl", "disp", "hp", "drat", "wt", "qsec") #making a list of variables from the mtcars dataset
cor_output <- map(preds, ~ cor(mtcars['mpg'], mtcars[.x])) #correlates mpg vector with each variable preds list (".x" references each item of list)
cor_output

preds <- c("cyl", "disp", "hp", "drat", "wt", "qsec")
cor_output <- map_dbl(preds, ~ cor(mtcars['mpg'], mtcars[.x])) #map_dbl makes outcomes doubles, turns into vector (rather than list output)
cor_output

# Use set_names to make mapped outputs more readable
cor_output %>% set_names(preds)

preds <- c("cyl", "disp", "hp", "drat", "wt", "qsec")
output <- map(preds, ~ lm(as.formula(paste("mpg ~ ", .x)), data = mtcars))
#makes a regression predicting mpg by each vector variables from preds
output %>% set_names(preds)

# Mutate across
starwars
ds <- starwars %>% select(-where(is.list)) #removes any vectors that are lists
#across function goes through and refernces each column 
ds %>% mutate(across(everything(), as.character)) 
ds %>% mutate(across(c("height", "mass"), round)) 
ds %>% mutate(across(where(is.numeric), round)) 
ds %>% mutate(across(where(is.numeric), round, .names = "{.col}_rounded")) #adding ".names" adds new columns w/ updated names that were mutated
ds %>% mutate(across(where(is.numeric), ~ .x *100)) 
ds %>% mutate(across(ends_with("color"), ~ str_remove_all(.x, "[:punct:]")))

#############
# Iteration #
#############

# Use for loops to print output
for (i in c(1, 2, 3, 4, 5)) {
  # "i" will take on each value of the vector 1:5
  print(paste("This is loop iteration", i))
}

days_of_the_week <- c("Mon", "Tues", "Wed", "Thur", "Fri", "Sat", "Sun")
for (i in days_of_the_week) {
  print(paste("Today is", i))
}

# Use for loops to run a series of analyses
preds <- c("cyl", "disp", "hp", "drat", "wt", "qsec")
cor_output <- vector() #make empty vector for "for loop" output
for (p in preds) {
  cor_output[p] <- cor(mtcars['mpg'], mtcars[p])
} 
cor_output

preds <- c("cyl", "disp", "hp", "drat", "wt", "qsec")
for (p in preds) {
  f <- as.formula(paste("mpg ~ ", p)) #building formula within loop
  res <- lm(f, data = mtcars) #output for each iteration
  print(p) #unsure what this part does
  print(summary(res))
}

#############
# Functions #
#############

# Defining a function
hyp <- function(side_a, side_b) { #function name and argument names
  a <- side_a^2
  b <- side_b^2
  h <- sqrt(a + b)
  return(h) #define output
} #good to write out functions as steps, but could be in 1 line for easy functions

hyp <- function(side_a, side_b) { #function name and argument names
  return(sqrt(side_a^2 + side_b^2)) #define output
}

hyp(3, 4)
hyp(9, 12)

# Function arguments
hyp <- function(side_a, side_b = NULL) { #NULL denotes that input can be skipped/not defined (so you have to include a side a, but could skip side b)
  a <- side_a^2
  b <- ifelse(is.null(side_b), a, side_b^2)
  h <- sqrt(a + b)
  return(h)
}
hyp(9, 9)
hyp(9)

# Write "wrapper functions" to make your life easier
mean_na <- function(x) mean(x, na.rm = T)
sd_na <- function(x) sd(x, na.rm = T)
se_na <- function(x) {
  x <- discard(x, is.na) #discard is like "filter if not" for vectors
  sd(x)/sqrt(length(x))
}

# Use lists to return multiple outputs
summary_stats_na <- function(x) {
  # Functions can call previously defined functions
  list(mean = mean_na(x), sd = sd_na(x), se = se_na(x))
}
data <- c(4, 6, 9, 12, 5, 6, NA)
summary_stats_na(data)

# Make a summary stats function
summary_stats <- function(df, var_select_string) {
  na_mean <- function(x) mean(x, na.rm = T)
  na_sd <- function(x) sd(x, na.rm = T)
  fx_list <- list(M = na_mean, SD = na_sd)
  df  %>% summarize(across(contains(var_select_string) & where(is.numeric), fx_list))
}
ds %>% summary_stats("height")
ds %>% group_by(species) %>% filter(n() > 1) %>% summary_stats(c("height","mass"))


######################################
# Combining automation and functions #
######################################

# Summarize across
ds <- starwars %>% select(-where(is.list))
fx_list <- list(mean = mean_na, sd = sd_na, se = se_na)
ds %>% summarize(across(height:mass, mean, na.rm = T)) #Previous format, but new versions of R/dyplyr prefer below formating
ds %>% summarize(across(height:mass, ~ mean(.x, na.rm = T))) #dyplr wants you to call function and include each item
ds %>% summarize(across(height:mass, mean_na)) 
ds %>% summarize(across(where(is.numeric), fx_list)) 
ds %>% summarize(across(where(is.numeric), fx_list, .names = "{.fn}-{.col}")) 

# Use map on a split data frame
ds <- mtcars %>% mutate(trans = factor(am, labels = c("auto","manual")))
df_split <- ds %>% group_by(trans) %>% group_split %>% set_names(levels(ds$trans))
df_split
results <- map(df_split, ~ lm(mpg ~ hp, data = .x))
map(results, summary)

# Piped expressions can be saved as functions
avg_height_mass <- . %>% #. is a placeholder dataset to be entered
  select(height, mass) %>% 
  summarize(mean_ht = mean_na(height), mean_ms = mean_na(mass))

starwars %>% avg_height_mass
starwars %>% group_by(species) %>% avg_height_mass

# Use map to run a series of analyses
preds <- c("cyl", "disp", "hp", "drat", "wt", "qsec")
mpg_lm <- function(p) {
  f <- as.formula(paste("mpg ~ ", p))
  res <- lm(f, data = mtcars)
  res$call <- f #adding column call with formula (to know what was run)
  return(res)
}
results <- map(preds, mpg_lm)
map(results, summary)

# What if we wanted all combinations?
preds <- c("cyl", "disp", "hp", "drat", "wt", "qsec")
all_combos <- expand.grid(preds, preds) %>% filter(Var1 != Var2) #removes when same variable twice (so we don't predict same variable by itself)
all_combos
mpg_lm <- function(criterion, predictor) {
  f <- as.formula(paste(criterion,"~",predictor))
  res <- lm(f, data = mtcars)
  res$call <- f
  return(res)
}
results <- map2(all_combos$Var1, all_combos$Var2, mpg_lm)
map(results, summary)

###############################################
# Making functions more complex/sophisticated #
###############################################

# Control program flow with `if` and `else` statements
x <- 1
if (x == 0) {
  paste(x, "is exactly 0")
} else if (x < 0) {
  paste(x, "is negative")
} else {
  paste(x, "is positive")
} 

# Control program flow with `if` and `else` statements
x <- -1
if (x == 0) {
  paste(x, "is exactly 0")
} else if (x < 0) {
  paste(x, "is negative")
} else {
  paste(x, "is positive")
} 
