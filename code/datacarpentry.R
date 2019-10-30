library(tidyverse)

#Exercise 1 ----
#1
lat1 <- 48.86
lat2 <- 41.89
long1 <- 2.35
long2 <- 2.5


# Check if two geographic points are near each other 
near <- function(lat1, long1, lat2, long2){
  if ((abs(lat1 - lat2) < 1 && (abs(long1 - long2) < 1))) {
    near <- TRUE
  } else {
    near <- FALSE
  }
  #When the function is run, either "TRUE" or "FALSE" will appear in the console
  return(near)
}
#The function must be run with inputs to produce a result
near(lat1,long1,lat2,long2)
#2
#3 - No
#4 Yes
#5
lim <- 7
# Check if two geographic points are near each other 
nearlim <- function(lat1, long1, lat2, long2, lim){
  if ((abs(lat1 - lat2) < lim && (abs(long1 - long2) < lim))) {
    near <- TRUE
  } else {
    near <- FALSE
  }
  #When the function is run, either "TRUE" or "FALSE" will appear in the console
  return(near)
}

#The function must be run with inputs to produce a result
nearlim(lat1,long1,lat2,long2,lim)
#7 - YES

#Exercise 2 ----
sequences <- read_csv("data/sequences.csv")
sequence <- "attggc"
get_gc_content <- function(sequence) {
  num_g <- str_count(sequence, "g")
  num_c <- str_count(sequence, "c")
  gc_content <- ((num_g + num_c) / str_length(sequence) * 100)
  return(gc_content)
}
get_gc_content(sequence)

#2

# create an empty data frame with one row for each sequence
gc_contents <- data.frame(gc_content = numeric(nrow(sequences)))

# loop over sequences using an index for the row and
# store the output in the new data frame
for (j in 1:nrow(sequences)){
  gc_contents[j,] <- get_gc_content(sequences$seq[j])
}

# Exercise 3 ---------

# Estimates the mass of an organism in kg based on its length in meters for a particular set of parameter values, those for Theropoda a = 0.73 and b = 3.63
length = 16
get_mass_from_length_theropoda <- function(length){
  mass <- 0.73 * length ^ 3.63
  return(mass)
}
get_mass_from_length_theropoda(length)
# Estimate mass usings all three parameters
a <- 10.95
b <- 2.64
len <- 12
get_mass_from_length <- function(len, a, b) {
  mass <- a * len ^ b
  return(mass)
}
new_mass <- get_mass_from_length(len, a, b)

# Exercise 4 ---- 
kg_to_lb <- function(mass) {
  weight <- mass * 2.205
  return(weight)
}
kg_to_lb(new_mass)
#17055

# Exercise 5 -----

theropoda_lengths <- c(17.8013631070471, 20.3764452071665, 14.0743486294308, 25.65782386974, 26.0952008049675, 20.3111541103134, 17.5663244372533, 11.2563431277577, 20.081903202614, 18.6071626441984, 18.0991894513166, 23.0659685685892, 20.5798853467837, 25.6179254233558, 24.3714331573996, 26.2847248252537, 25.4753783544473, 20.4642089867304, 16.0738256364701, 20.3494171706583, 19.854399305869, 17.7889814608919, 14.8016421998303, 19.6840911485379, 19.4685885050906, 24.4807784966691, 13.3359960054899, 21.5065994598917, 18.4640304608411, 19.5861532398676, 27.084751999756, 18.9609366301798, 22.4829168046521, 11.7325716149514, 18.3758846100456, 15.537504851634, 13.4848751773738, 7.68561192214935, 25.5963348603783, 16.588285389794)

a5 <- 0.73
b5 <- 3.63
len5 <- 12
get_mass_from_length <- function(len, a, b) {
  mass <- a * len ^ b
  return(mass)
}

masses <- rep(NA, length(theropoda_lengths))
for (j in 1:length(theropoda_lengths)) {
  masses[j] <- a5 * theropoda_lengths[j] ^ b5
}


a_values <- c(0.759, 0.751, 0.74, 0.746, 0.759, 0.751, 0.749, 0.751, 0.738, 0.768, 0.736, 0.749, 0.746, 0.744, 0.749, 0.751, 0.744, 0.754, 0.774, 0.751, 0.763, 0.749, 0.741, 0.754, 0.746, 0.755, 0.764, 0.758, 0.76, 0.748, 0.745, 0.756, 0.739, 0.733, 0.757, 0.747, 0.741, 0.752, 0.752, 0.748)

b_values <- c(3.627, 3.633, 3.626, 3.633, 3.627, 3.629, 3.632, 3.628, 3.633, 3.627, 3.621, 3.63, 3.631, 3.632, 3.628, 3.626, 3.639, 3.626, 3.635, 3.629, 3.642, 3.632, 3.633, 3.629, 3.62, 3.619, 3.638, 3.627, 3.621, 3.628, 3.628, 3.635, 3.624, 3.621, 3.621, 3.632, 3.627, 3.624, 3.634, 3.621)
 masses_loop <- rep(NA, length(theropoda_lengths))
mass_loop <- function(a, b, len) {
 
  for(j in 1:length(theropoda_lengths)) { 
    masses_loop[j] <- a_values[j] * theropoda_lengths[j] ^ b_values[j]
  }
  return(masses_loop)
}
mass_loop(a_values, b_values, theropoda_lengths)

#store values to a vector
vectors_loop <- rep(NA, length(theropoda_lengths))
vector_loop <- function(a, b, len) {
  
  for(j in 1:length(theropoda_lengths)) { 
    vectors_loop[j] <- a_values[j] * theropoda_lengths[j] ^ b_values[j]
    
  }
  return(masses_loop)
}
mass_loop(a_values, b_values, theropoda_lengths)


# Exercise 6 ----
length <- 10
name <- "Stegosauria"
get_mass_from_length_by_name <- function(length, name) { 
  if (name == "Stegosauria") {a <- 10.95
  b <- 2.64}
  else{ if (name == "Theropoda") {a <- 0.73
  b <- 3.36}
    else{ if (name == "Sauropoda") { a <- 214.44
    b <- 1.46}
      else{ a <- NA
      b <- NA}}}
  mass <- a * length ^ b
  return(mass)
  }
get_mass_from_length_by_name(length, name)





















