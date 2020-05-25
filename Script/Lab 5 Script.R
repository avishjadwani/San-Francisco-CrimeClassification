# load the library
library(tidyverse)

# load the data
census <- read.csv('census.csv', header = TRUE, sep = ',')
str(census)

# create samples
censusSample <- sample_n(census, 1000)
censusMasters <- census %>% 
  dplyr::filter(education == ' Masters' )

# one sample z test
z.test <- function(sample, pop){
  sample_mean = mean(sample) 
  pop_mean = mean(pop)
  n = length(sample) 
  var = var(pop)
  z = (sample_mean - pop_mean) / (sqrt(var/(n))) 
  return(z)
}

# performing the z test
z.test(censusSample$age, census$age)

# performing the t test
t.test(censusMasters$age, mu =) 
?t.test
# two sample z test
z_test2 = function(a, b, var_a, var_b){
  n.a = length(a)
  n.b = length(b)
  z = (mean(a) - mean(b)) / (sqrt((var_a)/n.a + (var_b)/n.b))
  return(z)
}

# create more samples
cen_1 <- census[1:16000,] #select all columns and rows from 1 to 16000  
cen_2 <- census[16001:32561,] #select all columns and rows from 16001 to 32561
cen_1_sample <- sample_n(cen_1, 1000) #sample 1000 rows from the first population
cen_2_sample <- sample_n(cen_2, 1000) #sample 1000 rows from the second population

# performing two sample z test
z_test2(censusMasters$age, censusBachelors$age, var(census$age), var(census$age))

# performing two sample t test
t.test(censusBachelors$age, censusMasters$age)

# performing chi-squared test
chisq.test(census$education, census$occupation)

