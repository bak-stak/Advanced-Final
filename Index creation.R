#INDEX CREATION 

#We want to create a index for political trust, we will therefore run a FA, on selected trust
#related items from ITANES data to check wether the items are related

#Load required packages

library(tidyverse)
library(corrplot)
library(psych)
pca_library <- c("corrr", "ggcorrplot", "FactoMineR", "factoextra")

lapply(pca_library, library, character.only = TRUE)

#Keep just relevant VARs

trust_data_NA <- ITANES_2022_Post_electoral%>%
  select(seriale, Q04b, Q05b, Q05c, Q07a, Q07b, Q07e, Q100a, Q12, Q15, Q22)%>%
  mutate(across(-seriale, ~ ifelse(.x >= 50, NA, .x))) %>%
  mutate( #reverse were needed to put the negative connotation on the high end
    Q04b = 6 - Q04b,
    Q05b = 6 - Q05b,
    Q05c = 6 - Q05c,
    Q15  = 6 - Q15
  )

# NA imputation

# Here NA imputation is quite important: we can't just remove NA as we probably
# are in a MNAR (missing not at random) situation, moreover these are a lot of 
# items, it would be quite bad to lose all the informations for just 1 NA
# We can use mice (multiple imputation by chained equations) that take the most
# similar observation which have a value in the variable of interest

library(mice)

imputation <- mice(trust_data_NA,
                   m = 1,
                   method = "pmm",
                   seed = 123,
                   printFlag = FALSE)

trust_data <- complete(imputation, 1)


#Transform Q06 as numeric

# Isolate cateogrical variable (every variable except for q06 which is a 0-10 likert scale)
# For Fa is key not to mix continuos and categorical var and advise the model when you do it
# For this reason i am creating cat_var

cat_var <- c("Q04b", "Q05b", "Q05c", "Q07a", "Q07b","Q07e", 
             "Q12", "Q15", "Q22") 

cat_var2 <- c("Q04b", "Q05b", "Q05c", "Q07a", "Q07b","Q07e") 
# With mixcor we impute a mixed correlation matrix

mixed_cormat <- mixedCor(data = trust_data,
                         c = cat_var2,
                         smooth = TRUE)

# Plot the correlation 

corrplot(mixed_cormat$rho, method = "number",
         type = "lower", order = "hclust",
         tl.col = "black", tl.srt = 45)

# Since there are a lot of strong correlation use pca to determine how many 
# dimension to use 

eigenvalues <- eigen(mixed_cormat$rho)$values

explained_variance <- (eigenvalues / sum(eigenvalues)) * 100

df_screeplot <- data.frame(
  Component = 1:length(eigenvalues),
  Eigenvalues = eigenvalues,
  Variance <- explained_variance
)

print(df_screeplot)

# We have 3 component with >1 eigenvalue, so for the Kaiser method (keeping PCs)
# with EV > 1 we would take 3 of them. Anyway, we can also use a screeplot to visualize it

ggplot(df_screeplot, aes(x = as.factor(Component), y = Variance)) +
  geom_bar(stat = "identity", fill = "steelblue", width = 0.6) +
  geom_line(aes(x = Component, y = Variance), color = "black", linewidth = 0.8) +
  geom_point(aes(x = Component, y = Variance), color = "red", size = 3) +
  geom_hline(yintercept = (1/length(eigenvalues))*100, linetype = "dashed", color = "darkgray") +
  geom_text(aes(label = round(Variance, 1)), vjust = -1.5, size = 3.5) +
  labs(title = "Scree Plot",
       subtitle = "Elbow method",
       x = "Components",
       y = "Explained variance") +
  theme_minimal()

# The screeplot shows a sharp cut between first and second component, suggesting that
# We can try both 1 & 3 components

pca_model <- principal(r = mixed_cormat$rho,
                       nfactors = 1,
                       rotate = "varimax")

pca_model$loadings
pca_model$uniquenesses

pca_model3 <- principal(r = mixed_cormat$rho,
                        nfactors = 2,
                        rotate = "varimax")

pca_model3$loadings
pca_model3$uniquenesses
pca_model3$weights


# Perform FA with 3 dimension 
fa <- fa(r = mixed_cormat$rho,
         nfactors = 2,
         rotate = "varimax")

# Summarize fa and extract loadings & scores
summary(fa)
fa$loadings
fa$uniquenesses
fa$weights

# We now discovered 2 indexes: institutional distrust (MR1) & Stealth democracy attitudes (MR2)
# Stealth democracy: want the system to work well but don't want the politics to 
# lead it

#Create the new weighted indexes & scale it 0-10

trust_data <- trust_data %>%
  mutate(Instituional_distrust = Q04b*-0.002 + Q05b*-0.036 + Q07a*0.501 + 
           Q05c*-0.006 + Q07b*0.331 + Q07e*0.211,
         Stealth_democracy = Q04b*0.3 + Q05b*0.347 + Q07a*0.085 +
           Q05c*0.296 + Q07b*0.038 + Q07e*-0.108)

#we determine a function to rescale -5 +5

rescale <- function(x) {
  x_numeric <- as.numeric(x) #force x to be numeric
  
  x_min <- min(x_numeric, na.rm = TRUE)
  x_max <- max(x_numeric, na.rm = TRUE)
  
  (((x_numeric - x_min) * 10) / (x_max - x_min)) - 5
}

trust_rescaled <- trust_data %>%
  mutate(Instituional_distrust = rescale(Instituional_distrust),
         Stealth_democracy = rescale(Stealth_democracy))
