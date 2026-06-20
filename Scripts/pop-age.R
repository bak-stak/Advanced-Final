#Populist parties: still a home for the politically distrustful, or a faded and institutionalized phenomenon? 

#Directory: C:/Users/39331/Desktop/Advanced multivariate analysis

#Library needed----
library(haven) #reading dta file
library(tidyverse)
library(lubridate) #used to extract year from data_nascita
library(stargazer)
library(sjPlot)


#load the ITANES database & keep interest VARs----

ITANES_2022_Post_electoral <- read_dta("Itanes2022_POST_release 01_weighted.dta")

ITANES_2022_Post_electoral <- ITANES_2022_Post_electoral%>%
  mutate(
    data_pulita = dmy(data_nascita),
    anno_nascita = year(data_pulita),
    anno_nascita = if_else(anno_nascita > 2022, anno_nascita - 100, anno_nascita),
    eta = 2022 - anno_nascita #create age variable 
  )

#INDEX CREATION----

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
  mutate(Institutional_distrust = Q04b*-0.002 + Q05b*-0.036 + Q07a*0.501 + 
           Q05c*-0.006 + Q07b*0.331 + Q07e*0.211,
         Stealth_democracy = Q04b*0.3 + Q05b*0.347 + Q07a*0.085 +
           Q05c*0.296 + Q07b*0.038 + Q07e*-0.108)

#we determine a function to rescale -5 +5

rescale <- function(x) {
  x_numeric <- as.numeric(x) #force x to be numeric
  
  x_min <- min(x_numeric, na.rm = TRUE)
  x_max <- max(x_numeric, na.rm = TRUE)
  
  (((x_numeric - x_min) * 10) / (x_max - x_min))
}

trust_rescaled <- trust_data %>%
  mutate(Institutional_distrust = rescale(Institutional_distrust),
         Stealth_democracy = rescale(Stealth_democracy))%>%
  select(seriale, Institutional_distrust, Stealth_democracy)
#FINAL DF ----

populist_df <- ITANES_2022_Post_electoral%>%
  select(seriale, eta_classe, Q10LHa, Q10LHb,eta)%>%
  mutate(seriale = as.numeric(zap_labels(seriale)),
         Abstension = as.numeric(zap_labels(Q10LHa)))%>%
  filter(Abstension < 3)%>%
  mutate(Abstension = ifelse(Abstension == 2, 1, 0),
         Abstension = as.factor(Abstension))%>%
  mutate(populist_vote = Q10LHb %in% c("2","5"))%>%
  select(seriale, eta_classe, populist_vote, eta, Abstension)


pop_age <- left_join(trust_rescaled, populist_df, by = join_by(seriale))


#Data study and visualization----

ggplot(pop_age, aes(x = Institutional_distrust))+
  geom_histogram() + 
  theme_minimal()

ggplot(pop_age, aes(x = Stealth_democracy))+
  geom_histogram() + 
  theme_minimal()


#Addittive models-----

pop_age$populist_vote <- as.factor(pop_age$populist_vote)

Complete_pooling <- glm(populist_vote ~ Institutional_distrust + Stealth_democracy,
                        family = binomial(link = "logit"),
                        data = pop_age)

stargazer(Complete_pooling, type = "text")

plot_model(Complete_pooling, 
           type = "pred", 
           terms = c("Stealth_democracy", "Institutional_distrust"),
           show.data = TRUE, 
           jitter = TRUE) +
  scale_x_continuous(limits = c(0,10), breaks = 0:10) +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent_format()) +
  theme_bw()

#Try it on vote/non vote
CP_abstension <- glm(Abstension ~ Institutional_distrust + Stealth_democracy,
                        family = binomial(link = "logit"),
                        data = pop_age)

stargazer(CP_abstension, type = "text")

plot_model(CP_abstension, 
           type = "pred", 
           terms = c("Stealth_democracy", "Institutional_distrust"),
           show.data = TRUE, 
           jitter = TRUE) +
  scale_x_continuous(limits = c(0,10), breaks = 0:10) +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent_format()) +
  theme_bw()

#try fitting the model only on voters (exclude abstension)

no_abs <- pop_age%>%
  mutate(Abstension = as.numeric(Abstension))%>%
  filter(Abstension<2)

CP_voters <- glm(populist_vote ~ Institutional_distrust + Stealth_democracy,
                        family = binomial(link = "logit"),
                        data = no_abs)

stargazer(CP_voters, type = "text")

plot_model(CP_voters, 
           type = "pred", 
           terms = c("Institutional_distrust", "Stealth_democracy"),
           show.data = TRUE, 
           jitter = TRUE) +
  scale_x_continuous(limits = c(0,10), breaks = 0:10) +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent_format()) +
  theme_bw()



# INTERACTION MODEL ----

# Clean the dataset to perform interaction model and avoid confusing sjPlot

pop_age_clean <- pop_age %>%
  filter(Institutional_distrust > 0,
         Stealth_democracy > 0)%>%
  drop_na() %>%
  mutate(
    log_distrust = log1p(Institutional_distrust), #without transforming them before sjPlot wouldn't
    log_stealth  = log1p(Stealth_democracy),       #recognize them so we have to perform this now
    eta_classe   = as.factor(eta_classe)
  )

# STEALTH DEMOCRACY X AGE CLASS

stealth_x_class <- glm(populist_vote
                    ~ log_stealth
                    * eta_classe,
                    family = binomial(link = "logit"),
                    data = pop_age_clean)

stargazer(stealth_x_class, type = "text")

sjPlot::plot_model(stealth_x_class, 
                   type = "int",
                   show.legend = TRUE,
                   ) +
  theme_bw()


# Stealth x age class shows no significative correlation 

# INSTITUIONAL DISTRUST X AGE CLASS

distrust_x_class <- glm(populist_vote
                       ~ log_distrust
                       * eta_classe,
                       family = binomial(link = "logit"),
                       data = pop_age_clean)

stargazer(distrust_x_class, type = "text")

sjPlot::plot_model(distrust_x_class, 
                   type = "int",
                   show.legend = TRUE
                  ) +
  theme_bw()



# GAM - Check the shape of the distribution ----

library(mgcv)

mod_gam <- gam(populist_vote
               ~ s(Institutional_distrust)
               + s(Stealth_democracy)
               + eta_classe,
               family = binomial, data = pop_age_clean)

plot(mod_gam, pages = 1, shade = TRUE)


# It seems like at a certain point the populist vote decrease for institutional distrust
# This can suggest that after a certain ceiling non voting become the choice: this could
# be the reason for our non working models. We can try a model that take institutional
# distrust till 7/8 and check the significativity of the relation and we can do the same
# for non vote. Moreover we can impute a GAM for abstension


mod_gam <- gam(Abstension
               ~ s(Institutional_distrust)
               + s(Stealth_democracy)
               + eta_classe,
               family = binomial, data = pop_age)

plot(mod_gam, pages = 1, shade = TRUE)

# Our HP was right abstension explode for really high levels of distrust 
# Regarding stealth democracy it seems like it is more linear correlated to populist vote
# Now we can try our DBNSM (distrust but not so much) model with level lower than 8

# Before starting with our plan we want to check how many high distrust individuals we have
table(pop_age$Institutional_distrust >= 8)

# 987 is really significant so we can proceed

# DBNSM Model -----

# DBNSM addictive

DBNSM_df <- pop_age_clean%>%
  filter(Institutional_distrust <= 8)

DBNSM <- glm(populist_vote
             ~ Institutional_distrust + Stealth_democracy,
             family = binomial(link = "logit"),
             data = DBNSM_df)

stargazer(DBNSM, type = "text")

plot_model(DBNSM,
           type = "pred",
           terms = c("Institutional_distrust", "Stealth_democracy[0,5,10]"),
           show.legend = TRUE) + 
  theme_minimal()

# DBNSM Interaction 

DBNSM_distrust_int <- glm(populist_vote
                        ~ Stealth_democracy + Institutional_distrust
                        * eta_classe,
                        family = binomial(link = "logit"),
                        data = DBNSM_df)

stargazer(DBNSM_distrust_int, type = "text")

sjPlot::plot_model(DBNSM_distrust_int, 
                   type = "int",
                   ci.lvl = NA,
                   show.legend = TRUE
) +
  theme_bw()

table(pop_age_clean$eta_classe)     


# Control by FDI
# Control by macro-regions / instruction 
# Control by M5S/League

#Macro regions controlling ----

# North - South - Red zone (included: Toscana, Emilia-Romagna, Umbria, Marche)
          
Regions_df <- ITANES_2022_Post_electoral%>%
  mutate(Macro_region = case_when(
    regione < 7 ~ "North",
    regione < 12 ~ "Red_area",
    TRUE ~ "South"))%>%
  select(seriale, Macro_region)

Regions_df <- left_join(pop_age_clean, Regions_df, by = join_by(seriale))%>%
  mutate(Macro_region = factor(Macro_region))

#Pure addittive model

Regions_model_add <- glm(populist_vote
                    ~ Stealth_democracy + Macro_region + Institutional_distrust
                    + eta_classe,
                    family = binomial(link = "logit"),
                    data = Regions_df)

stargazer(Regions_model_add, type = "text")

plot_model(Regions_model_add, 
           type = "pred", 
           terms = c("Institutional_distrust", "eta_classe"),
) +
  theme_minimal()
  
             



