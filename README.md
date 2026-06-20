## Advanced multivariate analysis final 

This is a repository for the Advanced muultivariate analyisis final project of Giovanni Bacchiega
The title of the paper is "Populist parties: still a home for the politically distrustful, or a faded and institutionalized phenomenon?"
The aim is to study how institutional distrust translated to populist vote and abstention in 2022 political elections, with a specific attention to how that worked for each age cohort. 

The repository is structured as follows:

  - Data: contain the data employed for the analysis in .DTA format; the dataset used is the           post_electoral survey 2022 by ITANES
  - Documentation: a brief pre-registration like document with the initial aim of the study and a     declaration of the use of AI during the study
  - Paper: the final paper in HTML and PDF versions
  - Quarto.PxA: the two quarto documents employed for the final stesure of the paper (the final       product is Populist_parties_2, which was created to reorder the file; also all the atachments of the quarto documents are included. NB Some pieces of codes and chunks might differ from the scripts as some changings have been introduced in the last version
  - Scripts: The whole script used:
      => Coef function: used to create a function that extracted coefficients from the logistic          regression
      => Index creation: used to create the distrust index employed
      => pop-age: the first "full" script; used also stealth democracy as predictor and kept age classes 1 & 2 separated
      => No_stealth: removed stealth democracy as predictor
      => Merged age classes 1 & 2: merged age classes
      => Parallel_multivariate: was an attempt of multivariate before switching to an interaction   model 
    
