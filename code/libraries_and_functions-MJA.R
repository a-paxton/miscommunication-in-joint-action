#### libraries_and_functions-MJA.r: Part of `miscommunication_in_joint_action.Rmd` ####
#
# This script loads libraries and creates a number of additional functions.
#
# Written by: A. Paxton (University of California, Berkeley)
# Date last modified: 14 April 2017
#####################################################################################

#### Load necessary libraries ####
# library(crqa)
library(lme4)
library(plyr)
library(ggplot2)
library(reshape2)
library(pander)
library(dplyr)
library(purrr)
library(pander)
library(gridExtra)
library(plotrix)
library(gtable)
library(MuMIn)
library(viridis)
library(tidyr)
library(Rmisc)
library(grid)

#### Identify groups of variables from data file ####

# string or character variables that aren't factors
character_vars = c('Transcript',
                   'cleaned_transcript',
                   'unique_words',
                   'spatial_words',
                   'assent_words',
                   'negation_words',
                   'question_words',
                   'IncompletedWords_Completed')

# variables we won't need throughout these analyses
unused_vars = c('BinnedTurn',
                'IncompletedWords_Completed',
                'Utterances',
                'gCong',
                'VisCon_Global',
                'VisConLocal')

#### Create functions we'll need ####

# "derive_p": derive p-value
derive_p = function(x) {return((1-pnorm(x))*2)}

##

# "equal_lengths": trim time series to be of equal lengths
equal_lengths = function(ts1,ts2){
  
  # see whether it's a 1D object
  if (is.null(dim(ts1))){
    
    # if the two time series aren't of equal lengths...
    if (length(ts1) != length(ts2)){
      
      # ... find the shortest length ...
      minlength = min(length(ts1),length(ts2))
      
      # ... and force them both to be that long ...
      ts1 = ts1[1:minlength]
      ts2 = ts2[1:minlength]
    }
    
  } else {
    
    # if the two time series aren't of equal lengths...
    if (dim(ts1)[1] != dim(ts2)[1]){
      
      # ... find the shortest length ...
      minlength = min(dim(ts1)[1],dim(ts2)[1])
      
      # ... and force them both to be that long ...
      ts1 = ts1[1:minlength,]
      ts2 = ts2[1:minlength,]
    }
  }
  
  # spit out the time series if/once they're the same length
  return(list(ts1,ts2))
}

##

# "add_var_prefix": add prefix to all variable names except "dyad" and "lag"
add_var_prefix = function(df,var_prefix){
  
  # load library for function
  library(magrittr)
  
  # specify which we're skipping
  skip_vars = c('dyad','Pair','lag')
  skip_cols = df[names(df) %in% skip_vars]
  
  # grab all of the other columns and add the relevant prefix
  renaming_cols = df[! names(df) %in% skip_vars]
  renaming_cols = renaming_cols %>%
    setNames(paste0(var_prefix,".",names(.)))
  
  # combine the renamed and unchanged frames and return it
  new.df = cbind.data.frame(skip_cols,renaming_cols)
  return(new.df)
}

## 

# "pander_lme": simplify lme4 printouts (available on GitHub: https://github.com/a-paxton/stats-tools)
pander_lme = function(lme_model_name, stats.caption){
  
  # load in pander
  library(pander)
  
  # # disable scientific notation
  # options(scipen = 999)
  # 
  # convert the model summary to a dataframe
  neat_output = data.frame(summary(lme_model_name)$coefficient)
  
  # round p-values to 2 decimal places except if it's quite small
  neat_output$p = 2*(1-pnorm(abs(neat_output$t.value)))
  neat_output$p[neat_output$p >= .005] = round(neat_output$p[neat_output$p >= .005],2)
  neat_output$p[neat_output$p >= .0005] = round(neat_output$p[neat_output$p >= .0005],3)
  neat_output$p[neat_output$p < .0005] = round(neat_output$p[neat_output$p < .0005],4)
  
  # create significance and trending markers
  neat_output$Sig = ' '
  neat_output$Sig[neat_output$p < .1] = '.'
  neat_output$Sig[neat_output$p < .05] = '*'
  neat_output$Sig[neat_output$p < .01] = '**'
  neat_output$Sig[neat_output$p < .001] = '***'
  
  # set a caption that includes R-squared values
  if (stats.caption == TRUE){
    
    # use MuMIN to calculate R-squared
    library(MuMIn)
    model_marginal_r_squared = r.squaredGLMM(lme_model_name)[['R2m']]
    model_conditional_r_squared = r.squaredGLMM(lme_model_name)[['R2c']]
    neat_caption = paste('**Marginal *R*-squared: ',
                         round(model_marginal_r_squared,2), 
                         ". Conditional *R*-squared: ",
                         round(model_conditional_r_squared,2),".**",sep="")
    
    # return the table
    return(pander(neat_output, split.table = Inf, caption = neat_caption, style = 'rmarkdown'))
  } else { # or return a table without it
    return(pander(neat_output, split.table = Inf, style = 'rmarkdown'))
  }
}

## 

# "pander_glmer": simplify glmer printouts
pander_glmer = function(glmer_model_name){
  
  # load in pander
  library(pander)
  library(dplyr)
  
  # # disable scientific notation
  # options(scipen = 999)
  # 
  # convert the model summary to a dataframe
  neat_output = data.frame(summary(glmer_model_name)$coefficient)
  
  # round p-values to 2 decimal places except if it's quite small
  neat_output$p = neat_output$Pr...z..
  neat_output$p[neat_output$p >= .005] = round(neat_output$p[neat_output$p >= .005],2)
  neat_output$p[neat_output$p >= .0005] = round(neat_output$p[neat_output$p >= .0005],3)
  neat_output$p[neat_output$p < .0005] = round(neat_output$p[neat_output$p < .0005],4)
  
  # create significance and trending markers
  neat_output$Sig = ' '
  neat_output$Sig[neat_output$p < .1] = '.'
  neat_output$Sig[neat_output$p < .05] = '*'
  neat_output$Sig[neat_output$p < .01] = '**'
  neat_output$Sig[neat_output$p < .001] = '***'
  
  # drop or rename columns
  neat_output = neat_output %>% 
    rename(Std.Error = Std..Error) %>% 
    dplyr::select(-Pr...z..)
  
  # return the table
  return(pander(neat_output, split.table = Inf, style = 'rmarkdown'))
}


##

# "pander_anova": simplify anova printouts and include adjusted R-squared and F-stats
pander_anova = function(anova_model_name){
  
  # load in pander
  require(pander)
  require(plyr)
  
  # disable scientific notation
  options(scipen = 999)
  
  # convert the model summary to a dataframe and rename variables
  neat_output = data.frame(anova_model_name)
  
  # round p-values (using Psychological Science's recommendations)
  neat_output$p = neat_output$Pr..Chisq.
  neat_output$p[is.na(neat_output$p)] = 0
  neat_output$p[neat_output$p < .0005] = round(neat_output$p[neat_output$p < .0005],4)
  neat_output$p[neat_output$p >= .0005] = round(neat_output$p[neat_output$p >= .0005],3)
  neat_output$p[neat_output$p >= .25] = round(neat_output$p[neat_output$p >= .25],2)
  
  # create significance and trending markers
  neat_output$sig = ' '
  neat_output$sig[neat_output$p < .15] = '.'
  neat_output$sig[neat_output$p < .05] = '*'
  neat_output$sig[neat_output$p < .01] = '**'
  neat_output$sig[neat_output$p < .001] = '***'
  
  # re-create blank spaces from original anova output
  neat_output$p[is.na(neat_output$Pr..Chisq.)] = ' '
  neat_output$sig[is.na(neat_output$Pr..Chisq.)] = ' '
  neat_output = replace(neat_output,is.na(neat_output),' ')
  
  # rename variables
  neat_output = plyr::rename(neat_output, replace = c('Df' = 'DF',
                                                      'logLik' = 'Log Likelihood',
                                                      'Chisq' = "Chi Sq.",
                                                      'Chi.Df' = "Chi Sq. DF"))
  neat_output = subset(neat_output, select = -c(Pr..Chisq.))
  
  # return the neatened table
  return(pander(neat_output, style="rmarkdown",split.table = Inf))
}
