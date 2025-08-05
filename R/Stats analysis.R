layoffs_cleaned_ready <- read_csv("data/layoffs_cleaned_ready.csv")

layoffs_for_impute <- layoffs_cleaned_ready %>%
  select(num_laid_off, pct_laid_off,
         industry, stage, country, office, year, quarter)

mice_imputed <- mice::mice(
  layoffs_for_impute,
  m = 5,
  method = c("pmm", "pmm", "", "", "", "", "", ""),  # Only imputing num_laid_off and pct_laid_off
  maxit = 10,
  seed = 666
)

# Extract the first completed imputed dataset.
completed_data <- complete(mice_imputed, 1)

# Create a working copy of the original dataset to insert imputed values.
layoffs_imputed <- layoffs_cleaned_ready


layoffs_imputed <- layoffs_imputed %>%
  mutate(num_laid_off = coalesce(num_laid_off, completed_data$num_laid_off),
         pct_laid_off = coalesce(pct_laid_off, completed_data$pct_laid_off)
  )

# is.na check

layoffs_imputed %>%
  filter(if_all(everything(), is.na))


# descriptive analysis --------------------------------

 # mean, median by country, region
  
  layoffs_min.mid_region <- layoffs_imputed %>%
    group_by(region) %>%
    summarise(
      mean_laid_off = mean(num_laid_off),
      med_pct_workforce = median(pct_laid_off)
    )

layoffs_min.mid_region
  
  ggplot(layoffs_min.mid_region, aes(mean_laid_off)) +
    geom_density(colour = "blue") +
    ggtitle("Frequency density distribution for mean of total employees laid-off") +
    theme_minimal()
  
  ggplot(layoffs_min.mid_region, aes(med_pct_workforce)) +
    geom_density(colour = "green") +
    ggtitle("Frequency density distribution for median of total employees laid-off") +
    theme_minimal()
  
  

# library(zoo) for YYQQ analysis ----------------------

library(zoo)  
  
  
  





















