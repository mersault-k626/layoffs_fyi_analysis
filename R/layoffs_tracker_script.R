
# load library ----------------------------------------

library(readr)
library(tidyverse)
library(lubridate)
library(countrycode)
library(lsr)
library(DBI)
library(RPostgres)
library(broom)
library(ggfortify)


# load raw data ---------------------------------------


layoffs_OG <- read_csv("data/Layoffs.fyi  - Tech Layoffs Tracker.csv")

glimpse(layoffs_OG)


# cleaning --------------------------------------------

bad_rows <- layoffs_OG %>% 
  filter(if_any(c(company_name, HQ_location, industry, country), is.na)) %>%
  select(company_name, HQ_location, industry, country)

View(bad_rows)

industries <- layoffs_OG %>%  
  distinct(industry)

# address NOT NULL columns

layoffs_OG <- layoffs_OG %>%       
  mutate(
    industry = case_when(
      company_name == "Eyeo" ~ "Marketing",         
      company_name == "Appsmith" ~ "Product",  
      TRUE ~ industry
    ),
    HQ_location = case_when(
      company_name == "Product Hunt" ~ "SF Bay Area",
      TRUE ~ HQ_location
    ),
    country = case_when(
      company_name == "Fit Analytics" ~ "Germany",
      company_name == "Ludia" ~ "Canada",
      TRUE ~ country
    )
  )

# double check bad rows

bad_rows <- layoffs_OG %>% 
  filter(if_any(c(company_name, HQ_location, industry, country), is.na)) %>%
  select(company_name, HQ_location, industry, country)

# remove "Non-U.S." and any white spaces

layoffs_OG <- layoffs_OG %>%
  mutate(across(where(is.character), ~ trimws(gsub(",Non-U.S.", "", .x))))

# adding continent and region
      # check if there's any countrycode non-compliant
      layoffs_OG %>%
        filter(is.na(countrycode(country, "country.name", "continent"))) %>%
        distinct(country)
      
      
continents <- read_csv("data/country-continent-codes.csv")
      

      

layoffs_expanded <- layoffs_OG %>%
  mutate(country_code = countrycode(country, origin = "country.name", destination = "iso3c")) %>%
  left_join(continents, by = c("country_code" = "iso3"), relationship = "many-to-many") %>%
  select(-(code:number)) %>%
    mutate(region = countrycode(country_code, origin = "iso3c", destination = "un.regionsub.name")) %>%
  rename(country=country.x)

# clean mn_raised, remove currency

layoffs_cleaned <- layoffs_expanded %>%  
  mutate(
    mn_raised_dollar = mn_raised_dollar %>%
      gsub("[$,]", "", .) %>% 
      na_if("N/A") %>%
      na_if("Unknown") %>%
      as.numeric()
    )

# remove date_added, limits data to 1H 2025

layoffs_light <- layoffs_cleaned %>%
  select(-date_added) %>% 
  mutate(date_announced = as.Date(date_announced, format = "%m/%d/%Y")) %>%
  filter(date_announced < "2025-07-01")


# Rename HQ_location to office_location

layoffs_light <- layoffs_light %>%
  rename(office = HQ_location) %>%
  mutate(
    pct_laid_off = as.numeric(gsub("%", "", pct_laid_off)) / 100
  )

# address a couple more na :/

layoffs_light <- layoffs_light %>%
  mutate(
    office = case_when(
      company_name == "BitMEX" ~ "Hong Kong",
      company_name == "WeDoctor" ~ "Zhejiang",
      TRUE ~ office
    )
  )

str(layoffs_light)
glimpse(layoffs_light)

# clean data type from dbl to numeric; removes na in stage cols

layoffs_light <- layoffs_light %>%
  mutate(pct_laid_off = as.numeric(round(pct_laid_off, 3)),
         num_laid_off = as.integer(num_laid_off),
         mn_raised_dollar = as.numeric(mn_raised_dollar))

layoffs_light <-layoffs_light %>%
  mutate(stage = if_else(is.na(stage), "Unknown", stage))

layoffs_light %>%
  distinct(stage)

layoffs_light <- layoffs_light %>%
  select(date_announced, company_name, num_laid_off, pct_laid_off, 
         industry, stage, country, region, continent, 
         mn_raised_dollar, office, country_code, source)

View(layoffs_light %>%
  filter(office %in% c(
    "New Delhi,New York City", "Melbourne,Victoria")))

layoffs_light <- layoffs_light %>%
  mutate(office = case_when(
    office == "New Delhi,New York City" ~ "New York",
    office == "Melbourne,Victoria" ~ "Melbourne",
    TRUE ~ office
  ))

# SQL export ------------------------------------------


# one-time environ 
file.edit("~/.Renviron")

# dbConnect to establish link with postgreSQL

con <- dbConnect(
  RPostgres::Postgres(),
  dbname   = Sys.getenv("PGDATABASE"),
  host     = Sys.getenv("PGHOST"),
  port     = Sys.getenv("PGPORT"),
  user     = Sys.getenv("PGUSER"),
  password = Sys.getenv("PGPASSWORD")
)

# export

dbWriteTable(
  conn = con,
  name = "layoffs",               # table name in PostgreSQL
  value = layoffs_light,          # your R data frame
  overwrite = TRUE              # replace if table already exists
)

# continued in SQL...


# Imported from SQL -----------------------------------

layoffs_cleaned_ready <- dbReadTable(con, "layoffs_export_view")

glimpse(layoffs_cleaned_ready)


View(layoffs_cleaned_ready)

write_csv(layoffs_cleaned_ready, "layoffs_cleaned_ready.csv")

layoffs_cleaned_ready <- read_csv("layoffs_cleaned_ready.csv")


# Analysis --------------------------------------------

View(
  layoffs_cleaned_ready %>%
    distinct(stage)
)


layoffs_cleaned_ready %>%
  filter(!stage == "Unknown") %>%
  group_by(stage) %>%
  summarise(
    total_retrenched = sum(num_laid_off, na.rm = TRUE),
    avg_pct_fired = mean(pct_laid_off, na.rm = TRUE),
    median_pct_fired = median(pct_laid_off, na.rm = TRUE)
  ) %>%
  ggplot(aes(x = reorder(stage, -avg_pct_fired))) +
  geom_col(aes(y = avg_pct_fired, fill = "Average"), alpha = 0.7) +
  geom_line(aes(y = median_pct_fired, color = "Median", group = 1), 
            size = 1.5) +
  geom_point(aes(y = median_pct_fired, color = "Median"), size = 2) +
  scale_fill_manual(values = c("Average" = "steelblue")) +
  scale_color_manual(values = c("Median" = "red")) +
  guides(fill = guide_legend(override.aes = list(linetype = 0)),
         color = guide_legend(override.aes = list(fill = NA, shape = 15, linetype = 1))) +
  labs(
    title = "Average vs Median Percentage Laid Off by Funding Stage",
    subtitle = "Bars show average, line shows median",
    y = "Percentage Laid Off (%)",
    x = "Funding Stage",
    fill = "Metric",
    color = "Metric"
  ) +
  ggthemes::theme_clean() +
  theme(legend.position = "top")


layoffs_cleaned_ready %>%
  filter(!stage == "Unknown") %>%
  group_by(stage) %>%
  summarise(
    total_retrenched = sum(num_laid_off, na.rm = TRUE),
    avg_pct_fired = mean(pct_laid_off, na.rm = TRUE),
    median_pct_fired = median(pct_laid_off, na.rm = TRUE)
  ) %>%
  ggplot(aes(x = reorder(stage, -avg_pct_fired))) +
  geom_col(aes(y = avg_pct_fired, fill = "Average"), alpha = 0.7) +
  geom_line(aes(y = median_pct_fired, fill = "Median", group = 1), 
            size = 1.5, color = "red") +
  geom_point(aes(y = median_pct_fired, fill = "Median"), 
             size = 2, color = "red") +
  scale_fill_manual(values = c("Average" = "steelblue", "Median" = "red")) +
  scale_y_continuous(
    limits = c(0, 1.05),      
    expand  = c(0, 0)       
  ) +
  labs(
    title = "Average vs Median Percentage Laid Off by Funding Stage",
    subtitle = "Bars show average, line shows median",
    y = "Percentage Laid Off (%)",
    x = "Funding Stage",
    fill = "Metric"
  ) +
  ggthemes::theme_clean() +
  theme(legend.position = "top")
  
  
  

# regression model ------------------------------------

 lm.tibble <- layoffs_cleaned_ready %>%
  select(industry, num_laid_off) %>%
  filter(!is.na(num_laid_off)) %>%
  mutate(industry = as.factor(industry))

str(lm.tibble$industry)

layoffs.lmmodel <- lm(num_laid_off ~ industry, data = lm.tibble)

glance(layoffs.lmmodel)


autoplot(layoffs.lmmodel,
         which = 1:4,
         nrow = 2,
         ncol = 2)



# Model Performance: This linear regression explains only 6.1% of the variation in layoff numbers (R² = 0.0611), indicating that industry alone is a poor predictor of layoff magnitude.
# Statistical Significance: The model is statistically significant (p < 0.001), but this is largely due to the large sample size (n = 2,673) rather than meaningful practical relationships.
# Industry Effects (compared to Aerospace baseline of ~231 layoffs):
#   
#   Hardware shows the largest positive effect (+1,619 additional layoffs)
# Infrastructure (+297) and Consumer (+241) also show substantial increases
# Product (-170), AI (-196), and Legal (-150) show the largest reductions
# 
# Key Limitations:
#   
#   Low explanatory power: 93.9% of variation remains unexplained
# High residual error: Standard error of 935 layoffs suggests large prediction uncertainty
# Missing variables: Company size, financial health, market conditions likely matter more than industry
# 
# Practical Takeaway: While some industries (like Hardware) tend to have larger layoffs on average, industry classification alone provides limited insight into predicting layoff size. The model suggests other factors are far more important in determining layoff magnitude.
# This is essentially a one-way ANOVA in regression form, showing industry means differ but don't explain much overall variation.


# log standardisation ---------------------------------

# imputation with `mice`

layoffs_for_impute <- layoffs_cleaned_ready %>%
  select(pct_laid_off, stage, industry, region, year, quarter, mn_raised_dollar)

imputed.layoffs <- layoffs_cleaned_ready

imputed <- mice::mice(layoffs_for_impute, m = 5, method = "pmm", seed = 42)

imputed.layoffs$pct_laid_off <- complete(imputed, 1)$pct_laid_off

  # check for remaining na

sum(is.na(imputed.layoffs$pct_laid_off))

# “To address missing pct_laid_off values, I used multivariate imputation via the mice package in R, based on company stage, region, industry, and capital raised. I chose predictive mean matching to preserve the shape and limits of the original variable.”

# introduce log

imputed.layoffs <- imputed.layoffs %>%
  mutate(log_num_laid_off = log1p(log1p(num_laid_off)))

# analyse before/after

ggplot(imputed.layoffs, aes(x = num_laid_off)) +
  geom_histogram(bins = 50) +
  labs(title = "Raw Layoff Counts (Highly Skewed)")

# Log-transformed
ggplot(imputed.layoffs, aes(x = log_num_laid_off)) +
  geom_histogram(bins = 50) +
  labs(title = "Log-Transformed Layoff Counts")


# “To deal with extreme layoff counts concentrated in North America, I applied a log transformation to num_laid_off using log1p(). This reduced skew and allowed me to compare regions more fairly. I then reported both log-scaled averages and their back-transformed interpretations.”

glimpse(imputed.layoffs)




  region_summary <- imputed.layoffs %>%
  group_by(region) %>%
  summarise(
    total_laid_off    = sum(log_num_laid_off, na.rm = TRUE),
    avg_pct_laid_off  = mean(pct_laid_off, na.rm = TRUE)
  )

# Find a rescaling multiplier
scale_factor <- max(region_summary$total_laid_off) / max(region_summary$avg_pct_laid_off)

region_summary <- region_summary %>%
  mutate(scaled_pct = avg_pct_laid_off * scale_factor)
library(ggplot2)

ggplot(region_summary, aes(x = region)) +
  geom_col(aes(y = total_laid_off), fill = "#2c3e50") +
  geom_line(aes(y = scaled_pct, group = 1), colour = "#e74c3c", size = 1.2) +
  geom_point(aes(y = scaled_pct), colour = "#e74c3c", size = 3) +
  scale_y_continuous(
    name = "Total Log Layoffs",
    sec.axis = sec_axis(~ . / scale_factor, name = "Average % Laid Off")
  ) +
  labs(title = "Layoffs by Region (Dual Axis)",
       x = "Region") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# hypothesis testing ----------------------------------

imputed.layoffs %>%
  group_by(region) %>%
  summarise(average = mean(pct_laid_off))
# ➤ Quick check to see average % laid off by region
# ➤ Not hypothesis testing yet, but helps see whether some regions got hit harder

any(duplicated(imputed.layoffs$company))
# ➤ Checks if you’ve got duplicated companies
# ➤ TRUE = you got duplicates, might screw with group-level stats
# ➤ FALSE = good to go

aov_model <- aov(pct_laid_off ~ stage, data = imputed.layoffs)
# ➤ Standard one-way ANOVA: compares mean % laid off between different funding stages
# ➤ But don’t trust it blindly — only valid if normality and equal variances hold (which is sus here)

TukeyHSD(aov_model)
# ➤ Post-hoc pairwise comparison (if ANOVA is legit, which in this case... maybe not)
# ➤ You’re running it anyway to get a feel for which stages differ most

ggplot(imputed.layoffs, aes(x = stage, y = pct_laid_off)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Layoff Percentage by Funding Stage",
       y = "% Staff Laid Off", x = "Funding Stage")
# ➤ Visualise group spreads and medians
# ➤ This plot basically screams: "yo, ANOVA assumptions are broken"
#     - variance not equal (some box heights way taller)
#     - shape skewed (look at seed & unknown)

layoffs.kruskal <- kruskal.test(pct_laid_off ~ stage, data = imputed.layoffs)
# ➤ Kruskal-Wallis = non-parametric ANOVA
# ➤ Doesn’t care about normality or equal variances
# ➤ If p < 0.05 → at least one group differs significantly in layoff %

# “Given that variance and distribution shape differed across groups, I validated the ANOVA with a Kruskal-Wallis test. The result confirmed a significant difference in layoff severity across stages (χ² = 335.5, p < 2.2e-16), even without assuming normality or homogeneity of variance.”
# ➤ Copy-paste above if you want a tight justification in your STAR deck

dunn.layoffs <- FSA::dunnTest(pct_laid_off ~ stage, data = imputed.layoffs, method = "bh")
# ➤ Pairwise test after Kruskal
# ➤ Benjamini-Hochberg (method = "bh") controls false discovery rate
# ➤ This is the safer, non-parametric version of Tukey

dunn_results <- as_tibble(dunn.layoffs$res)
# ➤ Format the result nicely for filtering/sorting
# ➤ You’ll need this tibble to interpret the big Z-scores

dunn_results %>%
  filter(P.adj < 0.05) %>%
  arrange(desc(Z))
# ➤ Show only statistically significant comparisons (after BH correction)
# ➤ Sorted by Z-score — bigger Z = bigger difference between stages
# ➤ Use this to pinpoint which funding stages got wrecked the worst




# distribution ----------------------------------------

num_total_null <- layoffs_cleaned_ready %>%
  filter(is.na(num_laid_off)) %>%
  count()

num_total_null

num_total_comp <- layoffs_cleaned_ready %>%
  filter(!is.na(num_laid_off)) %>%
  count()
num_total_comp

pct_total_null <- layoffs_cleaned_ready %>%
  filter(is.na(pct_laid_off)) %>%
  count()

pct_total_null

pct_total_comp <- layoffs_cleaned_ready %>%
  filter(!is.na(pct_laid_off)) %>%
  count()
pct_total_comp


# imputation ------------------------------------------

# Subset the original dataset to only include variables relevant for imputation.
layoffs_for_impute <- layoffs_cleaned_ready %>%
  select(num_laid_off, pct_laid_off,
         industry, stage, country, year, quarter)

# Load the mice package for multivariate imputation.
library(mice)

# Run multiple imputation with Predictive Mean Matching (PMM) for two numeric columns.
# Leave other variables untouched. Generate 5 imputed datasets with 10 iterations.
mice_imputed <- mice(
  layoffs_for_impute,
  m = 5,
  method = c("pmm", "pmm", "", "", "", "", ""),  # Only imputing num_laid_off and pct_laid_off
  maxit = 10,
  seed = 123
)

# Plot density of imputed num_laid_off across the 5 imputed datasets.
densityplot(mice_imputed, ~num_laid_off)

# Extract the first completed imputed dataset.
completed_data <- complete(mice_imputed, 1)

# Create a working copy of the original dataset to insert imputed values.
layoffs_cleaned_ready_2 <- layoffs_cleaned_ready

# Load dplyr for data manipulation.
library(dplyr)

# Insert imputed values only where original values are NA.
# coalesce() retains original data if not missing.
layoffs_cleaned_ready_2 <- layoffs_cleaned_ready_2 %>%
  mutate(
    num_laid_off  = coalesce(num_laid_off,  completed_data$num_laid_off),
    pct_laid_off  = coalesce(pct_laid_off,  completed_data$pct_laid_off)
  )

# Check for any rows where *all* values are NA (should be none after imputation).
layoffs_cleaned_ready_2 %>%
  filter(if_all(everything(), is.na))


# Compute mean values of the two columns post-imputation to assess impact.
layoffs_cleaned_ready_2 %>%
  summarise(mean(num_laid_off), round(mean(pct_laid_off), 2))
# Compare with the OG
  
  layoffs_cleaned_ready %>%
    summarise(mean(num_laid_off, na.rm = TRUE), round(mean(pct_laid_off, na.rm = TRUE), 2))
  

  # Create combined dataset for num_laid_off
  num_laid_off_compare <- bind_rows(
    layoffs_cleaned_ready %>%
      select(num_laid_off) %>%
      mutate(source = "Original"),
    
    layoffs_cleaned_ready_2 %>%
      select(num_laid_off) %>%
      mutate(source = "Imputed")
  )
  
  # Same for pct_laid_off
  pct_laid_off_compare <- bind_rows(
    layoffs_cleaned_ready %>%
      select(pct_laid_off) %>%
      mutate(source = "Original"),
    
    layoffs_cleaned_ready_2 %>%
      select(pct_laid_off) %>%
      mutate(source = "Imputed")
  )
  
  ggplot(num_laid_off_compare, aes(x = num_laid_off, colour = source)) +
    geom_density(linewidth = 1) +
    theme_minimal() +
    labs(
      title = "Density Comparison: num_laid_off",
      x = "Number Laid Off",
      y = "Density",
      colour = "Dataset"
    ) +
    scale_x_continuous(labels = scales::comma)
  

  ggplot(pct_laid_off_compare, aes(x = pct_laid_off, colour = source)) +
    geom_density(linewidth = 1) +
    theme_minimal() +
    labs(
      title = "Density Comparison: pct_laid_off",
      x = "Percentage Laid Off",
      y = "Density",
      colour = "Dataset"
    ) +
    scale_x_continuous(labels = scales::percent_format(scale = 1))
  
  # hypothesis testing ----------------------------------
  
  # Kruskal-Wallis test to see the relations between time-cycle and layoffs total
  
  layoffs_cleaned_ready_3 <- layoffs_cleaned_ready_2 %>%
    mutate(year_quarter = paste0(year, "-Q", quarter))
  
  kruskal.test(num_laid_off ~ year_quarter, data = layoffs_cleaned_ready_3)
  
  
  FSA::dunnTest(num_laid_off ~ year_quarter, data = layoffs_cleaned_ready_3, method = "bh")
  
  
  layoffs_cleaned_ready_3 %>%
    group_by(year_quarter) %>%
    summarise(median_layoffs = median(num_laid_off, na.rm = TRUE)) %>%
    ggplot(aes(x = year_quarter, y = median_layoffs)) +
    geom_col(fill = "steelblue") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(
      title = "Median Layoffs by YYYY-QQ",
      x = "Year–Quarter",
      y = "Median Number Laid Off"
    )
  
  
  #  Chi-square test of independence
  
  layoffs_chi.sq <- layoffs_cleaned_ready_2 %>%
    filter(!is.na(industry), !is.na(stage)) %>%
    mutate(industry = as.factor(industry),
           stage = as.factor(stage))
  
  lsr::associationTest(~ industry + stage, data = layoffs_chi.sq)
  
  
  


