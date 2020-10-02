# Nuuk Fjord shrub abundance drivers

# Literature review database cleaning ----

# Jonathan von Oppen, jonathan.vonoppen [at] bio.au.dk

# 27 September 2020

# ________________________________________ ----

# Dependencies ----
if (!require('pacman')) install.packages('pacman', repos="https://cloud.r-project.org")
pacman::p_load(tidyverse,
               tidylog,
               janitor)

# Load raw database ----
lit_raw <- read.csv(file.path("data", "input_data", "shrub_drivers_lit_review_records.csv"),
                    sep = ",",
                    header = TRUE)

# Clean database ----
lit <- lit_raw %>% 
  
  rename_all(tolower) %>% 
  
  # drop unnecessary columns
  select(-c(starts_with("book"),
            starts_with("conference"),
            starts_with("funding"),
            starts_with("journal"),
            starts_with("publisher"),
            contains("keywords"),
            contains("addresses"),
            contains("cited"),
            contains("count"),
            contains("abbreviation"),
            contains("number"),
            contains(".id"),
            contains("access"),
            ends_with("status"),
            ends_with("page"),
            "author.full.names",
            "group.authors",
            "language",
            "document.type",
            "orcids",
            "issn",
            "eissn",
            "isbn",
            "publication.date",
            "volume",
            "issue",
            "supplement",
            "special.issue",
            "meeting.abstract",
            "wos.categories",
            "research.areas")) %>% 
  
  # replace dots and spaces in variable names with underscores
  janitor::clean_names() %>% 
  
  # calculate and add number of drivers
  left_join(lit_raw %>% 
              select(study_id, drivers) %>% 
              filter(!(drivers == "")) %>%
              mutate(n_drivers = lengths(str_split(drivers, ";"))),
            by = c("study_id", "drivers")) %>% 
  
  # filter for species-level, empirical studies, study location above 60°N
  filter(study_level == "species",
         str_detect(study_type, "empirical"),
         lat > 60 | 
           location == "circumpolar" | 
           location == "Northern Fennoscandia" | 
           location == "Abisko, SE; Svalbard, NO; Zackenberg, GL; Fountainemore, IT; Alaska, US") %>% 
  
  # study ID #169 driver_regime is missing -> drivers temp & soil N -> insert "abiotic"
  mutate(driver_regime = case_when(study_id == 169 ~ "abiotic",
                                   TRUE ~ as.character(driver_regime)),
         driver_regime = factor(driver_regime)) %>% 
  
  # recode driver regime "biotic; abiotic" level
  mutate(driver_regime = fct_recode(driver_regime,
                                    "abiotic & biotic" = "biotic; abiotic"),
         n_species = fct_relevel(n_species,
                                 as.character(c(1:25)),
                                 "unknown")) %>% 
  
  # reorder columns
  select(study_id,
         source,
         publication_type,
         authors,
         publication_year,
         source_title,
         article_title,
         abstract,
         doi,
         date_of_export,
         study_type:drivers,
         n_drivers,
         driver_regime:comment)

# Save data ----

# write.csv(lit,
#           file = file.path("data", "processed", "nuuk_shrub_drivers_lit_selection.csv"),
#           row.names = FALSE)

# ________________________________________ ----
# end of script