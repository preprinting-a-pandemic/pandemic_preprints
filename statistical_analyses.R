# Descriptive statistics function, returning median and IQR, mean and sd, and Anderson-Darling test for normality
descriptive_stats <- function(d, colnamestring) {
  d %>%
    filter(posted_date >= "2020-01-01") %>%
    group_by(covid_preprint) %>%
    summarise_at(.vars = c(colnamestring), .funs = funs(
      median = median(., na.rm = TRUE),
      IQR = IQR(., na.rm = TRUE),
      mean = mean(., na.rm = TRUE),
      sd = sd(., na.rm = TRUE),
      ad_statistic = nortest::ad.test(.)$statistic,
      ad_p = nortest::ad.test(.)$p.value
    ))
}



# Figure 2: Preprint attributes
# Panel B: Preprint screening time
# Descriptive statistics
preprints %>%
  filter(posted_date >= "2020-01-01") %>%
  mutate(
    doi_date = gsub("\\.", "-", substr(doi, 9, 18)),
    days = as.numeric(ymd(posted_date) - ymd(doi_date))
  ) %>%
  group_by(covid_preprint, source) %>%
  summarise_at(.vars = c("days"), .funs = funs(
    median = median(., na.rm = TRUE),
    IQR = IQR(., na.rm = TRUE),
    mean = mean(., na.rm = TRUE),
    sd = sd(., na.rm = TRUE),
    ad_statistic = nortest::ad.test(.)$statistic,
    ad_p = nortest::ad.test(.)$p.value
  ))

# Two-way ANOVA, screening time ~ preprint type, server
anova_screening <- preprints %>%
  filter(posted_date >= "2020-01-01") %>%
  mutate(
    doi_date = gsub("\\.", "-", substr(doi, 9, 18)),
    days = as.numeric(ymd(posted_date) - ymd(doi_date))
  ) %>%
  with(., aov(days ~ source * covid_preprint))

summary(anova_screening)

# Posthoc contrasts for specific preprint type/server combinations
preprints %>%
  filter(posted_date >= "2020-01-01") %>%
  assign_covid_preprint() %>%
  mutate(
    doi_date = gsub("\\.", "-", substr(doi, 9, 18)),
    days = as.numeric(ymd(posted_date) - ymd(doi_date)),
    int_source_covid_preprint = interaction(source, covid_preprint)
  ) %>%
  with(., lm(days ~ int_source_covid_preprint - 1)) %>%
  multcomp::glht(., linfct = multcomp::mcp(int_source_covid_preprint = "Tukey")) %>%
  summary()

# Panel C: Author counts
# Descriptive statistics
descriptive_stats(preprints, "n_authors")

# Panel D: Author attributes: country, nationality (top 10 countries)
# Examine first-time preprinters pooling all countries
preprinter_history %>%
  with(., table(author_group, preprinter_status)) %>%
  prop.table(1) * 100
preprinter_history %>%
  with(., table(author_group, preprinter_status)) %>%
  chisq.test()

# Panel E: License types
# Fisher's exact test of association
preprints %>%
  filter(posted_date >= "2020-01-01") %>%
  mutate(license = case_when(
    str_detect(license, "cc0") ~ "cc0",
    T ~ license
  )) %>%
  with(., fisher.test(license, covid_preprint, simulate.p.value = TRUE, B = 1000))

# Panel F: Revisions per preprint for COVID vs non-COVID
# Descriptive statistics
descriptive_stats(preprints, "n_versions")
# Mann-Whitney test
preprints %>%
  filter(posted_date >= "2020-01-01") %>%
  with(., wilcox.test(n_versions ~ covid_preprint))

# Panel G: Word counts
# Descriptive statistics
descriptive_stats(preprints %>% filter(source == "biorxiv"), "n_words")
# Mann-Whitney test
preprints %>%
  filter(posted_date >= "2020-01-01" & source == "biorxiv") %>%
  with(., wilcox.test(n_words ~ covid_preprint))

# Panel H: Reference counts
# Descriptive statistics
descriptive_stats(preprints %>% filter(source == "biorxiv"), "n_refs")
# Mann-Whitney, number of references
preprints %>%
  filter(posted_date >= "2020-01-01" & source == "biorxiv") %>%
  with(., wilcox.test(n_refs ~ covid_preprint))

# Panel I: Percentage of preprints published
# Chi-square test of association
preprints %>%
  filter(posted_date >= "2020-01-01") %>%
  with(., chisq.test(is_published, covid_preprint))

# Panel J: Publishing timeline
# Descriptive statistics (including control period Sep - Dec 2019)
preprints %>%
  mutate(covid_preprint = case_when(
    covid_preprint == T ~ "COVID-19 preprints",
    (covid_preprint == F & posted_date >= "2020-01-01") ~ "non-COVID-19 preprints",
    T ~ "Preprints Sep-Dec 2019")) %>%
  group_by(covid_preprint) %>%
  filter(delay_in_days > 0) %>% # Filter out erroneous preprints that were published before preprinted
  summarise_at(.vars = c("delay_in_days"), .funs = funs(
    median = median(., na.rm = TRUE),
    IQR = IQR(., na.rm = TRUE),
    mean = mean(., na.rm = TRUE),
    sd = sd(., na.rm = TRUE),
    ad_statistic = nortest::ad.test(.)$statistic,
    ad_p = nortest::ad.test(.)$p.value
  ))

# Mann-Whitney, time to publishing (excluding control period Sep - Dec 2019)
preprints %>%
  filter(posted_date >= "2020-01-01") %>%
  with(., wilcox.test(delay_in_days~covid_preprint))

# Figure 3: Preprint access
# Panel A: Abstract views
# Prepare data
d <- preprint_usage %>%
  inner_join(preprints, by = "doi") %>%
  group_by(doi, posted_date, covid_preprint) %>%
  summarize(
    full_text_views = sum(full_text_views),
    abstract_views = sum(abstract_views),
    pdf_downloads = sum(pdf_downloads)
  ) %>%
  ungroup() %>%
  assign_covid_preprint() %>%
  mutate(
    posted_week = floor_date(posted_date, unit = "week", week_start = 1),
    serial_date = (posted_date - as.Date("2020-01-01")) %>%
      as.numeric(units = "days")
  ) %>%
  filter(serial_date >= 0)

# Negative binomial regression, views ~ preprint type and posting date
views_nbmod <- d %>%
  with(., MASS::glm.nb(abstract_views ~ covid_preprint * serial_date))

# Poisson regression, views ~ preprint type and posting date
views_poismod <- d %>%
  with(., glm(abstract_views ~ covid_preprint + serial_date, family = "poisson"))

# Confirm negative binomial as better fitting model
AIC(views_nbmod, views_poismod)

# Model summary
views_nbmod %>% summary()
views_nbmod %>% 
  coef() %>% 
  exp()
1 - (views_nbmod %>% coef() %>% .[3:4] %>% exp() %>% .^7) # Calculate multiplicative rate of views for each subsequent week

# Panel B: PDF downloads
# Negative binomial regression, downloads ~ preprint type and posting date
dloads_nbmod <- d %>%
  with(., MASS::glm.nb(pdf_downloads ~ covid_preprint + serial_date))

# Poisson regression, downloads ~ preprint type and posting date
dloads_poismod <- d %>%
  with(., glm(pdf_downloads ~ covid_preprint + serial_date, family = "poisson"))

# Confirm negative binomial as better fitting model
AIC(dloads_nbmod, dloads_poismod)

# Model summary
dloads_nbmod %>% summary()
dloads_nbmod  %>% 
  coef() %>% 
  exp()
1 - (dloads_nbmod %>% coef() %>% .[3] %>% exp() %>% .^7) # Calculate multiplicative rate of downloads for each subsequent week

# Figure 4: Preprint usage and sharing
# Prepare data
d <- preprints %>%
  filter(posted_date >= "2020-01-01") %>%
  inner_join(preprint_citations, by = "doi") %>%
  inner_join(preprint_comments %>% rename(comments = comments_count), by = "doi") %>%
  inner_join(preprint_altmetrics, by = "doi") %>%
  assign_covid_preprint() %>%
  mutate(
    posted_week = floor_date(posted_date, unit = "week", week_start = 1),
    serial_date = (posted_date - as.Date("2020-01-01")) %>%
      as.numeric(units = "days")
  )

# Panel A: Citations per preprint (COVID vs non-COVID)
# Negative binomial regression, citations ~ preprint type and posting date
citations_nbmod <- d %>%
  with(., MASS::glm.nb(citations ~ covid_preprint + serial_date))

# Poisson regression, citations ~ preprint type and posting date
citations_poismod <- d %>%
  with(., glm(citations ~ covid_preprint + serial_date, family = "poisson"))

# Confirm negative binomial as better fitting model
AIC(citations_nbmod, citations_poismod)

# Model summary
citations_nbmod %>% summary()
citations_nbmod %>%
  coef() %>%
  exp()
1 - (citations_nbmod %>% coef() %>% .[3] %>% exp() %>% .^7) # Calculate multiplicative rate of citations for each subsequent week

# Panel B: Tweets per preprint (COVID vs non-COVID)
# Negative binomial regression, tweets ~ preprint type and posting date
tweets_nbmod <- d %>%
  with(., MASS::glm.nb(twitter ~ covid_preprint + serial_date))

# Poisson regression, tweets ~ preprint type and posting date
tweets_poismod <- d %>%
  with(., glm(twitter ~ covid_preprint + serial_date, family = "poisson"))

# Confirm negative binomial as better fitting model
AIC(tweets_nbmod, tweets_poismod)

# Model summary
tweets_nbmod %>% summary()
tweets_nbmod %>%
  coef() %>%
  exp()
1 - (tweets_nbmod %>% coef() %>% .[3] %>% exp() %>% .^7) # Calculate multiplicative rate of tweets for each subsequent week

# Panel C: News mentions per preprint (COVID vs non-COVID)
# Calculate table of any vs none
d %>%
  mutate(newsflag = ifelse(news == 0, "0", "1")) %>%
  with(., table(newsflag, covid_preprint)) %>%
  prop.table(margin = 2) * 100

# Negative binomial regression, news ~ preprint type and posting date
news_nbmod <- d %>%
  with(., MASS::glm.nb(news ~ covid_preprint + serial_date))

# Poisson regression, news ~ preprint type and posting date
news_poismod <- d %>%
  with(., glm(news ~ covid_preprint + serial_date, family = "poisson"))

# Confirm negative binomial as better fitting model
AIC(news_nbmod, news_poismod)

# Model summary
news_nbmod %>% summary()
news_nbmod %>%
  coef() %>%
  exp()
1 - (news_nbmod %>% coef() %>% .[3] %>% exp() %>% .^7) # Calculate multiplicative rate of news for each subsequent week

# Panel D: Blog mentions per preprint (COVID vs non-COVID)
# Negative binomial regression, blogs ~ preprint type and posting date
blogs_nbmod <- d %>%
  with(., MASS::glm.nb(blogs ~ covid_preprint + serial_date))

# Poisson regression, blogs ~ preprint type and posting date
blogs_poismod <- d %>%
  with(., glm(blogs ~ covid_preprint + serial_date, family = "poisson"))

# Confirm negative binomial as better fitting model
AIC(blogs_nbmod, blogs_poismod)

# Model summary
blogs_nbmod %>% summary()
blogs_nbmod %>%
  coef() %>%
  exp()
1 - (blogs_nbmod %>% coef() %>% .[3] %>% exp() %>% .^7) # Calculate multiplicative rate of blogs for each subsequent week

# Panel E: Count of comments per preprint (COVID vs non-COVID)
# Negative binomial regression, comments ~ preprint type and posting date
comments_nbmod <- d %>%
  with(., MASS::glm.nb(comments ~ covid_preprint + serial_date))

# Poisson regression, comments ~ preprint type and posting date
comments_poismod <- d %>%
  with(., glm(comments ~ covid_preprint + serial_date, family = "poisson"))

# Confirm negative binomial as better fitting model
AIC(comments_nbmod, comments_poismod)

# Model summary
comments_nbmod %>% summary()
comments_nbmod %>%
  coef() %>%
  exp()
1 - (comments_nbmod %>% coef() %>% .[3] %>% exp() %>% .^7) # Calculate multiplicative rate of comments for each subsequent week




# Supplementary Figure 1: Number of preprints in relation to previous epidemics (Zika, Ebola)
# Panel A
# Chi-square test of association
bind_rows(covid_counts, ebola_counts, zika_counts) %>%
  spread(epidemic, value = n) %>%
  column_to_rownames("epi_preprint") %>%
  chisq.test()

# Supplementary Figure 2:
# Panel D: First case -> first preprint by location
# Calculate correlation between first case and first preprint (using calendar days)
preprint_timing %>%
  mutate(
    serial_preprint = (first_preprint - as.Date("2020-01-01")) %>% as.numeric(units = "days"),
    serial_case = (first_case - as.Date("2020-01-01")) %>% as.numeric(units = "days")
  ) %>%
  filter(!is.na(serial_preprint) & !is.na(serial_case)) %>%
  with(., cor.test(serial_preprint, serial_case, method = "spearman"))

# Panel F: Time to publication for different publishers (top 8)
top_publishers <- preprints %>%
  filter(covid_preprint == T,
         posted_date >= "2020-01-01") %>%
  count(published_publisher) %>%
  na.omit() %>%
  arrange(-n) %>%
  slice(1:8) %>%
  pull(published_publisher)

# Two-way ANOVA, time to publishing ~ preprint type, publisher
anova_publishers <- preprints %>%
  filter(posted_date >= "2020-01-01",
         published_publisher %in% top_publishers,
         delay_in_days > 0) %>%
  assign_covid_preprint() %>% 
  with(., aov(delay_in_days ~ published_publisher*covid_preprint))

summary(anova_publishers)


# Supplementary Figure 3: Time prior to our study period
# Panel C: PDF downloads for additional preprint servers
# Two-way ANOVA, downloads ~ preprint type, server (including servers beyond bioRxiv, medRxiv)
anova_servers <- all_server_dloads %>%
  with(., aov(pdf_downloads ~ source * covid_preprint))

summary(anova_servers)

# Posthoc contrasts for specific preprint type/server combinations
all_server_dloads %>%
  assign_covid_preprint() %>%
  mutate(int_source_covid_preprint = interaction(source, covid_preprint)) %>%
  with(., lm(pdf_downloads ~ int_source_covid_preprint - 1)) %>%
  multcomp::glht(., linfct = multcomp::mcp(int_source_covid_preprint = "Tukey")) %>%
  summary()