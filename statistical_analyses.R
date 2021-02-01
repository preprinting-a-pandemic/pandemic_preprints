# Descriptive statistics function, returning median and IQR, mean and sd, and Anderson-Darling test for normality
descriptive_stats <- function(d, colnamestring) {
  d %>%
    filter(posted_date >= analysis_start,
           posted_date <= analysis_end) %>%
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

# Supplementary Table: descriptive statistics by server*preprint type combination
t1 <- preprints %>%
  filter(posted_date >= analysis_start,
         posted_date <= analysis_end) %>%
  group_by(covid_preprint, source) %>%
  summarise(n = n()) %>%
  ungroup()

t2 <- preprints %>%
  filter(posted_date >= analysis_start,
         posted_date <= analysis_end) %>%
  mutate(
    doi_date = gsub("\\.", "-", substr(doi, 9, 18)),
    days = as.numeric(ymd(posted_date) - ymd(doi_date))
  ) %>%
  group_by(covid_preprint, source) %>%
  summarise_at(.vars = c("days", "n_authors", "n_versions", "n_words", "n_refs"), .funs = funs(
    stat = paste0(median(., na.rm = TRUE), " (", IQR(., na.rm = TRUE), ")")
  )) %>%
  ungroup()

t3 <- preprint_usage %>%
  inner_join(preprints, by = c("doi", "source")) %>%
  group_by(doi, source, posted_date, covid_preprint) %>%
  summarize(
    full_text_views = sum(full_text_views),
    abstract_views = sum(abstract_views),
    pdf_downloads = sum(pdf_downloads)
  ) %>%
  ungroup() %>%
  filter(posted_date >= analysis_start,
         posted_date <= analysis_end) %>%
  group_by(covid_preprint, source) %>%
  summarise_at(.vars = c("abstract_views", "pdf_downloads"), .funs = funs(
    stat = paste0(median(., na.rm = TRUE), " (", IQR(., na.rm = TRUE), ")")
  )) %>%
  ungroup()

t4 <- preprints %>%
  filter(posted_date >= analysis_start,
         posted_date <= analysis_end) %>%
  inner_join(preprint_citations, by = "doi") %>%
  inner_join(preprint_comments %>% rename(comments = comments_count), by = "doi") %>%
  inner_join(preprint_altmetrics, by = "doi") %>%
  group_by(covid_preprint, source) %>%
  summarise_at(.vars = c("citations", "twitter", "news", "blogs", "wikipedia", "comments"), .funs = funs(
    stat = paste0(median(., na.rm = TRUE), " (", IQR(., na.rm = TRUE), ")")
  )) %>%
  ungroup()

cbind(t1,
      t2 %>% select(-covid_preprint, -source),
      t3 %>% select(-covid_preprint, -source),
      t4 %>% select(-covid_preprint, -source)) %>%
  assign_covid_preprint() %>%
  mutate(set = interaction(source, covid_preprint)) %>%
  select(-covid_preprint, -source) %>% 
  gather(variable, value, -set) %>% 
  mutate(variable = factor(variable, levels=unique(variable))) %>%
  spread(set, value) %>%
  mutate(variable = gsub("_stat", "", variable)) %>% write.csv("supplementary_table_descriptive_stats.csv")


# Figure 2: Preprint attributes
# Panel B: Preprint screening time
# Descriptive statistics
preprints %>%
  filter(posted_date >= analysis_start,
         posted_date <= analysis_end) %>%
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
  filter(posted_date >= analysis_start,
         posted_date <= analysis_end) %>%
  mutate(
    doi_date = gsub("\\.", "-", substr(doi, 9, 18)),
    days = as.numeric(ymd(posted_date) - ymd(doi_date))
  ) %>%
  with(., aov(days ~ source * covid_preprint))

summary(anova_screening)

# Posthoc contrasts for specific preprint type/server combinations
preprints %>%
  filter(posted_date >= analysis_start,
         posted_date <= analysis_end) %>%
  assign_covid_preprint() %>%
  mutate(
    doi_date = gsub("\\.", "-", substr(doi, 9, 18)),
    days = as.numeric(ymd(posted_date) - ymd(doi_date)),
    int_source_covid_preprint = interaction(source, covid_preprint)
  ) %>%
  with(., lm(days ~ int_source_covid_preprint - 1)) %>%
  multcomp::glht(., linfct = multcomp::mcp(int_source_covid_preprint = "Tukey")) %>%
  summary()

# Panel C: Revisions per preprint for COVID vs non-COVID
# Descriptive statistics
descriptive_stats(preprints, "n_versions")
# Mann-Whitney test
preprints %>%
  filter(posted_date >= analysis_start,
         posted_date <= analysis_end) %>%
  with(., wilcox.test(n_versions ~ covid_preprint))

# Panel D: License types
# Chi-squared test of association
preprints %>%
  filter(posted_date >= analysis_start,
         posted_date <= analysis_end) %>%
  mutate(license = case_when(
    str_detect(license, "cc0") ~ "cc0",
    T ~ license
  )) %>%
  with(., table(license, covid_preprint)) %>%
  chisq.test()

# Panel E: Word counts
# Descriptive statistics
descriptive_stats(preprints %>% filter(source == "biorxiv"), "n_words")
# Mann-Whitney test
preprints %>%
  filter(posted_date >= analysis_start,
         posted_date <= analysis_end,
         source == "biorxiv") %>%
  with(., wilcox.test(n_words ~ covid_preprint))

# Panel F: Reference counts
# Descriptive statistics
descriptive_stats(preprints %>% filter(source == "biorxiv"), "n_refs")
# Mann-Whitney, number of references
preprints %>%
  filter(posted_date >= analysis_start,
         posted_date <= analysis_end,
         source == "biorxiv") %>%
  with(., wilcox.test(n_refs ~ covid_preprint))


# Figure 3: Preprint authorship
# Panel A: Author counts
# Descriptive statistics
descriptive_stats(preprints, "n_authors")
# Mann-Whitney test
preprints %>%
  filter(posted_date >= analysis_start,
         posted_date <= analysis_end) %>%
  with(., wilcox.test(n_authors ~ covid_preprint))

# Panel C: Author attributes: country, nationality (top 10 countries)
# Examine first-time preprinters pooling all countries
preprinter_history %>%
  with(., table(author_group, preprinter_status)) %>%
  prop.table(1) * 100
preprinter_history %>%
  with(., table(author_group, preprinter_status)) %>%
  chisq.test()

# Individual tests of first-time preprinters by country, only top 15 countries
options(scipen = 999)
rcompanion::groupwiseCMH(xtabs(n ~ preprinter_status + author_group + institution_match_country_code,
                               data = preprinter_history_tabular),
                         group = 3,
                         fisher = FALSE,
                         gtest = FALSE,
                         chisq = TRUE,
                         method = "bonferroni",
                         correct = "none",
                         digits = 3
) %>% arrange(adj.p)

# Full statistical test for United States, UK, Germany, India, France, Canada, Italy, China
xtabs(n ~ preprinter_status + author_group,
      data = preprinter_history_tabular %>% filter(institution_match_country_code == "US")) %>%
  prop.table(2) %>% round(3)*100

xtabs(n ~ preprinter_status + author_group,
      data = preprinter_history_tabular %>% filter(institution_match_country_code == "GB")) %>%
  prop.table(2) %>% round(3)*100

xtabs(n ~ preprinter_status + author_group,
      data = preprinter_history_tabular %>% filter(institution_match_country_code == "DE")) %>%
  prop.table(2) %>% round(3)*100

xtabs(n ~ preprinter_status + author_group,
      data = preprinter_history_tabular %>% filter(institution_match_country_code == "IN")) %>%
  prop.table(2) %>% round(3)*100

xtabs(n ~ preprinter_status + author_group,
      data = preprinter_history_tabular %>% filter(institution_match_country_code == "FR")) %>%
  prop.table(2) %>% round(3)*100

xtabs(n ~ preprinter_status + author_group,
      data = preprinter_history_tabular %>% filter(institution_match_country_code == "CA")) %>%
  prop.table(2) %>% round(3)*100

xtabs(n ~ preprinter_status + author_group,
      data = preprinter_history_tabular %>% filter(institution_match_country_code == "IT")) %>%
  prop.table(2) %>% round(3)*100

xtabs(n ~ preprinter_status + author_group,
      data = preprinter_history_tabular %>% filter(institution_match_country_code == "CN")) %>%
  prop.table(2) %>% round(3)*100

# Panel D: First case -> first preprint by location
# Calculate correlation between first case and first preprint (using calendar days)
preprint_timing %>%
  mutate(
    serial_preprint = (first_preprint - as.Date("2020-01-01")) %>% as.numeric(units = "days"),
    serial_case = (first_case - as.Date("2020-01-01")) %>% as.numeric(units = "days")
  ) %>%
  filter(!is.na(serial_preprint) & !is.na(serial_case)) %>%
  with(., cor.test(serial_preprint, serial_case, method = "spearman"))


# Figure 4: Publication outcomes
# Percentage of preprints published
# Percentage values
preprints %>%
  filter(posted_date >= analysis_start,
         posted_date <= analysis_end) %>%
  mutate(is_published = !is.na(published_doi)) %>%
  with(., table(covid_preprint, is_published)) %>%
  prop.table(1) * 100
# Chi-square test of association
preprints %>%
  filter(posted_date >= analysis_start,
         posted_date <= analysis_end) %>%
  mutate(is_published = !is.na(published_doi)) %>%
  with(., table(covid_preprint, is_published)) %>%
  chisq.test()

# Panel C: Publishing timeline
# Descriptive statistics (including control period Jan - Dec 2019)
preprints %>%
  mutate(covid_preprint = case_when(
    (covid_preprint == T & posted_date >= analysis_start & posted_date <= analysis_end) ~ "COVID-19 preprints",
    (covid_preprint == F & posted_date >= analysis_start & posted_date <= analysis_end) ~ "non-COVID-19 preprints",
    T ~ "preprints 2019")) %>%
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

# Mann-Whitney, time to publishing (excluding control period Jan - Dec 2019)
preprints %>%
  filter(posted_date >= analysis_start,
         posted_date <= analysis_end,
         delay_in_days > 0) %>%
  with(., wilcox.test(delay_in_days~covid_preprint))

# Panel D: Time to publication for different publishers (top 10)
top_publishers <- preprints %>%
  filter(covid_preprint == T,
         posted_date >= analysis_start,
         posted_date <= analysis_end) %>%
  count(published_publisher) %>%
  na.omit() %>%
  top_n(10, n) %>%
  pull(published_publisher)

# Two-way ANOVA, time to publishing ~ preprint type, publisher
anova_publishers <- preprints %>%
  filter(posted_date >= analysis_start,
         posted_date <= analysis_end,
         published_publisher %in% top_publishers,
         delay_in_days > 0) %>%
  assign_covid_preprint() %>% 
  with(., aov(delay_in_days ~ published_publisher*covid_preprint))

summary(anova_publishers)

# Posthoc contrasts for specific preprint type/publisher combinations
preprints %>%
  filter(posted_date >= analysis_start,
         posted_date <= analysis_end,
         published_publisher %in% top_publishers,
         delay_in_days > 0) %>%
  mutate(published_publisher = factor(published_publisher,
                                      levels = c("American Association for the Advancement of Science (AAAS)",
                                                 "American Society for Microbiology",
                                                 "BMJ",
                                                 "Elsevier BV",
                                                 "Frontiers Media SA",
                                                 "MDPI AG",
                                                 "Oxford University Press (OUP)",
                                                 "Public Library of Science (PLoS)",
                                                 "Springer Science and Business Media LLC",
                                                 "Wiley"),
                                      labels = c("AAAS", "ASM", "BMJ", "Elsevier", "Frontiers", 
                                                 "MDPI", "OUP", "PLoS", "Springer", "Wiley")),
         int_publisher_covid_preprint = interaction(published_publisher, covid_preprint)) %>%
  with(., lm(delay_in_days ~ int_publisher_covid_preprint - 1)) %>%
  multcomp::glht(., linfct = multcomp::mcp(int_publisher_covid_preprint = c(
    "AAAS.FALSE - AAAS.TRUE = 0", 
    "ASM.FALSE - ASM.TRUE = 0",
    "BMJ.FALSE - BMJ.TRUE = 0",
    "Elsevier.FALSE - Elsevier.TRUE = 0",
    "Frontiers.FALSE - Frontiers.TRUE = 0",
    "MDPI.FALSE - MDPI.TRUE = 0",
    "OUP.FALSE - OUP.TRUE = 0",
    "PLoS.FALSE - PLoS.TRUE = 0",
    "Springer.FALSE - Springer.TRUE = 0",
    "Wiley.FALSE - Wiley.TRUE = 0"))) %>%
  summary()


# Figure 5: Preprint access
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
  mutate(
    posted_week = floor_date(posted_date, unit = "week", week_start = 1),
    serial_date = (posted_date - as.Date("2020-01-01")) %>%
      as.numeric(units = "days")
  ) %>%
  filter(posted_date >= analysis_start,
         posted_date <= analysis_end)

# Negative binomial regression, views ~ preprint type and posting date
views_nbmod <- d %>%
  with(., MASS::glm.nb(abstract_views ~ covid_preprint * serial_date))

# Poisson regression, views ~ preprint type and posting date
views_poismod <- d %>%
  with(., glm(abstract_views ~ covid_preprint * serial_date, family = "poisson"))

# Confirm negative binomial as better fitting model
AIC(views_nbmod, views_poismod)

# Model summary
views_nbmod %>% summary()
views_nbmod %>% 
  coef() %>% 
  exp
1 - (views_nbmod %>% coef() %>% .[3] %>% exp() %>% .^30) # Calculate multiplicative rate of views for each subsequent month for non-COVID-19 preprints
1 - (views_nbmod %>% coef() %>% .[3:4] %>% sum %>% exp() %>% .^30) # Calculate multiplicative rate of views for each subsequent month for COVID-19 preprints

# Panel B: PDF downloads
# Negative binomial regression, downloads ~ preprint type and posting date
dloads_nbmod <- d %>%
  with(., MASS::glm.nb(pdf_downloads ~ covid_preprint * serial_date))

# Poisson regression, downloads ~ preprint type and posting date
dloads_poismod <- d %>%
  with(., glm(pdf_downloads ~ covid_preprint * serial_date, family = "poisson"))

# Confirm negative binomial as better fitting model
AIC(dloads_nbmod, dloads_poismod)

# Model summary
dloads_nbmod %>% summary()
dloads_nbmod  %>% 
  coef() %>% 
  exp()
1 - (dloads_nbmod %>% coef() %>% .[3] %>% exp() %>% .^30) # Calculate multiplicative rate of views for each subsequent month for non-COVID-19 preprints
1 - (dloads_nbmod %>% coef() %>% .[3:4] %>% sum %>% exp() %>% .^30) # Calculate multiplicative rate of views for each subsequent month for COVID-19 preprints


# Figure 6: Preprint usage
# Prepare data
d <- preprints %>%
  filter(posted_date >= analysis_start,
         posted_date <= analysis_end) %>%
  inner_join(preprint_citations, by = "doi") %>%
  inner_join(preprint_comments %>% rename(comments = comments_count), by = "doi") %>%
  inner_join(preprint_altmetrics, by = "doi") %>%
  mutate(
    posted_week = floor_date(posted_date, unit = "week", week_start = 1),
    serial_date = (posted_date - as.Date("2020-01-01")) %>%
      as.numeric(units = "days")
  )

# Panel A: Citations per preprint (COVID vs non-COVID)
# Calculate table of any vs none
d %>%
  mutate(citationflag = ifelse(citations == 0, "0", "1")) %>%
  with(., table(covid_preprint, citationflag)) %>%
  prop.table(1) * 100

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
1 - (citations_nbmod %>% coef() %>% .[3] %>% exp() %>% .^30) # Calculate multiplicative rate of citations for each subsequent month

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
1 - (tweets_nbmod %>% coef() %>% .[3] %>% exp() %>% .^30) # Calculate multiplicative rate of tweets for each subsequent month

# Panel C: News mentions per preprint (COVID vs non-COVID)
# Calculate table of any vs none
d %>%
  mutate(newsflag = ifelse(news == 0, "0", "1")) %>%
  with(., table(covid_preprint, newsflag)) %>%
  prop.table(1) * 100

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
1 - (news_nbmod %>% coef() %>% .[3] %>% exp() %>% .^7) # Calculate multiplicative rate of news for each subsequent month

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
1 - (blogs_nbmod %>% coef() %>% .[3] %>% exp() %>% .^30) # Calculate multiplicative rate of blogs for each subsequent month

# Panel E: Wikipedia mentions per preprint (COVID vs non-COVID)
# Negative binomial regression, wikipedia ~ preprint type and posting date
wikipedia_nbmod <- d %>%
  with(., MASS::glm.nb(wikipedia ~ covid_preprint + serial_date))

# Poisson regression, wikipedia ~ preprint type and posting date
wikipedia_poismod <- d %>%
  with(., glm(wikipedia ~ covid_preprint + serial_date, family = "poisson"))

# Confirm negative binomial as better fitting model
AIC(wikipedia_nbmod, wikipedia_poismod)

# Model summary
wikipedia_nbmod %>% summary()
wikipedia_nbmod %>%
  coef() %>%
  exp()
1 - (wikipedia_nbmod %>% coef() %>% .[3] %>% exp() %>% .^30) # Calculate multiplicative rate of wikipedia mentions for each subsequent month

# Panel F: Count of comments per preprint (COVID vs non-COVID)
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
1 - (comments_nbmod %>% coef() %>% .[3] %>% exp() %>% .^30) # Calculate multiplicative rate of comments for each subsequent month


# Supplementary Figure 1: Number of preprints in relation to previous epidemics (Zika, Ebola)
# Panel A
# Chi-square test of association
bind_rows(covid_counts, ebola_counts, zika_counts) %>%
  spread(epidemic, value = n) %>%
  column_to_rownames("epi_preprint") %>%
  chisq.test()


# Supplementary Figure 3: Time prior to our study period
# Panel C: PDF downloads for additional preprint servers
# Two-way ANOVA, downloads ~ preprint type, server (including servers beyond bioRxiv, medRxiv)
all_server_dloads_aov <- bind_rows(preprints %>%
                                     left_join(preprint_usage, by = c("doi", "source")) %>%
                                     group_by(source, doi, posted_date, covid_preprint) %>%
                                     summarize(pdf_downloads = sum(pdf_downloads)) %>%
                                     ungroup() %>%
                                     select(-doi) %>%
                                     mutate(pdf_downloads = replace_na(pdf_downloads, 0)),
                                   other_server_dloads %>%
                                     mutate(posted_date = as.Date(posted_date, "%d/%m/%Y"))) %>%
  filter(posted_date >= analysis_start & posted_date <= analysis_end)

anova_servers <- all_server_dloads_aov %>%
  with(., aov(pdf_downloads ~ source * covid_preprint))

summary(anova_servers)

# Posthoc contrasts for specific preprint type/server combinations
all_server_dloads %>%
  assign_covid_preprint() %>%
  mutate(int_source_covid_preprint = interaction(source, covid_preprint)) %>%
  with(., lm(pdf_downloads ~ int_source_covid_preprint - 1)) %>%
  multcomp::glht(., linfct = multcomp::mcp(int_source_covid_preprint = "Tukey")) %>%
  summary()

# Supplementary Model: Factors associated with word count
# Specify data (bioRxiv, complete word count)
d <- preprints %>%
  filter(posted_date >= analysis_start,
         posted_date <= analysis_end,
         source == "biorxiv",
         n_words != 0,
         n_refs != 0,
         !is.na(category),
         !is.na(institution_match_country_name)) %>%
  mutate(is_published = !is.na(published_doi),
         serial_date = (posted_date - as.Date("2020-01-01")) %>%
           as.numeric(units = "days"))

# # Use top 10 cats/top 15 countries only? - NOT USED
# top_cats <- d %>% count(category) %>% top_n(10, n) %>% pull(category)
# top_nats <- d %>% count(institution_match_country_name) %>% top_n(15, n) %>% pull(institution_match_country_name)
# 
# #d <- d %>% filter(category %in% top_cats & institution_match_country_name %in% top_nats)

# Set baselines as most common categories
d <- d %>% mutate(institution_match_country_name = fct_relevel(institution_match_country_name, "United States", after = 0),
                  category = fct_relevel(category, "neuroscience", after = 0))

# Build mixed-effects regression: all bioRxiv preprints
library(lme4)
library(lmerTest)

words_mix <- lmer(n_words ~ (1|category) + (1|institution_match_country_name) + n_authors + is_published*serial_date + covid_preprint, data = d, REML = TRUE)
summary(words_mix)
confint(words_mix)
car::vif(words_mix)

# Calculate intraclass coefficients
vcov <- words_mix %>% VarCorr(comp=c("Variance", "Std.Dev")) %>% as.data.frame  %>% pull(vcov)
vcov[1]/sum(vcov)
vcov[2]/sum(vcov)

# LRTs for random effects
anova(words_mix, 
      lmer(formula = update(formula(words_mix), ~ . - (1|category)), data = d))

anova(words_mix, 
      lmer(formula = update(formula(words_mix), ~ . - (1|institution_match_country_name)), data = d))

# Build mixed-effects regression: published bioRxiv preprints only
words_pub_mix <- lmer(n_words ~ (1|category) + (1|institution_match_country_name) + n_authors + serial_date + delay_in_days*covid_preprint, data = d %>% filter(delay_in_days > 0), REML = TRUE)
summary(words_pub_mix)
confint(words_pub_mix)
car::vif(words_pub_mix)

# Calculate intraclass coefficients
vcov <- words_pub_mix %>% VarCorr(comp=c("Variance", "Std.Dev")) %>% as.data.frame  %>% pull(vcov)
vcov[1]/sum(vcov)
vcov[2]/sum(vcov)

# LRTs for random effects
anova(words_pub_mix, 
      lmer(formula = update(formula(words_pub_mix), ~ . - (1|category)), data = d %>% filter(delay_in_days > 0)))

anova(words_pub_mix, 
      lmer(formula = update(formula(words_pub_mix), ~ . - (1|institution_match_country_name)), data = d %>% filter(delay_in_days > 0)))
