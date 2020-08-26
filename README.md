This repository contains code and datasets for the study "Preprinting a pandemic: the role of preprints in the COVID-19 pandemic" [currently available from bioRxiv: [https://www.biorxiv.org/content/10.1101/2020.05.22.111294v1](https://www.biorxiv.org/content/10.1101/2020.05.22.111294v1)].

This README contains an overview of the different files and datasets contained in this repository. The full methodology is documented in the above linked preprint.

# Files

Files referred to here are the main files used for retrieving metadata from various sources and for any analysis/visualisation conducted in the context of the linked study. The following files are contained (all in the upper level directory):

* `preprint_details.Rmd` contains code used to retrieve details of preprints from [bioRxiv](https://biorxiv.org) and [medRxiv](https://medRxiv.org) via the [bioRxiv API](https://api.biorxiv.org), and add data from additional sources such as institutional data from [ROR](https://ror.org), word and reference counts scraped from the bioRxiv website, and additional metadata related to published articles from [Crossref](https://crossref.org) and [Unpaywall](https://unpaywall.org).
* `journal_article_details.Rmd` contains code used to retrieve details of COVID-19 related journal articles, using data from [Dimensions](https://dimensions.ai), Crossref and Unpaywall.
* `preprint_citations.Rmd` contains code used to retrieve citation counts of bioRxiv and medRxiv preprints from Dimensions.
* `preprint_altmetrics.Rmd` contains code used to retrieve altmetric counts (e.g. counts of tweets, mentions in news articles and blogs) from [Altmetric](https://altmetric.com).
* `preprint_comments.Rmd` contains code used to retrieve comment counts for bioRxiv and medRxiv preprints via the [Disqus](https://disqus.com) API.
* `preprint_usage.Rmd` contains code used to retrieve usage data (e.g. abstract views and PDF downloads) for bioRxiv and medRxiv preprints, which are scraped from the public preprint webpages.
* `preprint_firstcase.Rmd` contains code to calculate the date of chronologically first bioRxiv or medRxiv preprint and first COVID-19 case per country.
* `figures.Rmd` contains code for reproducing all the figures and supplementary figures displayed in the study manuscript. Figures are stored in the `outputs/figures` folder.

# Datasets

All datasets are contained in the folder `data`. A description of the contents of each dataset is as follows:

* `preprints_basic_20131101_20181231.csv` and `preprints_basic_20190101_20200430.csv` contain basic metadata of all preprints deposited on bioRxiv and medRxiv from November 2013 (i.e. coinciding with the launch of bioRxiv) to April 2020. These files were split into two due to Github file size limits of 100Mb. The following fields are contained in each file:
  * source: the server to which the preprint was posted, i.e. "bioRxiv" or "medRxiv"
  * doi: the DOI of the preprint
  * posted_date: the date at which the first version of the preprint was posted
  * covid_preprint: a boolean based on presence of COVID-19 related terms in the preprint title or abstract
  * title: the title of the preprint
  * abstract: the abstract of the preprint
  * n_versions: the total number of preprint versions deposited
  * license: the license type chosen by the authors on preprint submission (e.g. "CC BY")
  * type: submission type (e.g. "New Results"). Note that types are missing for older preprints
  * category: submission category (e.g. "Neurology")
  * authors: names of all preprint authors
  * n_authors: total number of preprint authors
  * author_corresponding: name of corresponding author
  * author_corresponding_institution: raw institute string of corresponding author
  * is_published: a boolean determining if a preprint has been published in a scientific journal, based on publication notices posted by bioRxiv/medRxiv directly
  * published_doi: the doi of the related published article, if it exists


* `preprints_full_20190901_20200430.csv` contains enhanced metadata for a subset of preprints published between September 2019 and April 2020 (i.e. encapsulating our 4-month study period, as well as the 4 months previous for comparison purposes). In addition to the basic metadata contained in the basic metadata files above, this file contains the following fields:
  * n_words: word counts of main text
  * n_refs: number of references
  * institution_match_score: the score returned via the ROR affiliation matching tool, expressed as a number between zero (no match) and one (perfect match)
  * institution_match_type: the method used by the ROR affiliation matching tool (e.g. "Phrase" or "Common Terms")
  * institution_match_name: the name of the best matching institution according to the ROR affiliation matching tool
  * institution_match_country_name: the country name in which the best matching affiliation is based
  * institution_match_country_code: the country code in which the best matching affiliation is based
  * published_title: the title of the related published article, if it exists
  * published_date: the publication date of the related published article
  * delay_in_days: the delay time between preprint posting and article publication
  * published_abstract: the abstract of the related published article. Note that this data is derived from Crossref and may thus be incomplete, e.g. if a publisher does not include abstract information in their metadata
  * published_journal: the journal in which the article is published
  * published_publisher: the publisher of the journal in which the article is published
  * published_article_is_oa: a boolean determining if the published article is OA in any form (i.e. on the publisher page or in a repository), according to data from Unpaywall
  * published_journal_is_oa: a boolean determining if the journal in which the published article is published in is an OA journal (according to data from Unpaywall, which is in turn based on data from [DOAJ](https://doaj.org/))
  * published_best_oa_location: determines whether the articles is OA in a journal or OA in a repository. If in both, then 'journal' is considered the 'best' location
  * published_best_oa_license: the license type, if published in a journal (where available)
  
  
* `preprint_altmetrics_20190901_20200430.csv` contains altmetric data for all bioRxiv and medRxiv preprints deposited between September 2019 and April 2020 as well as for their linked published articles. The following fields are contained:
  * doi: the doi of the preprint
  * score: Altmetric Attention Score of the preprint
  * twitter: number of tweets mentioning preprint
  * facebook: number of facebook posts mentioning preprint
  * blogs: number of blogs mentioning preprint
  * news: number of news articles mentioning preprint
  * wikipedia: number of citations to preprint in Wikipedia
  * policies: number of citations to preprint in policy documents
  * published_doi: the doi of the published article
  * published_score: Altmetric Attention Score of the published article
  * published_twitter: number of tweets mentioning published article
  * published_facebook: number of facebook posts mentioning published article
  * published_blogs: number of blogs mentioning published article
  * published_news: number of news articles mentioning published article
  * published_wikipedia: number of citations to published article in Wikipedia
  * published_policies: number of citations to published article in policy documents
  
  
* `preprint_citations_20190901_20200430.csv` contains citation counts for all bioRxiv and medRxiv preprints deposited between September 2019 and April 2020 as well as for their linked published articles. The following fields are contained:
  * doi:  the doi of the preprint
  * citations: the total number of citations received by the preprint
  * published_doi: the doi of the published article
  * published_citations: the total number of citations received by the published article
  
  
* `preprint_usage_20190901_20200430.csv` contains usage data for all bioRxiv and medRxiv preprints deposited between September 2019 and April 2020. The following fields are contained:
  * doi: the doi of the preprint
  * collection_date: the calendar month for which the usage data relates to (non-cumulative)
  * abstract_views
  * full_text_views (only available for bioRxiv preprints)
  * pdf_downloads
  
* `preprint_comments_20190901_20200430.csv` contains counts of comments harvested from the Disqus API, for all bioRxiv and medRxiv preprints deposited between September 2019 and April 2020. The following fields are contained:
  * doi: the doi of the preprint
  * comments_count: the total number of commments received by a preprint. Note that the count is aggregated over all preprint versions (e.g. if V1 received 10 comments, and V2 received 5 comments, the comments_count here would be 15).
  
* `preprint_article_changes.csv` contains a dataset comparing elements of preprints (e.g. the number of figures and tables) to their published papers. The methodology is described in detail in the published paper.

* `preprint_sentiment_tweets.csv` contains aggregated sentiment scores for individual tweets (following removal of retweets) associated with bioRxiv and medRxiv preprints deposited between September 2019 and April 2020:
  * word_count: the word count of the tweet
  * sd: the standard deviation of tweet sentiment
  * ave_sentiment: the mean sentiment score for the tweet (i.e the average of sentiment for each word contained in the tweet)
  * doi: the preprint doi
  * twitter_rank: the rank of the preprint in terms of total number of tweets (e.g. a value of 1 is the top-tweeted preprint)

* `preprint_sentiment_comments.csv` contains aggregated sentiment scores for individual comments associated with each preprint in our dataset (see `cv_mx_disqus_comments.csv` and `cv_bx_disqus_comments.csv` for raw comment data:
  * word_count: the word count of the comment
  * sd: the standard deviation of comment sentiment
  * ave_sentiment: the mean sentiment score for the comment (i.e the average of sentiment for each word contained in the comment)
  * doi: the preprint doi
  * comment_rank: the rank of the preprint in terms of total number of comments (e.g. a value of 1 is the top-commented preprint)
  
* `preprint_earliest_per_country.csv` contains data used to compare the date of the first preprint recorded by a country, against the date of the first case of COVID-19 in that country:
  * doi: the doi of the first relevant COVID-19 preprint from a country
  * author_corresponding_institution: raw institute string of corresponding author 
  * institution_match_score: the score returned via the ROR affiliation matching tool, expressed as a number between zero (no match) and one (perfect match)
  * institution_match_type: the method used by the ROR affiliation matching tool (e.g. "Phrase" or "Common Terms")
  * institution_match_name: the name of the best matching institution according to the ROR affiliation matching tool
  * institution_match_country_name: the country name in which the best matching affiliation is based
  * institution_match_country_code: the country code in which the best matching affiliation is based
  * first_preprint: the date of the first reported preprint from a country
  * Country.Region: the name of the relevant country
  * first_case: the date of the first case of COVID-19 in the relevant country
  * lag: the number of days between the first reported case in a country, and the first relevant COVID-19 preprint

* `cv_mx_disqus_comments.csv` and `cv_bx_disqus_comments.csv`: comment data for COVID-19 preprints deposited to medRxiv and bioRxiv, respectively:
  * id: the preprint ID
  * dsqus_id: the Disqus ID of the comment
  * dsqus_user: the Disqus user ID of the commenter
  * dsqus_name: the Disqus user name of the commenter
  * dsqus_comment: the Disqus comment text
  * preprint_doi: doi of the preprint being commented on
  * dsqus_date: the date on which the comment was made
  * dsqus_time: the time at which the comment was made
  * dsqus_timestamp: timestamp of comment
  * dsqus_url: url to the preprint being commented on

* `journal_articles_20200101_20200430.csv` contains metadata for all journal articles (i.e. not only those linked to our dataset of preprints) relating to COVID-19, extracted from Dimensions, Crossref and Unpaywall:
  * doi: the journal article DOI
  * created: the publication date (based on the 'created' date issued by Crossref, i.e. the date of the initial DOI registration)
  * title: the journal article title
  * journal: the name of the journal in which the article was published
  * publisher: the name of the publisher associated with the journal
  * times_cited: the total number of citations received by the article (until May 1st, 2020)
  * article_is_oa: a boolean determining if the article is OA in any form (i.e. on the publisher page or in a repository), according to data from Unpaywall
  * journal_is_oa: a boolean determining if the journal in which the published article is published in is an OA journal (according to data from Unpaywall, which is in turn based on data from [DOAJ](https://doaj.org/))
  * best_oa_location: determines whether the articles is OA in a journal or OA in a repository. If in both, then 'journal' is considered the 'best' location
  * best_oa_license: the license type, if published in a journal (where available)
  
* `policy_docs.csv` contains data for manually identified policy documents and their bibliographies/reference lists:
  * type: topic of policy document, either COVID-19 or control (i.e. non-COVID-19)
  * set: the source or series of the policy document
  * doc: the filename of the policy document
  * nrefs: the number of items the policy document cites in total
  * nbiorxiv: the number of bioRxiv preprints cited
  * nmedrxiv: the number of medRxiv preprints cited
  * nssrn: the number of SSRN preprints cited
  * nosf: the number of OSF preprints cited
  * nressquare: the number of Research Square preprints cited
  * narxiv: the number of arXiv preprints cited
  * npreprints: the total number of preprints cited (sum of nbiorxiv:narxiv)
