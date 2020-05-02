This repository contains code and datasets for the study "Preprinting a pandemic: the role of preprints in the 2020 COVID-19 pandemic" [work in progress].

The README below details the general approach to harvesting data for this study, and a more detailed overview of the data contained in each of the main datasets.

# General approach

### Preprint metadata

Basic metadata of *all* preprints (i.e. not just those related to COVID-19) posted on bioRxiv and medRxiv were harvested via the [bioRxiv API](https://www.altmetric.com/products/altmetric-api/) (note that the bioRxiv API allows querying of medRxiv records by changing the "server" parameter).

The bioRxiv API also supplies DOI-DOI links between preprints and published articles, based on links tracked by bioRxiv/medRxiv directly. These may be incomplete, in particular for more recently published articles, or where titles have changed between a preprint and its published version. Some information on this is found in the [bioRxiv FAQ](https://www.biorxiv.org/about/FAQ). 

Preprint metadata harvested from the bioRxiv API was enhanced through several additional sources:

* Raw author affiliation strings were queried against the [ROR affiliation matching tool](https://github.com/ror-community/ror-api), to return disambiguated affiliation and country data. A similar approach was taken in a recent study on bioRxiv international authorship by [Abdill, Adamowicz and Blekhman](https://www.biorxiv.org/content/10.1101/2020.04.25.060756v1). The service returns a "best matching" affiliation, as well as a matching score which may be used as a threshold to include/exclude certain records.
* Where a preprint was linked with a published article, more detailed metadata relating to the published article was added via the [Crossref API](http://api.crossref.org/). In addition, the OA status of the published article was determined using [Unpaywall](http://unpaywall.org/).

### Altmetrics 

Almetric attention scores, and count data for individual indicators (e.g. tweets, facebook posts) were harvested for each preprint as well as their linked published article by querying DOIs against the [Altmetric.com API](https://www.altmetric.com/products/altmetric-api/). This service is free, but supplying an API key will remove rate limits. API keys can be requested through the Altmetric [Researcher Data Access Program](https://www.altmetric.com/research-access/).

### Citation counts

Citation counts for each preprint as well as their linked published article were harvested via the [Dimensions API](https://www.dimensions.ai/). Accessing the Dimensions API requires an API key, which can be requested through the Dimensions [Research Data Access Program](https://www.dimensions.ai/scientometric-research/). 

### Usage data

Usage data for preprints only were harvested by crawling of the public bioRxiv and medRxiv webpages. Usage data includes abstract views, pdf downloads, and for bioRxiv only, full text views. Usage data is available only at a monthly level.

### Journal articles

As well as collecting metadata of preprints, a dataset of journal articles relating COVID-19 was collected through Dimensions. The dataset includes articles with search terms related to COVID-19 in their titles and abstracts only. As well as basic article metadata, citation counts (also from Dimensions), altmetric counts (from Altmetric.com) and OA information (from Unpaywall) were added.

# Datasets

All datasets are contained in the folder `data`. A description of the contents of each dataset is as follows:

* `preprints_basic_20131101_20181231.csv` and `preprints_basic_20190101_20200430.csv` contain basic metadata of all preprints deposited on bioRxiv and medRxiv from November 2013 (i.e. coinciding with the launch of bioRxiv) to April 2020. These files were split into two due to Github file size limits (files must be <100Mb). The following fields are contained in each file:
  * source: "bioRxiv" or "medRxiv"
  * doi: the doi of the preprint
  * posted_date
  * covid_preprint: a boolean based on presence of COVID-19 related terms in the preprint title or abstract
  * title
  * abstract
  * n_versions: the total number of preprint versions deposited. Note that we only keep metadata relating to the earliest deposited version
  * type: submission type (e.g. "New Results"). Note that types are missing for older preprints
  * category: submission category (e.g. "neurology")
  * authors: names of all preprint authors
  * n_authors: total number of preprint authors
  * author_corresponding: name of corresponding author
  * author_corresponding_institution: raw institute string of corresponding author
  * is_published: a boolean determining if a preprint has been published in a scientific journal, based on publication notices posted by bioRxiv/medRxiv directly
  * published_doi: the doi of the related published article, if it exists


* `preprints_full_20190901_20200430.csv` contains enhanced metadata for a subset of preprints published between September 2019 and April 2020 (i.e. encapsulating our 4-month study period, as well as the 4 months previous for comparison purposes). In addition to the basic metadata contained in the basic metadata files above, this file contains the following fields:
  * institution_match_score: the score returned via the ROR affiliation matching tool, expressed as a number between zero (no match) and one (perfect match)
  * institution_match_type: the method used by the ROR affiliation matching tool (e.g. "Phrase" or "Common Terms")
  * institution_match_name: the name of the best matching institution according to the ROR affiliation matching tool
  * institution_match_country_name: the country name in which the best matching affiliation is based
  * institution_match_country_code: the country code in which the best matching affiliation is based
  * published_title: the title of the related published article, if it exists
  * published_date: the publication date of the related published article
  * delay_in_days: the delay time between preprint posting and article publication
  * published_abstract: the abstract of the related published article. Note that this data is derived from Crossref and may thus be incomplete, e.g. if a publisher does not include abstract information in their metadata
  * published_journal
  * published_publisher
  * published_article_is_oa: a boolean determining if the published article is OA in any form (i.e. on the publisher page or in a repository), according to Unpaywall
  * published_journal_is_oa: a boolean determining if the journal in which the published article is published in is an OA journal (according to Unpaywall, which is in turn based on data from [DOAJ](https://doaj.org/))
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
  

Descriptions to complete:

* `journal_articles_20200101_20200430.csv`
* `journal_article_altmetrics_20200101_20200430.csv`