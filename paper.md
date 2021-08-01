---
title: "R-Opitools – An Opinion Analytical Tool for Big Digital Text Document (DTD)"
date: "30th July 2021"
bibliography: paper.bib
output: pdf_document
affiliations:
- name: Crime and Well-being Big Data Centre, Manchester Metropolitan University
  index: 1
tags:
- digital text document
- sentiment analysis
- opinion mining
- randomization testing
authors:
- name: Monsuru Adepeju
  orcid: 0000-0002-9006-4934
  affiliation: 1
---

# Statement of Need

Since the year 2000, various computational intelligence techniques have been developed for analyzing sentiments of users either in the field of natural language processing or computational linguistics. To date, the majority of the techniques as deployed across various fields, including Social science [@Somasundaran:2010; @Morency:2011; @Nikolovska:2020], Market research [@Feldman:2011; @Otaibi:2018], and in Electoral politics [@Ansari:2020; @Connor:2010] have focused largely on detecting subjectivity, and/or extracting and classifying sentiments and opinions in a text document. Building on this existing work, the current paper advances an opinion impact analytical tool, namely; `Opitools`, that does not only extracts inherent themes from within a digital text document (DTD), but also evaluates the extent to which a specified theme may have contributed to the overall opinion expressed in the document. Based on this advancement, `Opitools` holds prospect for wider applications, as compared to the existing techniques, in the aforementioned application fields. For example, in Law enforcement, the package can be deployed to understand factors that drive public perception of the police services [@Adepeju:2021]; in Market research, to identify factors that underlie customers satisfaction in a product; and, in Electoral politics, to identify factors that may be influencing electoral opinions in an upcoming election. These stated examples are also illustrated in documentation of the package (please see the `README.md` and the package `Vignette` for details). Lastly, it is argued that this contribution provides a framework by which some of the existing opinion mining tools [@Tai:2015; @Kiomourtzis:2014; @Wawer:2016; @Munson:2019] may be further advanced for wider research impacts.

# Implementation

Having extracted a set of thematic keywords from a digital text document, the goal is to computationally classify the sentiments expressed in each text record into positive, negative or a neutral sentiment, using a lexicon-based classification approach [@Nielsen:2011; @Adepeju:2021]. The resulting sentiment scores are combined in order to estimate the overall opinion score of the document. To assess the impacts of a selected theme (or a subject) on the estimated opinion score, we simply ask the question; *If expected opinion scores were generated under the null hypothesis, how likely would we be to find a score higher than the estimated score?*. The question is answered by employing a non-parametric randomization testing [@Fisher:1935; @Good:2006] strategy involving random re-assignment of sentiment labels of the original text document to derive the expectation distribution. By comparing the observed score with the expectation distribution, the statistical significance of impacts is derived.


# Key Functionalities

The `Opitools` package includes a function, i.e. `opi_imp`,  for exploring themes of a digital text document. To conduct the impact analysis, the main function is the `opi_impact()` which draws from two supporting functions, `opi_score()` and `opi_sim()`, to compute the observed opinion score and its expectations, respectively. Four different opinion score functions are embedded in the package, plus a `'fun'` parameter to allow a user to specify their own pre-defined score functions. This provision is to further facilitate the uptake of the `Opitools` in any application field.

# Acknowledgment

We gratefully acknowledge the Economic and Social Research Council (ESRC), who funded the Understanding Inequalities project (Grant Reference ES/P009301/1) through which this research was conducted.

# References

