#' keywords relating to COVID-19 pandemics
#'
#' A list of keywords relating to the COVID-19 pandemic
#'
#' @format A dataframe containing one variable:
#' \itemize{
#'   \item keys: list of keywords
#'     }
"covid_keys"


#' Observed sentiment document (OSD).
#'
#' A tidy-format list (dataframe) showing the resulting
#' classification of each text records into positive, negative
#' or neutral sentiment. The second column of the dataframe consists of
#' labels variables `present` and `absent` to indicate whether any of the secondary
#' keywords exist in a text record.
#'
#' @format A dataframe with the following variables:
#' \itemize{
#'   \item ID: numeric id of text record with valid
#'   resultant sentiments score and classification.
#'   \item sentiment: Containing the sentiment classes.
#'   \item keywords: Indicator to show whether a secondary
#'   keyword in present or absent in a text record.
#'     }
"osd_data"


#' Fake Twitter posts on police/policing
#'
#' A text document (an OTD) containing twitter posts
#' (for an anonymous geographical location 1) on police/policing.
#' The OTD also includes
#' posts that express sentiments on policing in relation to
#' the COVID-19 pandemic (Secondary subject B)
#'
#' @format A dataframe containing one variable
#' \itemize{
#'   \item text: individual text records
#'     }
"policing_dtd"

#' Fake Twitter posts on police/policing 2
#'
#' A text document (an OTD) containing twitter posts
#' (for an anonymous geographical location 2) on police/policing
#' (primary subject A). The OTD includes
#' posts that express sentiments on policing in relation to
#' the COVID-19 pandemic (Secondary subject B)
#'
#' @format A dataframe with the following variables:
#' \itemize{
#'   \item text: individual text records
#'   \item group: real/arbitrary groups of text records
#'     }
"tweets"

#' Keywords relating to signages at train stations
#'
#' List of signages at the Piccadilly Train
#' Station (Manchester)
#'
#' @format A dataframe containing one variable:
#' \itemize{
#'   \item keys: list of keywords
#'     }
"signage_keys"

#' Keywords relating to facilities at train stations
#'
#' List of facilities at the Piccadilly Train
#' Station (Manchester)
#'
#' @format A dataframe containing one variable:
#' \itemize{
#'   \item keys: list of keywords
#'     }
"facility_keys"

#' Customer reviews from tripadvisor website
#'
#' A text document (an OTD) containing the  customer reviews
#' of the Piccadilly train station (Manchester) downloaded
#' from the www.tripadvisor.co.uk'. The reviews cover from
#' July 2016 to March 2021.
#'
#' @format A dataframe containing one variable
#' \itemize{
#'   \item text: individual text records
#'     }
"reviews_dtd"

