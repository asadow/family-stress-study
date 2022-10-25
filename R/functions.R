## qualtRics turns variables into factors with recoding values using
## convert TRUE; it maintains the recoding
## So that as_numeric works and keeps the choice text labels
## label = TRUE (data in choice text) default
## label cannot be FALSE while convert is TRUE
## So we must start with choice text
## We cannot start with numeric and convert later to choice
fetch_survey_chr <- function(x, ...)
{fetch_survey(
  surveyID = x,
  col_types = cols(.default = "c"),
  ...
)
}
