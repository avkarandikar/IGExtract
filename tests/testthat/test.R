library(IGExtract)
library(testthat)

chunked = data.frame(
  document = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
  text = c("Power plants", "must not", "ever", "pollute", "the air",
           "and also",
           "sewage plants", "must not", "ever", "pollute", "the water"),
  component = c("attribute", "deontic", NA, "aim", "object",
                NA,
                "attribute", "deontic", NA, "aim", "object"),
  statement_ID = c(1, 1, 1, 1, 1,
                   2,
                   3, 3, 3, 3, 3)
)
unchunked = data.frame(
  document = 2,
  text = "Chemical plants must not ever pollute the soil"
)

set.seed(1)
tags = tag(chunked, unchunked)
my_features = features(tags,
                       number_of_words = 3,
                       word_window = 3,
                       part_of_speech_window = 3)
balanced = balance(my_features$chunked, minimum_number = 0)
my_training_and_testing = training_and_testing(my_features$chunked)
chunker = chunker(my_training_and_testing$training)
validated = validate(chunker, my_training_and_testing$testing)
chunked = chunk(chunker, my_features$unchunked)

expect_equal(chunked$text, c(
  "Chemical plants",
  "must not",
  "ever",
  "pollute",
  "the soil"
))
expect_equal(chunked$component, c(
  "attribute",
  "deontic",
  "component",
  "aim",
  "object"
))
expect_equal(sum(diag(validated)), 7)
expect_equal(nrow(balanced), 18)
