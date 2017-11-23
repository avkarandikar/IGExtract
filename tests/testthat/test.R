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
features = features(chunked, unchunked, number_of_words = 3, window = 3)
training_and_testing = training_and_testing(features$chunked, fraction = 0.5)
chunker = chunker(training_and_testing$training)
validated = validate(chunker, training_and_testing$testing)
chunked = chunk(chunker, features$unchunked)

test_that("everything", {
  expect_equal(validated, structure(c(1L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 0L, 0L,
                                      0L, 0L, 0L, 0L, 1L, 0L, 0L, 1L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L,
                                      0L, 1L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 0L, 0L, 0L, 0L,
                                      0L, 0L, 0L, 0L, 0L, 1L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L,
                                      0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 0L, 0L, 0L, 0L, 0L, 0L,
                                      0L, 0L, 0L, 0L), .Dim = c(9L, 9L), .Dimnames = structure(list(
                                        c("beginning statement beginning attribute", "inside statement beginning aim",
                                          "inside statement beginning deontic", "inside statement beginning object",
                                          "inside statement inside attribute", "inside statement inside deontic",
                                          "inside statement inside object", "inside statement outside component",
                                          "outside statement outside component"), . = c("beginning statement beginning attribute",
                                                                                        "inside statement beginning aim", "inside statement beginning deontic",
                                                                                        "inside statement beginning object", "inside statement inside attribute",
                                                                                        "inside statement inside deontic", "inside statement inside object",
                                                                                        "inside statement outside component", "outside statement outside component"
                                          )), .Names = c("", ".")), class = "table"))
  expect_equal(chunked, structure(list(document = c(2, 2, 2, 2, 2), statement_ID = c(1L,
                                                                                   1L, 1L, 1L, 1L), component = c("attribute", "deontic", "component",
                                                                                                                  "aim", "object"), text = c("Chemical plants", "must not", "ever",
                                                                                                                                             "pollute", "the soil")), class = c("tbl_df", "tbl", "data.frame"
                                                                                                                                             ), row.names = c(NA, -5L), .Names = c("document", "statement_ID",
                                                                                                                                                                                   "component", "text")))
})

