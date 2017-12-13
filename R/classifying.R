utils::globalVariables(c(
  ":=", "statement_ID", "component", "start", "end", "word", ".", "type",
  "features", "inside", "text_ID", "statement_tag", "part_of_speech", "tag",
  "predict", "component_position", "statement_position",
  "document", "text", "component_tag", "chunked", "word_original"))

lags_and_leads = function(df, name, window = 0) {
  name = enquo(name)
  if (window > 0) {
    for (i in 1:window) {
      lag_name = paste(quo_name(name), "lag", i, sep = "_")
      lead_name = paste(quo_name(name), "lead", i, sep = "_")
      df =
        df %>%
        mutate(
          !!lag_name := lag(!!name, i, default = "<START>"),
          !!lead_name := lead(!!name, i, default = "<END>")
        )
    }
  }
  df
}

parts_of_speech = function(chunked, unchunked) {

    # POS tagging is much faster if all text is put in at once
    # Put together marking separation with unicode character for record seperator
    # then take it apart again and reconstitute texts by counting record seperators
  all_texts =
    bind_rows(
      chunked %>% select(text),
      unchunked %>% select(text)) %>%
    .$text %>%
    paste(collapse = " \u241E ")

  all_texts %>%
    NLP::annotate(list(
      openNLP::Maxent_Sent_Token_Annotator(),
      openNLP::Maxent_Word_Token_Annotator(),
      openNLP::Maxent_POS_Tag_Annotator())) %>%
    as.data.frame %>%
    filter(type != "sentence") %>%
    mutate(word =
             all_texts %>%
             stringi::stri_sub(start, end),
           text_ID =
             (word == "\u241E") %>%
             cumsum %>%
             {. + 1}) %>%
    filter(word != "\u241E") %>%
    rowwise %>%
    mutate(part_of_speech = features$POS) %>%
    ungroup %>%
    select(text_ID, word, part_of_speech)
}

#' Create tags
#'
#' Tag words based on their part of speech and position in the institutional
#' grammar
#'
#' @import dplyr
#'
#' @param chunked A data frame with a text variable.
#' @param unchunked A data frame with a text variable.
#'
#' @examples
#' chunked = data.frame(
#'   document = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
#'   text = c("Power plants", "must not", "ever", "pollute", "the air",
#'             "and also",
#'             "sewage plants", "must not", "ever", "pollute", "the water"),
#'   component = c("attribute", "deontic", NA, "aim", "object",
#'                 NA,
#'                 "attribute", "deontic", NA, "aim", "object"),
#'   statement_ID = c(1, 1, 1, 1, 1,
#'                    2,
#'                    3, 3, 3, 3, 3)
#' )
#' unchunked = data.frame(
#'   document = 2,
#'   text = "Chemical plants must not ever pollute the soil"
#' )
#' tag(chunked, unchunked)
#'
#' @export
tag = function(chunked, unchunked) {
  texts =
    bind_rows(
      chunked %>% mutate(chunked = TRUE),
      unchunked %>% mutate(chunked = FALSE)
    ) %>%
    mutate(text_ID = 1:n())

  tags =
    texts %>%
    group_by(document, statement_ID) %>%
    # inside of a statement if it has any components
    summarize(inside =
                component %>%
                is.na %>%
                all %>%
                `!`) %>%
    ungroup %>%
    right_join(texts, by = c("document", "statement_ID")) %>%
    right_join(parts_of_speech(chunked, unchunked), by = "text_ID") %>%
    group_by(text_ID) %>%
    mutate(component_tag =
             is.na(component) %>%
             ifelse(
               "outside component",
               paste(
                 (1:n() == 1) %>%
                   ifelse("beginning",
                          "inside"),
                 component))) %>%
    ungroup %>%
    group_by(chunked, document, statement_ID) %>%
    mutate(statement_tag =
             inside %>%
             ifelse(
               ((1:n() == 1)) %>%
                 ifelse("beginning statement",
                        "inside statement"),
               "outside statement")) %>%
    ungroup %>%
    mutate(tag = paste(statement_tag, component_tag) %>% as.factor) %>%
    select(chunked, document, word, part_of_speech, tag)
}

#' Create features
#'
#' Create features for chunking from tagged data
#'
#' @import dplyr
#'
#' @param tags Tagged data, the result of tag.
#' @param number_of_words The number of distinct words to use for chunking
#' @param word_window The number of words before and after each word to use for
#'     chunking
#' @param part_of_speech_window The number of parts of speech before and after
#'     each word to use for chunking
#'
#' @examples
#' chunked = data.frame(
#'   document = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
#'   text = c("Power plants", "must not", "ever", "pollute", "the air",
#'             "and also",
#'             "sewage plants", "must not", "ever", "pollute", "the water"),
#'   component = c("attribute", "deontic", NA, "aim", "object",
#'                 NA,
#'                 "attribute", "deontic", NA, "aim", "object"),
#'   statement_ID = c(1, 1, 1, 1, 1,
#'                    2,
#'                    3, 3, 3, 3, 3)
#' )
#' unchunked = data.frame(
#'   document = 2,
#'   text = "Chemical plants must not ever pollute the soil"
#' )
#' tags = tag(chunked, unchunked)
#' features(tags)
#'
#' @export
features = function(tags,
                    number_of_words = 10,
                    part_of_speech_window = 20,
                    word_window = 5) {

  words =
    tags %>%
    rename(word_original = word) %>%
    mutate(word =
             word_original %>%
             forcats::fct_lump(n = number_of_words, other_level = "<OTHER>") %>%
             as.character) %>%
    group_by(chunked, document) %>%
    lags_and_leads(word, word_window) %>%
    lags_and_leads(part_of_speech, part_of_speech_window) %>%
    ungroup %>%
    mutate_all(as.factor) %>%
    { .[, sapply(., nlevels) > 1 ] } %>%
    mutate(chunked = as.logical(chunked))

  list(
    chunked =
      words %>%
      filter(chunked) %>%
      select(-chunked),
    unchunked =
      words %>%
      filter(!chunked) %>%
      select(-chunked)
  )
}

#' Balance data
#'
#' Will discard tags with below the minimum number of observations, and sample
#' down the rest of the tags to have the same number of observations.
#'
#' @param data A data frame with a tag column
#' @param minimum_number A minimum number of observations per tag
#'
#' @examples
#' chunked = data.frame(
#'   document = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
#'   text = c("Power plants", "must not", "ever", "pollute", "the air",
#'             "and also",
#'             "sewage plants", "must not", "ever", "pollute", "the water"),
#'   component = c("attribute", "deontic", NA, "aim", "object",
#'                 NA,
#'                 "attribute", "deontic", NA, "aim", "object"),
#'   statement_ID = c(1, 1, 1, 1, 1,
#'                    2,
#'                    3, 3, 3, 3, 3)
#' )
#' unchunked = data.frame(
#'   document = 2,
#'   text = "Chemical plants must not ever pollute the soil"
#' )
#' tags = tag(chunked, unchunked)
#' my_features = features(tags)
#' balance(my_features$chunked, minimum_number = 0)
#'
#' @export
balance = function(data, minimum_number = 100) {
  counts =
    data %>%
    group_by(tag) %>%
    count %>%
    filter(n >= minimum_number)

  counts %>%
  left_join(data, by = "tag") %>%
  group_by(tag) %>%
  sample_n(min(counts$n)) %>%
  ungroup %>%
  select(-n) %>%
  mutate(tag = factor(tag))
}

#' Split data into training and testing data
#'
#' Will return a list with a training and a testing data set
#'
#' @param data A data frame
#' @param fraction The percentage of the data to put into the training data
#'
#' @examples
#' chunked = data.frame(
#'   document = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
#'   text = c("Power plants", "must not", "ever", "pollute", "the air",
#'             "and also",
#'             "sewage plants", "must not", "ever", "pollute", "the water"),
#'   component = c("attribute", "deontic", NA, "aim", "object",
#'                 NA,
#'                 "attribute", "deontic", NA, "aim", "object"),
#'   statement_ID = c(1, 1, 1, 1, 1,
#'                    2,
#'                    3, 3, 3, 3, 3)
#' )
#' unchunked = data.frame(
#'   document = 2,
#'   text = "Chemical plants must not ever pollute the soil"
#' )
#' tags = tag(chunked, unchunked)
#' my_features = features(tags)
#' training_and_testing(my_features$chunked)
#'
#' @export
training_and_testing = function(data, fraction = 0.5) {
  training_rows =
    data %>%
    select(tag, document) %>%
    mutate(row = 1:n()) %>%
    group_by(tag, document) %>%
    sample_frac(fraction) %>%
    .$row

  list(
    training = slice(data, training_rows),
    testing = slice(data, -training_rows)
  )
}

#' Build an chunker
#'
#' Build an chunker based on random forest to chunk text with
#'     institutional grammar
#'
#' @param features_chunked Features of chunked text
#' @param classifier Defaults to randomForest
#' @param ... Extra parameters to pass to randomForest
#'
#' @examples
#' chunked = data.frame(
#'   document = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
#'   text = c("Power plants", "must not", "ever", "pollute", "the air",
#'             "and also",
#'             "sewage plants", "must not", "ever", "pollute", "the water"),
#'   component = c("attribute", "deontic", NA, "aim", "object",
#'                 NA,
#'                 "attribute", "deontic", NA, "aim", "object"),
#'   statement_ID = c(1, 1, 1, 1, 1,
#'                    2,
#'                    3, 3, 3, 3, 3)
#' )
#' unchunked = data.frame(
#'   document = 2,
#'   text = "Chemical plants must not ever pollute the soil"
#' )
#' tags = tag(chunked, unchunked)
#' my_features = features(tags)
#' chunker(my_features$chunked)
#'
#' @export
chunker = function(features_chunked, classifier = randomForest::randomForest, ...)
  classifier(tag ~ .,
             data =
               features_chunked %>%
               select(-document, -word_original),
             ...)

#' Validate a chunker
#'
#' Will return a two way table to assess tagging accuracy. Actual tags will be
#' rows, and predicted tags will be columns.
#'
#' @param chunker a chunker
#' @param testing testing data features
#'
#' @examples
#' chunked = data.frame(
#'   document = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
#'   text = c("Power plants", "must not", "ever", "pollute", "the air",
#'             "and also",
#'             "sewage plants", "must not", "ever", "pollute", "the water"),
#'   component = c("attribute", "deontic", NA, "aim", "object",
#'                 NA,
#'                 "attribute", "deontic", NA, "aim", "object"),
#'   statement_ID = c(1, 1, 1, 1, 1,
#'                    2,
#'                    3, 3, 3, 3, 3)
#' )
#' unchunked = data.frame(
#'   document = 2,
#'   text = "Chemical plants must not ever pollute the soil"
#' )
#' tags = tag(chunked, unchunked)
#' my_features = features(tags)
#' my_training_and_testing = training_and_testing(my_features$chunked)
#' my_chunker = chunker(my_training_and_testing$training)
#' validate(my_chunker, my_training_and_testing$testing)
#'
#' @export
validate = function(chunker, testing) {
  stats::predict(chunker, newdata = testing) %>%
    table(testing$tag, .)
}

position_IDs = function(vector)
  cumsum(
    vector == "beginning" |
      (vector == "outside" &
         lag(vector, default = "inside") != "outside"))

#' Chunk text
#'
#' Chunk text with a chunker and unchunked features.
#'
#' @param chunker A chunker
#' @param features_unchunked Unchunked text features
#'
#' @examples
#' chunked = data.frame(
#'   document = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
#'   text = c("Power plants", "must not", "ever", "pollute", "the air",
#'             "and also",
#'             "sewage plants", "must not", "ever", "pollute", "the water"),
#'   component = c("attribute", "deontic", NA, "aim", "object",
#'                 NA,
#'                 "attribute", "deontic", NA, "aim", "object"),
#'   statement_ID = c(1, 1, 1, 1, 1,
#'                    2,
#'                    3, 3, 3, 3, 3)
#' )
#' unchunked = data.frame(
#'   document = 2,
#'   text = "Chemical plants must not ever pollute the soil"
#' )
#' tags = tag(chunked, unchunked)
#' my_features = features(tags, word_window = 3, number_of_words = 3, part_of_speech_window = 3)
#' my_chunker = chunker(my_features$chunked)
#'
#' @export
chunk = function(chunker, features_unchunked)
  features_unchunked %>%
  mutate(tag =
           stats::predict(chunker,
                          newdata = features_unchunked %>% select(-document))) %>%
  select(document, word = word_original, tag) %>%
  tidyr::separate(tag, c("statement_position", "statement",
                         "component_position", "component")) %>%
  group_by(document) %>%
  mutate(statement_ID = position_IDs(statement_position)) %>%
  group_by(document, statement_ID) %>%
  mutate(text_ID = position_IDs(component_position)) %>%
  group_by(document, statement_ID, text_ID) %>%
  summarize(
    component = first(component),
    text = paste(word, collapse = " ")) %>%
  ungroup %>%
  select(-text_ID)
