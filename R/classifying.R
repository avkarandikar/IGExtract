utils::globalVariables(c(
  ":=", "statement_ID", "component", "start", "end", "word", ".", "type",
  "features", "inside", "text_ID", "statement_tag", "part_of_speech", "tag",
  "predict", "component_position", "statement_position", "word_original",
  "document", "text"))

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

#' Create parts of speech
#'
#' Chunked and unchunked text must be included together.
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
#' parts_of_speech(chunked, unchunked)
#'
#' @export
parts_of_speech = function(chunked, unchunked) {
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
    mutate(word_original =
             all_texts %>%
             stringi::stri_sub(start, end),
           text_ID =
             (word_original == "\u241E") %>%
             cumsum %>%
             {. + 1}) %>%
    filter(word_original != "\u241E") %>%
    rowwise %>%
    mutate(part_of_speech = features$POS) %>%
    ungroup %>%
    select(text_ID, word_original, part_of_speech)
}

#' Create features for chunking
#'
#' Chunked and unchunked text must be included together. Will return a list
#' with features for both.
#'
#' @import dplyr
#'
#' @param chunked A data frame of chunked text. Include four columns: document (a
#'     document text ID), component (one of ABDICO, if applicable), text (text
#'     of the fragment), and a statement_ID for each statement and segment
#'     of text between statements.
#' @param unchunked A data frame of unchunked text. Include two columns:
#'     document (a document text ID) and text (the contexts of the text).
#' @param my_parts_of_speech Created with the parts_of_speech function
#' @param number_of_words The number of distinct words to use for chunking
#' @param window The number of words before and after each word to use for
#'     chunking
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
#' my_parts_of_speech = parts_of_speech(chunked, unchunked)
#' features(chunked, unchunked, my_parts_of_speech,
#'          number_of_words = 3, window = 3)
#'
#' @export
features = function(chunked, unchunked, my_parts_of_speech,
                    number_of_words = 50, window = 10) {

  texts =
    bind_rows(
      chunked %>% mutate(chunked = TRUE),
      unchunked %>% mutate(chunked = FALSE)
    ) %>%
    mutate(text_ID = 1:n())

  words =
    texts %>%
    group_by(document, statement_ID) %>%
    summarize(inside =
                component %>%
                is.na %>%
                all %>%
                `!`) %>%
    ungroup %>%
    right_join(texts, by = c("document", "statement_ID")) %>%
    right_join(my_parts_of_speech, by = "text_ID") %>%
    mutate(word =
             word_original %>%
             forcats::fct_lump(n = number_of_words, other_level = "<OTHER>") %>%
             as.character) %>%
    group_by(chunked, document, statement_ID) %>%
    mutate(statement_tag =
             inside %>%
             ifelse(
               ((1:n() == 1)) %>%
                 ifelse("beginning statement", "inside statement"),
               "outside statement"
             )
    ) %>%
    group_by(text_ID) %>%
    mutate(tag =
             is.na(component) %>%
             ifelse(
               "outside component",
               paste(
                 (1:n() == 1) %>%
                   ifelse("beginning", "inside"),
                 component)) %>%
             paste(statement_tag, .)) %>%
    ungroup %>%
    select(chunked, document, word, word_original, part_of_speech, tag) %>%
    group_by(chunked, document) %>%
    lags_and_leads(word, window) %>%
    lags_and_leads(part_of_speech, window) %>%
    ungroup

  words.dummies =
    words %>%
    select(-tag, -document, -chunked, -word_original) %>%
    as.data.frame %>%
    dummies::dummy.data.frame(dummy.classes = "ALL")

  new_names = make.names(names(words.dummies))

  together =
    words.dummies %>%
    stats::setNames(new_names) %>%
    .[!duplicated(new_names)] %>%
    mutate(chunked = words$chunked)

  list(
    chunked =
      together %>%
      mutate(tag = as.factor(words$tag)) %>%
      filter(chunked) %>%
      select(-chunked),
    unchunked =
      together %>%
      mutate(word_original = words$word_original,
             document = words$document) %>%
      filter(!chunked) %>%
      select(-chunked)
  )
}


#' Split data into training and testing data
#'
#' Will return a list with a training and a testing data
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
#' my_parts_of_speech = parts_of_speech(chunked, unchunked)
#' my_features = features(chunked, unchunked, my_parts_of_speech,
#'                        number_of_words = 3, window = 3)
#' training_and_testing(my_features$chunked, fraction = 0.5)
#'
#' @export
training_and_testing = function(data, fraction = 0.5) {
  training_rows =
    data %>%
    select(tag) %>%
    mutate(row = 1:n()) %>%
    group_by(tag) %>%
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
#' my_parts_of_speech = parts_of_speech(chunked, unchunked)
#' my_features = features(chunked, unchunked, my_parts_of_speech,
#'                        number_of_words = 3, window = 3)
#' chunker(my_features$chunked, ntree = 400)
#'
#' @export
chunker = function(features_chunked, ...)
  randomForest::randomForest(tag ~ ., data = features_chunked, ...)

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
#' my_parts_of_speech = parts_of_speech(chunked, unchunked)
#' my_features = features(chunked, unchunked, my_parts_of_speech,
#'                        number_of_words = 3, window = 3)
#' my_training_and_testing = training_and_testing(my_features$chunked, fraction = 0.5)
#' my_chunker = chunker(my_training_and_testing$training)
#' validate(my_chunker, my_training_and_testing$testing)
#'
#' @export
validate = function(chunker, testing) {
  stats::predict(chunker, newdata = testing) %>%
    table(testing$tag, .)
}

beginnings_to_tags = function(vector)
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
#' my_parts_of_speech = parts_of_speech(chunked, unchunked)
#' my_features = features(chunked, unchunked, my_parts_of_speech,
#'                        number_of_words = 3, window = 3)
#' my_chunker = chunker(my_features$chunked)
#' chunk(my_chunker, my_features$unchunked)
#'
#' @export
chunk = function(chunker, features_unchunked)
  features_unchunked %>%
  mutate(tag =
           stats::predict(chunker,
                          newdata = features_unchunked)) %>%
  select(document, word_original, tag) %>%
  tidyr::separate(tag, c("statement_position", "statement",
                         "component_position", "component")) %>%
  group_by(document) %>%
  mutate(statement_ID = beginnings_to_tags(statement_position)) %>%
  group_by(document, statement_ID) %>%
  mutate(text_ID = beginnings_to_tags(component_position)) %>%
  group_by(document, statement_ID, text_ID) %>%
  summarize(
    component = first(component),
    text = paste(word_original, collapse = " ")) %>%
  ungroup %>%
  select(-text_ID)
