# ─────────────────────────────────────────────────────────────────────────────
# textR.R  —  NLP / Text Analysis module
#
# Required : tidytext, textdata
# ─────────────────────────────────────────────────────────────────────────────

# ── Module UI ──────────────────────────────────────────────────────────────────
textRUI <- function(id) {
  ns <- NS(id)
  tagList(
    br(),
    uiOutput(ns("tab_guard")),
    fluidRow(
      column(3,
        selectInput(ns("text_col"), "Select text column:", choices = NULL)
      ),
      column(2,
        br(),
        actionButton(ns("analyze_btn"), "Analyze Text", class = "btn-primary")
      )
    ),
    br(),
    uiOutput(ns("text_results"))
  )
}

# ── Module Server ──────────────────────────────────────────────────────────────
textRServer <- function(id, dataset, reset_trigger, report_rv = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    show_text <- reactiveVal(FALSE)
    text_data <- reactiveVal(NULL)

    # ── No-data guard ──────────────────────────────────────────────────
    output$tab_guard <- renderUI({
      if (!is.null(dataset())) return(NULL)
      .no_data_ui()
    })

    observe({
      req(dataset())
      df       <- dataset()
      txt_cols <- names(df)[sapply(df, function(x) is.character(x) || is.factor(x))]
      updateSelectInput(session, "text_col",
                        choices  = txt_cols,
                        selected = if (length(txt_cols) > 0) txt_cols[1] else NULL)
    })

    observe({
      has_data <- !is.null(dataset())
      if (!has_data) { shinyjs::disable("analyze_btn"); return() }
      df       <- dataset()
      txt_cols <- names(df)[sapply(df, function(x) is.character(x) || is.factor(x))]
      if (length(txt_cols) == 0) shinyjs::disable("analyze_btn")
      else                       shinyjs::enable("analyze_btn")
    })

    observeEvent(reset_trigger(), {
      show_text(FALSE)
      text_data(NULL)
      updateSelectInput(session, "text_col", choices = character(0), selected = NULL)
    })

    # ── Analyze button ─────────────────────────────────────────────────
    observeEvent(input$analyze_btn, {
      req(dataset(), input$text_col)
      df  <- dataset()
      col <- input$text_col

      if (!col %in% names(df)) {
        showNotification("Selected column not found.", type = "error", duration = 4)
        return()
      }

      texts <- as.character(df[[col]])
      texts <- texts[!is.na(texts) & nchar(trimws(texts)) > 0]

      if (length(texts) == 0) {
        showNotification("No non-empty text values in selected column.", type = "warning", duration = 4)
        return()
      }

      n_orig   <- length(texts)
      text_cap <- 50000L
      if (n_orig > text_cap) {
        set.seed(42L)
        texts <- texts[sample(n_orig, text_cap)]
        showNotification(
          paste0("Large dataset: analyzing a random sample of ",
                 format(text_cap, big.mark = ","), " rows out of ",
                 format(n_orig, big.mark = ","), " (seed = 42)."),
          type = "message", duration = 7
        )
      }
      n_used <- length(texts)

      text_data(list(texts = texts, col = col, n_orig = n_orig, n_used = n_used))
      show_text(TRUE)
    })

    # ── Results scaffold ───────────────────────────────────────────────
    # NOTE: Word Cloud tab uses inline conditional content — do NOT pre-assign
    # tabPanel to a variable and pass it to tabsetPanel, as this causes Shiny
    # to mis-serialize the tag and display "[object Object]" in the browser.
    output$text_results <- renderUI({
      if (!show_text()) {
        return(div(style = "color:#555; margin-top:60px; text-align:center; font-size:15px;",
                   "Select a text column and click \u201cAnalyze Text\u201d."))
      }
      tabsetPanel(
        tabPanel("Overview",
          br(),
          withSpinner(DTOutput(ns("overview_table")))
        ),
        tabPanel("Word Frequency",
          br(),
          withSpinner(plotOutput(ns("freq_plot"), height = "420px")),
          br(),
          withSpinner(DTOutput(ns("freq_table")))
        ),
        tabPanel("N-grams",
          br(),
          withSpinner(plotOutput(ns("ngram_plot"), height = "420px")),
          br(),
          withSpinner(DTOutput(ns("ngram_table")))
        ),
        tabPanel("Sentiment",
          br(),
          uiOutput(ns("sentiment_summary")),
          br(),
          withSpinner(plotOutput(ns("sentiment_plot"), height = "420px")),
          br(),
          withSpinner(DTOutput(ns("sentiment_table")))
        ),
      )
    })

    # ── Overview ──────────────────────────────────────────────────────
    output$overview_table <- renderDT({
      req(text_data())
      td       <- text_data()
      texts    <- td$texts
      n_orig   <- td$n_orig %||% length(texts)
      n_used   <- td$n_used %||% length(texts)
      all_text <- paste(texts, collapse = " ")

      words_raw    <- unlist(strsplit(tolower(all_text), "\\s+"))
      words_raw    <- words_raw[nchar(words_raw) > 0]
      word_count   <- length(words_raw)
      unique_words <- length(unique(words_raw))

      sents      <- unlist(strsplit(all_text, "[.!?]+"))
      sents      <- sents[nchar(trimws(sents)) > 0]
      sent_count <- length(sents)

      lex_div <- if (word_count > 0) round(unique_words / word_count, 4) else 0
      avg_wl  <- if (length(words_raw) > 0) round(mean(nchar(words_raw)), 2) else 0

      stop_w      <- tidytext::stop_words$word
      clean_words <- words_raw[!words_raw %in% stop_w & nchar(words_raw) > 1]
      most_common <- if (length(clean_words) > 0) {
        names(sort(table(clean_words), decreasing = TRUE))[1]
      } else "\u2014"

      base_metrics <- c("Total Characters", "Total Words", "Sentences",
                        "Unique Words", "Lexical Diversity", "Avg Word Length (chars)",
                        "Most Common Word (excl. stopwords)")
      base_values  <- c(
        format(nchar(all_text), big.mark = ","),
        format(word_count,      big.mark = ","),
        format(sent_count,      big.mark = ","),
        format(unique_words,    big.mark = ","),
        as.character(lex_div),
        as.character(avg_wl),
        most_common
      )

      if (n_used < n_orig) {
        base_metrics <- c("Rows in dataset", "Rows analyzed (sample, seed=42)", base_metrics)
        base_values  <- c(format(n_orig, big.mark = ","), format(n_used, big.mark = ","),
                          base_values)
      }

      df <- data.frame(Metric = base_metrics, Value = base_values, stringsAsFactors = FALSE)
      datatable(df, rownames = FALSE,
        options = list(pageLength = 10, dom = "t", scrollX = TRUE),
        class = "display compact cell-border")
    })

    # ── Word Frequency ─────────────────────────────────────────────────
    freq_df <- reactive({
      req(text_data())
      texts  <- text_data()$texts
      raw_df <- data.frame(text = texts, stringsAsFactors = FALSE)
      tokens <- tidytext::unnest_tokens(raw_df, word, text)
      tokens <- dplyr::anti_join(tokens, tidytext::stop_words, by = "word")
      tokens <- tokens[nchar(tokens$word) > 1, , drop = FALSE]
      wf     <- dplyr::count(tokens, word, sort = TRUE)
      wf$pct <- round(100 * wf$n / sum(wf$n), 2)
      wf
    })

    freq_plot_obj <- reactive({
      req(text_data())
      df <- freq_df()
      validate(need(nrow(df) > 0, "No words remain after removing stopwords."))
      df       <- head(df, 30)
      df$word  <- factor(df$word, levels = rev(df$word))
      ggplot2::ggplot(df, ggplot2::aes(x = word, y = n)) +
        ggplot2::geom_bar(stat = "identity", fill = "#00ff00", alpha = 0.85, color = "black") +
        ggplot2::coord_flip() +
        .dark_theme() +
        ggplot2::labs(title = "Top 30 Words (stopwords removed)", x = NULL, y = "Count")
    })

    output$freq_plot <- renderPlot({ req(show_text()); freq_plot_obj() })

    output$freq_table <- renderDT({
      req(show_text(), text_data())
      df <- freq_df()
      validate(need(nrow(df) > 0, "No words found."))
      names(df) <- c("Word", "Count", "Pct%")
      datatable(df, rownames = FALSE,
        options = list(pageLength = 15, scrollX = TRUE, dom = "ftp"),
        class = "display compact cell-border stripe hover")
    })

    # ── N-grams (Bigrams) ─────────────────────────────────────────────
    ngram_df <- reactive({
      req(text_data())
      texts   <- text_data()$texts
      raw_df  <- data.frame(text = texts, stringsAsFactors = FALSE)
      bigrams <- tidytext::unnest_tokens(raw_df, bigram, text, token = "ngrams", n = 2)
      stop_w  <- tidytext::stop_words$word
      bg      <- tidyr::separate(bigrams, col = "bigram",
                                 into = c("word1", "word2"), sep = " ")
      bg <- bg[
        !is.na(bg$word1) & !is.na(bg$word2) &
        !bg$word1 %in% stop_w & !bg$word2 %in% stop_w &
        nchar(bg$word1) > 1 & nchar(bg$word2) > 1, ]
      bg$bigram <- paste(bg$word1, bg$word2)
      bf        <- dplyr::count(bg, bigram, sort = TRUE)
      bf$pct    <- round(100 * bf$n / sum(bf$n), 2)
      bf
    })

    ngram_plot_obj <- reactive({
      req(text_data())
      df <- ngram_df()
      validate(need(nrow(df) > 0, "No bigrams found after removing stopwords."))
      df        <- head(df, 20)
      df$bigram <- factor(df$bigram, levels = rev(df$bigram))
      ggplot2::ggplot(df, ggplot2::aes(x = bigram, y = n)) +
        ggplot2::geom_bar(stat = "identity", fill = "#00bfff", alpha = 0.85, color = "black") +
        ggplot2::coord_flip() +
        .dark_theme() +
        ggplot2::labs(title = "Top 20 Bigrams (stopwords removed)", x = NULL, y = "Count")
    })

    output$ngram_plot  <- renderPlot({ req(show_text()); ngram_plot_obj() })

    output$ngram_table <- renderDT({
      req(show_text(), text_data())
      df <- ngram_df()
      validate(need(nrow(df) > 0, "No bigrams found."))
      names(df) <- c("Bigram", "Count", "Pct%")
      datatable(df, rownames = FALSE,
        options = list(pageLength = 15, scrollX = TRUE, dom = "ftp"),
        class = "display compact cell-border stripe hover")
    })

    # ── Sentiment ─────────────────────────────────────────────────────
    # Single reactive returns list(err=<msg|NULL>, df=<data.frame|NULL>).
    # No reactiveVal writes — avoids Shiny re-entrancy / side-effect bugs.
    sent_result <- reactive({
      req(text_data())
      tryCatch({
        texts  <- text_data()$texts
        raw_df <- data.frame(text = texts, stringsAsFactors = FALSE)
        tokens <- tidytext::unnest_tokens(raw_df, word, text)
        tokens <- dplyr::anti_join(tokens, tidytext::stop_words, by = "word")

        bing_or_err <- tryCatch(
          list(ok = TRUE, lex = tidytext::get_sentiments("bing")),
          error = function(e) {
            msg <- conditionMessage(e)
            errmsg <- if (grepl("textdata", msg, ignore.case = TRUE))
              "The 'textdata' package is required. Install it: install.packages('textdata')"
            else if (grepl("download|lexicon|dataset", msg, ignore.case = TRUE))
              "Bing lexicon not downloaded. Run textdata::lexicon_bing() in your R console."
            else
              paste0("Could not load Bing lexicon: ", msg)
            list(ok = FALSE, err = errmsg)
          }
        )

        if (!isTRUE(bing_or_err$ok))
          return(list(err = bing_or_err$err, df = NULL))

        result <- dplyr::inner_join(tokens, bing_or_err$lex, by = "word")

        if (nrow(result) == 0)
          return(list(
            err = "No sentiment words matched. Check that the column contains English prose.",
            df  = result
          ))

        list(err = NULL, df = result)
      }, error = function(e) {
        list(err = paste0("Sentiment analysis failed: ", conditionMessage(e)), df = NULL)
      })
    })

    output$sentiment_summary <- renderUI({
      req(show_text(), text_data())
      sr  <- sent_result()
      err <- sr$err

      if (!is.null(err)) {
        return(div(style = "color:#ff9944; padding:12px; background:#1a0d00; border-radius:6px;",
                   tags$strong("Sentiment unavailable: "), err))
      }

      df    <- sr$df
      n_pos <- sum(df$sentiment == "positive")
      n_neg <- sum(df$sentiment == "negative")
      net   <- n_pos - n_neg
      label <- if (net > 0) "Positive" else if (net < 0) "Negative" else "Neutral"
      lclr  <- if (net > 0) "#00ff00" else if (net < 0) "#ff4444" else "#888888"

      card <- function(bg_hex, bdr_hex, txt_clr, label_txt, value_txt) {
        div(style = paste0("background:", bg_hex, "; border:1px solid ", bdr_hex,
                           "; border-radius:6px; padding:12px; text-align:center;"),
          tags$p(style = "color:#888; margin:0; font-size:12px;", label_txt),
          tags$p(style = paste0("color:", txt_clr, "; font-size:22px; font-weight:bold; margin:0;"),
                 value_txt))
      }

      fluidRow(
        column(3, card("#001a00", "#00ff00", "#00ff00", "Positive Words", n_pos)),
        column(3, card("#1a0000", "#ff4444", "#ff4444", "Negative Words", n_neg)),
        column(3, card("#0a0a1a", lclr,      lclr,      "Net Score",      net)),
        column(3, card("#0a0a0a", lclr,      lclr,      "Overall",        label))
      )
    })

    sentiment_plot_obj <- reactive({
      req(text_data())
      sr  <- sent_result()
      validate(
        need(is.null(sr$err), sr$err %||% "Sentiment lexicon unavailable."),
        need(!is.null(sr$df) && nrow(sr$df) > 0, "No sentiment words matched.")
      )
      df      <- sr$df
      pos_df  <- df[df$sentiment == "positive", ]
      neg_df  <- df[df$sentiment == "negative", ]
      top_pos <- head(dplyr::count(pos_df, word, sort = TRUE), 10)
      top_neg <- head(dplyr::count(neg_df, word, sort = TRUE), 10)
      top_pos$sentiment <- "positive"
      top_neg$sentiment <- "negative"
      plot_df           <- rbind(top_pos, top_neg)
      plot_df$word      <- factor(plot_df$word, levels = plot_df$word[order(plot_df$n)])

      ggplot2::ggplot(plot_df, ggplot2::aes(x = word, y = n, fill = sentiment)) +
        ggplot2::geom_bar(stat = "identity", alpha = 0.85, color = "black") +
        ggplot2::scale_fill_manual(values = c("positive" = "#00ff00", "negative" = "#ff4444")) +
        ggplot2::coord_flip() +
        .dark_theme() +
        ggplot2::labs(title = "Top 10 Positive & Negative Words",
                      x = NULL, y = "Count", fill = "Sentiment")
    })

    output$sentiment_plot <- renderPlot({
      req(show_text())
      sr <- sent_result()
      validate(need(is.null(sr$err), sr$err %||% "Sentiment unavailable."))
      sentiment_plot_obj()
    })

    output$sentiment_table <- renderDT({
      req(show_text())
      sr <- sent_result()
      validate(need(is.null(sr$err),           sr$err %||% "Sentiment unavailable."),
               need(!is.null(sr$df) && nrow(sr$df) > 0, "No sentiment words matched."))
      wc <- dplyr::count(sr$df, word, sentiment, sort = TRUE)
      names(wc) <- c("Word", "Sentiment", "Count")
      datatable(wc, rownames = FALSE,
        options = list(pageLength = 15, scrollX = TRUE, dom = "ftp"),
        class = "display compact cell-border stripe hover")
    })

    # ── Push results to report_rv when analysis completes ─────────────
    observeEvent(text_data(), {
      if (is.null(report_rv) || is.null(text_data())) return()

      report_rv$text_col    <- text_data()$col
      report_rv$text_freq   <- tryCatch(freq_df(),   error = function(e) NULL)
      report_rv$text_ngrams <- tryCatch(ngram_df(),  error = function(e) NULL)

      report_rv$text_plots <- list(
        freq      = tryCatch(freq_plot_obj(),     error = function(e) NULL),
        ngrams    = tryCatch(ngram_plot_obj(),    error = function(e) NULL),
        sentiment = tryCatch(sentiment_plot_obj(), error = function(e) NULL)
      )

      sr <- tryCatch(sent_result(), error = function(e) list(err = conditionMessage(e), df = NULL))
      if (is.null(sr$err) && !is.null(sr$df) && nrow(sr$df) > 0) {
        report_rv$text_sentiment <- list(
          n_pos = sum(sr$df$sentiment == "positive"),
          n_neg = sum(sr$df$sentiment == "negative"),
          net   = sum(sr$df$sentiment == "positive") - sum(sr$df$sentiment == "negative"),
          df    = sr$df
        )
      } else {
        report_rv$text_sentiment <- NULL
      }
    }, ignoreNULL = TRUE, ignoreInit = TRUE)

  })
}
