#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(shiny)
  library(ggplot2)
  library(plotly)
  library(DT)
})

find_repo_dir <- function() {
  candidates <- unique(normalizePath(c(".", ".."), mustWork = FALSE))
  has_inputs <- vapply(
    candidates,
    function(path) all(file.exists(file.path(
      path,
      c("quantifiers_combined.csv", "number_combined.csv", "determiners_combined.csv")
    ))),
    logical(1)
  )
  if (!any(has_inputs)) {
    stop("Could not find the repository root containing the three combined CSV files.")
  }
  candidates[which(has_inputs)[1L]]
}

repo_dir <- find_repo_dir()
source(file.path(repo_dir, "combined_visualisations", "R", "combined_data.R"), local = TRUE)

semantics_levels <- c("c", "a", "s")
semantics_colors <- c(
  c = "#0072B2",
  a = "#D55E00",
  s = "#009E73",
  "c + a" = "#7A5AA6",
  "c + s" = "#008C95",
  "a + s" = "#B17829",
  "c + a + s" = "#666666"
)
primary_prefixes <- c("q_many", "number_plural", "det_a")
primary_labels <- c(
  q_many = "many among many/much",
  number_plural = "plural in number data",
  det_a = "a among singular a/the"
)

feature_columns <- function(smoothed = TRUE, transformed = FALSE) {
  suffix <- if (transformed) {
    "_logit_smoothed"
  } else if (smoothed) {
    "_prop_smoothed"
  } else {
    "_prop"
  }
  paste0(primary_prefixes, suffix)
}

feature_long <- function(df, columns) {
  pieces <- lapply(seq_along(columns), function(i) {
    data.frame(
      lemma = df$lemma,
      semantics = df$semantics,
      analysis_period = df$analysis_period,
      feature = factor(primary_labels[[i]], levels = unname(primary_labels)),
      value = df[[columns[[i]]]],
      min_primary_total = df$min_primary_total,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, pieces)
}

semantic_mapping_choices <- c(
  "Exclude" = "exclude",
  "Group 1" = "group_1",
  "Group 2" = "group_2",
  "Group 3" = "group_3"
)

apply_semantic_mapping <- function(df, mapping) {
  df$semantics_original <- as.character(df$semantics)
  mapped <- unname(mapping[df$semantics_original])
  df <- df[!is.na(mapped) & mapped != "exclude", , drop = FALSE]
  mapped <- mapped[!is.na(mapped) & mapped != "exclude"]

  if (nrow(df) == 0L) {
    df$semantics <- factor(character())
    return(df)
  }

  group_order <- unique(unname(mapping[mapping != "exclude"]))
  group_labels <- vapply(group_order, function(group) {
    members <- semantics_levels[unname(mapping[semantics_levels]) == group]
    paste(members, collapse = " + ")
  }, character(1))
  names(group_labels) <- group_order

  df$semantics <- factor(unname(group_labels[mapped]), levels = unname(group_labels))
  df
}

semantic_palette <- function(levels) {
  known <- semantics_colors[levels]
  if (any(is.na(known))) {
    fallback <- grDevices::hcl.colors(length(levels), palette = "Dark 3")
    known[is.na(known)] <- fallback[is.na(known)]
  }
  stats::setNames(unname(known), levels)
}

plot_theme <- function() {
  theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      plot.title.position = "plot"
    )
}

ui <- fluidPage(
  titlePanel("Combined Noun Semantics Visualisations"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "granularity",
        "Period granularity",
        choices = c(
          "All periods combined" = "overall",
          "Main periods" = "period_main",
          "Detailed harmonized periods" = "period_sorted"
        ),
        selected = "period_main"
      ),
      uiOutput("period_ui"),
      tags$hr(),
      tags$strong("Semantic categories"),
      tags$p("Exclude a level or assign multiple levels to the same group to merge them."),
      uiOutput("semantics_mapping_ui"),
      tags$hr(),
      numericInput("min_q", "Minimum many/much observations", value = 3, min = 0, step = 1),
      numericInput("min_number", "Minimum number observations", value = 20, min = 0, step = 1),
      numericInput("min_det", "Minimum singular a/the observations", value = 10, min = 0, step = 1),
      checkboxInput("complete_only", "Require all three primary features", value = TRUE),
      checkboxInput("smoothed", "Use smoothed proportions", value = TRUE),
      checkboxInput("show_labels", "Show lemma labels in PCA", value = FALSE),
      sliderInput("pca_label_max", "Maximum visible PCA labels", min = 5, max = 100, value = 25, step = 5),
      checkboxInput("facet_pca", "Facet PCA by period", value = FALSE),
      tags$hr(),
      tags$strong("Animated PCA"),
      numericInput("animation_min_periods", "Minimum selected periods per lemma", value = 2, min = 1, step = 1),
      checkboxInput("animation_labels", "Show current lemma labels", value = TRUE),
      sliderInput("animation_label_max", "Maximum animated labels", min = 5, max = 100, value = 25, step = 5),
      sliderInput("heatmap_max", "Maximum heatmap rows", min = 20, max = 250, value = 100, step = 10),
      actionButton("refresh_data", "Reload CSV files"),
      tags$hr(),
      downloadButton("download_filtered", "Download filtered aggregate")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Overview",
          tags$p(
            "Primary features: many versus much, plural versus singular in the number dataset, ",
            "and a versus the restricted to determiner rows where noun_pos = N."
          ),
          verbatimTextOutput("summary_text"),
          plotlyOutput("coverage_plot", height = 340),
          DTOutput("aggregate_table")
        ),
        tabPanel(
          "Profiles",
          tags$p("Cell-level distributions by semantics. Point size reflects the smallest primary-feature denominator."),
          plotlyOutput("profile_plot", height = 620)
        ),
        tabPanel(
          "PCA",
          tags$p("PCA uses standardized smoothed logits. Arrows show the direction of increasing feature values."),
          verbatimTextOutput("pca_text"),
          plotlyOutput("pca_plot", height = 650)
        ),
        tabPanel(
          "Animated PCA",
          tags$p(
            "The PCA coordinate system is fitted once across all selected periods. ",
            "Each frame shows the current lemma positions and the cumulative trajectories traveled in earlier selected periods."
          ),
          verbatimTextOutput("animated_pca_text"),
          plotlyOutput("animated_pca_plot", height = 700)
        ),
        tabPanel(
          "Clustered Heatmap",
          tags$p(
            "Each selected period is shown as a separate heatmap. Rows are clustered within semantic groups, ",
            "and colors are standardized across all displayed periods for comparison."
          ),
          uiOutput("heatmap_plots_ui")
        ),
        tabPanel(
          "Semantics Tree",
          tags$p("Exploratory classification tree using the three primary features and period. Validation predictions hold out entire lemmas."),
          verbatimTextOutput("tree_text"),
          plotOutput("tree_plot", height = 600),
          DTOutput("tree_confusion")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  aggregates <- reactiveVal(load_combined_aggregates(repo_dir))

  observeEvent(input$refresh_data, {
    aggregates(load_combined_aggregates(repo_dir))
    showNotification("Reloaded combined CSV files.", type = "message")
  })

  selected_aggregate <- reactive({
    aggregates()[[input$granularity]]
  })

  output$period_ui <- renderUI({
    df <- selected_aggregate()
    choices <- levels(df$analysis_period)
    choices <- choices[choices %in% as.character(unique(df$analysis_period))]
    selectizeInput(
      "periods",
      "Periods",
      choices = choices,
      selected = choices,
      multiple = TRUE
    )
  })

  output$semantics_mapping_ui <- renderUI({
    tagList(
      selectInput("semantic_map_c", "c", choices = semantic_mapping_choices, selected = "group_1"),
      selectInput("semantic_map_a", "a", choices = semantic_mapping_choices, selected = "group_2"),
      selectInput("semantic_map_s", "s", choices = semantic_mapping_choices, selected = "group_3")
    )
  })

  semantic_mapping <- reactive({
    c(
      c = if (is.null(input$semantic_map_c)) "group_1" else input$semantic_map_c,
      a = if (is.null(input$semantic_map_a)) "group_2" else input$semantic_map_a,
      s = if (is.null(input$semantic_map_s)) "group_3" else input$semantic_map_s
    )
  })

  semantics_data <- reactive({
    apply_semantic_mapping(selected_aggregate(), semantic_mapping())
  })

  period_filtered_data <- reactive({
    df <- semantics_data()
    periods <- input$periods
    if (!is.null(periods) && length(periods) > 0L) {
      df <- df[as.character(df$analysis_period) %in% periods, , drop = FALSE]
    }
    droplevels(df)
  })

  filtered_data <- reactive({
    df <- period_filtered_data()
    if (isTRUE(input$complete_only)) {
      df <- df[df$complete_primary, , drop = FALSE]
    }

    df <- df[
      df$q_many_total >= input$min_q &
        df$number_plural_total >= input$min_number &
        df$det_a_total >= input$min_det,
      ,
      drop = FALSE
    ]
    droplevels(df)
  })

  model_data <- reactive({
    df <- filtered_data()
    cols <- feature_columns(transformed = TRUE)
    keep <- stats::complete.cases(df[, cols, drop = FALSE]) & !is.na(df$semantics)
    droplevels(df[keep, , drop = FALSE])
  })

  output$summary_text <- renderText({
    period_df <- period_filtered_data()
    df <- filtered_data()
    sprintf(
      paste(
        "Available cells in selected periods: %d",
        "Complete primary cells before thresholds: %d",
        "Filtered cells: %d",
        "Filtered lemmas: %d",
        "Semantics in filtered data: %s",
        sep = "\n"
      ),
      nrow(period_df),
      sum(period_df$complete_primary),
      nrow(df),
      length(unique(df$lemma)),
      paste(names(table(df$semantics)), as.integer(table(df$semantics)), sep = "=", collapse = ", ")
    )
  })

  output$coverage_plot <- renderPlotly({
    df <- semantics_data()
    validate(need(nrow(df) > 0L, "No semantic categories are selected."))
    df$coverage <- paste0(
      ifelse(df$has_q_many, "Q", "-"),
      ifelse(df$has_number_plural, "N", "-"),
      ifelse(df$has_det_a, "D", "-")
    )
    counts <- as.data.frame(table(df$coverage), stringsAsFactors = FALSE)
    names(counts) <- c("coverage", "cells")
    counts$coverage <- factor(counts$coverage, levels = counts$coverage[order(counts$cells)])

    p <- ggplot(counts, aes(x = coverage, y = cells, text = paste("Coverage:", coverage, "<br>Cells:", cells))) +
      geom_col(fill = "#4C78A8") +
      coord_flip() +
      labs(
        title = "Feature coverage before filtering",
        subtitle = "Q = many/much, N = number plurality, D = singular a/the",
        x = NULL,
        y = "Lemma-period cells"
      ) +
      plot_theme()
    ggplotly(p, tooltip = "text")
  })

  output$aggregate_table <- renderDT({
    df <- filtered_data()
    shown <- df[c(
      "lemma", "semantics_original", "semantics", "analysis_period",
      "q_many_n", "q_many_total", "q_many_prop", "q_many_prop_smoothed",
      "number_plural_n", "number_plural_total", "number_plural_prop", "number_plural_prop_smoothed",
      "det_a_n", "det_a_total", "det_a_prop", "det_a_prop_smoothed",
      "q_plural_prop", "det_plural_prop"
    )]
    datatable(shown, options = list(pageLength = 15, scrollX = TRUE), rownames = FALSE) |>
      formatRound(columns = grep("_prop", names(shown), value = TRUE), digits = 3)
  })

  output$profile_plot <- renderPlotly({
    df <- filtered_data()
    validate(need(nrow(df) > 0L, "No cells satisfy the current filters."))
    cols <- feature_columns(smoothed = isTRUE(input$smoothed))
    long <- feature_long(df, cols)
    long$hover <- paste0(
      "Lemma: ", long$lemma,
      "<br>Semantics: ", long$semantics,
      "<br>Period: ", long$analysis_period,
      "<br>Value: ", sprintf("%.3f", long$value),
      "<br>Minimum denominator: ", long$min_primary_total
    )

    p <- ggplot(long, aes(x = semantics, y = value, color = semantics)) +
      geom_boxplot(outlier.shape = NA, alpha = 0.15) +
      geom_jitter(
        aes(size = min_primary_total, text = hover),
        width = 0.16,
        alpha = 0.65
      ) +
      facet_wrap(~feature, ncol = 1) +
      scale_color_manual(values = semantic_palette(levels(long$semantics)), drop = FALSE) +
      scale_size_continuous(range = c(1.5, 7), guide = "none") +
      coord_cartesian(ylim = c(0, 1)) +
      labs(x = "Semantics", y = "Proportion", color = "Semantics") +
      plot_theme()
    ggplotly(p, tooltip = "text")
  })

  pca_result <- reactive({
    df <- model_data()
    cols <- feature_columns(transformed = TRUE)
    validate(need(nrow(df) >= 4L, "At least four complete cells are required for PCA."))
    validate(need(all(vapply(df[cols], stats::sd, numeric(1), na.rm = TRUE) > 0), "All PCA features must vary."))
    fit <- stats::prcomp(df[cols], center = TRUE, scale. = TRUE)
    list(df = df, fit = fit, cols = cols)
  })

  animated_pca_result <- reactive({
    result <- pca_result()
    scores <- as.data.frame(result$fit$x[, 1:2, drop = FALSE])
    scores$lemma <- result$df$lemma
    scores$semantics <- result$df$semantics
    scores$analysis_period <- factor(
      as.character(result$df$analysis_period),
      levels = levels(result$df$analysis_period),
      ordered = TRUE
    )
    scores$min_primary_total <- result$df$min_primary_total
    scores$period_index <- as.integer(scores$analysis_period)

    selected_periods <- levels(droplevels(scores$analysis_period))
    scores$analysis_period <- factor(
      as.character(scores$analysis_period),
      levels = selected_periods,
      ordered = TRUE
    )
    scores$period_index <- as.integer(scores$analysis_period)

    lemma_period_counts <- table(scores$lemma)
    keep_lemmas <- names(lemma_period_counts)[lemma_period_counts >= input$animation_min_periods]
    scores <- scores[scores$lemma %in% keep_lemmas, , drop = FALSE]
    validate(need(length(selected_periods) >= 2L, "Select at least two periods for animation."))
    validate(need(nrow(scores) > 0L, "No lemmas satisfy the minimum selected-period requirement."))

    score_lemmas <- unique(scores$lemma)
    frame_paths <- lapply(seq_along(selected_periods), function(i) {
      do.call(rbind, lapply(score_lemmas, function(lemma) {
        x <- scores[scores$lemma == lemma & scores$period_index <= i, , drop = FALSE]
        if (nrow(x) == 0L) {
          x <- scores[scores$lemma == lemma, , drop = FALSE][1L, , drop = FALSE]
          x$PC1 <- NA_real_
          x$PC2 <- NA_real_
        }
        x$frame <- selected_periods[[i]]
        x
      }))
    })
    paths <- do.call(rbind, frame_paths)
    paths <- paths[order(paths$frame, paths$lemma, paths$period_index), , drop = FALSE]

    current <- do.call(rbind, lapply(seq_along(selected_periods), function(i) {
      do.call(rbind, lapply(score_lemmas, function(lemma) {
        x <- scores[scores$lemma == lemma & scores$period_index <= i, , drop = FALSE]
        if (nrow(x) == 0L) {
          x <- scores[scores$lemma == lemma, , drop = FALSE][1L, , drop = FALSE]
          x$PC1 <- NA_real_
          x$PC2 <- NA_real_
        } else {
          x <- x[order(x$period_index), , drop = FALSE]
          x <- x[nrow(x), , drop = FALSE]
        }
        x$frame <- selected_periods[[i]]
        x
      }))
    }))
    current$hover <- paste0(
      "Lemma: ", current$lemma,
      "<br>Semantics: ", current$semantics,
      "<br>Last observed period: ", current$analysis_period,
      "<br>Minimum denominator: ", current$min_primary_total
    )

    label_lemmas <- names(sort(
      tapply(scores$min_primary_total, scores$lemma, max, na.rm = TRUE),
      decreasing = TRUE
    ))
    label_lemmas <- head(label_lemmas, input$animation_label_max)
    current$label <- if (isTRUE(input$animation_labels)) {
      ifelse(current$lemma %in% label_lemmas, current$lemma, "")
    } else {
      rep("", nrow(current))
    }

    list(
      base = result,
      scores = scores,
      paths = paths,
      current = current,
      periods = selected_periods,
      lemma_period_counts = lemma_period_counts[keep_lemmas]
    )
  })

  output$pca_text <- renderText({
    result <- pca_result()
    explained <- 100 * result$fit$sdev^2 / sum(result$fit$sdev^2)
    sprintf(
      "Cells used: %d\nLemmas used: %d\nVariance explained: PC1 %.1f%%, PC2 %.1f%%",
      nrow(result$df),
      length(unique(result$df$lemma)),
      explained[[1]],
      explained[[2]]
    )
  })

  output$pca_plot <- renderPlotly({
    result <- pca_result()
    scores <- as.data.frame(result$fit$x[, 1:2, drop = FALSE])
    scores$lemma <- result$df$lemma
    scores$semantics <- result$df$semantics
    scores$analysis_period <- factor(as.character(result$df$analysis_period))
    scores$min_primary_total <- result$df$min_primary_total
    scores$hover <- paste0(
      "Lemma: ", scores$lemma,
      "<br>Semantics: ", scores$semantics,
      "<br>Period: ", scores$analysis_period,
      "<br>Minimum denominator: ", scores$min_primary_total
    )

    loadings <- as.data.frame(result$fit$rotation[, 1:2, drop = FALSE])
    loading_scale <- 0.75 * min(
      diff(range(scores$PC1)) / max(abs(loadings$PC1)),
      diff(range(scores$PC2)) / max(abs(loadings$PC2))
    )
    loadings$PC1 <- loadings$PC1 * loading_scale
    loadings$PC2 <- loadings$PC2 * loading_scale
    loadings$label <- unname(primary_labels)
    loadings$label_x <- loadings$PC1 * 1.12
    loadings$label_y <- loadings$PC2 * 1.12
    loadings$label_y <- loadings$label_y + c(0, -0.08, 0.08) * diff(range(scores$PC2))

    explained <- 100 * result$fit$sdev^2 / sum(result$fit$sdev^2)
    p <- ggplot(
      scores,
      aes(
        x = PC1,
        y = PC2,
        color = semantics,
        size = min_primary_total,
        text = hover
      )
    ) +
      geom_hline(yintercept = 0, color = "grey85") +
      geom_vline(xintercept = 0, color = "grey85") +
      geom_segment(
        data = loadings,
        aes(x = 0, y = 0, xend = PC1, yend = PC2),
        inherit.aes = FALSE,
        arrow = arrow(length = grid::unit(0.18, "cm")),
        color = "grey25"
      ) +
      geom_text(
        data = loadings,
        aes(x = label_x, y = label_y, label = label),
        inherit.aes = FALSE,
        color = "grey15",
        size = 3.5,
        show.legend = FALSE
      ) +
      scale_color_manual(values = semantic_palette(levels(scores$semantics)), drop = FALSE) +
      scale_size_continuous(range = c(2, 9), guide = "none") +
      scale_x_continuous(expand = expansion(mult = 0.12)) +
      scale_y_continuous(expand = expansion(mult = 0.12)) +
      labs(
        x = sprintf("PC1 (%.1f%%)", explained[[1]]),
        y = sprintf("PC2 (%.1f%%)", explained[[2]]),
        color = "Semantics"
      ) +
      plot_theme()

    if (isTRUE(input$facet_pca)) {
      p <- p +
        geom_point(alpha = 0.75) +
        facet_wrap(~analysis_period)
    } else {
      p <- p +
        geom_point(aes(shape = analysis_period), alpha = 0.75) +
        labs(shape = "Period")
    }

    if (isTRUE(input$show_labels)) {
      label_scores <- scores[order(scores$min_primary_total, decreasing = TRUE), , drop = FALSE]
      label_scores <- label_scores[seq_len(min(nrow(label_scores), input$pca_label_max)), , drop = FALSE]
      p <- p + geom_text(
        data = label_scores,
        aes(x = PC1, y = PC2, label = lemma),
        inherit.aes = FALSE,
        color = "grey15",
        nudge_y = 0.08,
        size = 3,
        show.legend = FALSE
      )
    }
    ggplotly(p, tooltip = "text")
  })

  output$animated_pca_text <- renderText({
    result <- animated_pca_result()
    sprintf(
      "Selected periods: %d\nAnimated lemmas: %d\nLemma-period observations: %d",
      length(result$periods),
      length(result$lemma_period_counts),
      nrow(result$scores)
    )
  })

  output$animated_pca_plot <- renderPlotly({
    result <- animated_pca_result()
    scores <- result$scores
    paths <- result$paths
    current <- result$current
    fit <- result$base$fit

    loadings <- as.data.frame(fit$rotation[, 1:2, drop = FALSE])
    loading_scale <- 0.75 * min(
      diff(range(scores$PC1)) / max(abs(loadings$PC1)),
      diff(range(scores$PC2)) / max(abs(loadings$PC2))
    )
    loadings$PC1 <- loadings$PC1 * loading_scale
    loadings$PC2 <- loadings$PC2 * loading_scale
    loadings$label <- unname(primary_labels)

    palette <- semantic_palette(levels(scores$semantics))
    explained <- 100 * fit$sdev^2 / sum(fit$sdev^2)
    x_range <- range(c(scores$PC1, loadings$PC1), na.rm = TRUE)
    y_range <- range(c(scores$PC2, loadings$PC2), na.rm = TRUE)
    x_pad <- diff(x_range) * 0.15
    y_pad <- diff(y_range) * 0.15

    p <- plot_ly(
        data = paths,
        x = ~PC1,
        y = ~PC2,
        frame = ~frame,
        split = ~lemma,
        color = ~semantics,
        colors = palette,
        type = "scatter",
        mode = "lines",
        line = list(width = 1.5),
        opacity = 0.45,
        hoverinfo = "skip",
        showlegend = FALSE
      ) |>
      add_trace(
        data = current,
        x = ~PC1,
        y = ~PC2,
        frame = ~frame,
        color = ~semantics,
        colors = palette,
        text = ~label,
        hovertext = ~hover,
        type = "scatter",
        mode = "markers+text",
        textposition = "top center",
        textfont = list(color = "#333333", size = 11),
        marker = list(size = 11, opacity = 0.85, line = list(color = "white", width = 0.7)),
        hoverinfo = "text",
        showlegend = TRUE
      )

    loading_shapes <- lapply(seq_len(nrow(loadings)), function(i) {
      list(
        type = "line",
        x0 = 0,
        y0 = 0,
        x1 = loadings$PC1[[i]],
        y1 = loadings$PC2[[i]],
        line = list(color = "#444444", width = 1.5)
      )
    })
    loading_annotations <- lapply(seq_len(nrow(loadings)), function(i) {
      list(
        x = loadings$PC1[[i]] * 1.08,
        y = loadings$PC2[[i]] * 1.08,
        text = loadings$label[[i]],
        showarrow = FALSE,
        font = list(color = "#333333", size = 11)
      )
    })

    p |>
      layout(
        xaxis = list(
          title = sprintf("PC1 (%.1f%%)", explained[[1]]),
          range = c(x_range[[1]] - x_pad, x_range[[2]] + x_pad),
          zeroline = TRUE,
          zerolinecolor = "#CCCCCC"
        ),
        yaxis = list(
          title = sprintf("PC2 (%.1f%%)", explained[[2]]),
          range = c(y_range[[1]] - y_pad, y_range[[2]] + y_pad),
          zeroline = TRUE,
          zerolinecolor = "#CCCCCC"
        ),
        legend = list(title = list(text = "Semantics")),
        hovermode = "closest",
        shapes = loading_shapes,
        annotations = loading_annotations
      ) |>
      animation_opts(frame = 900, transition = 500, redraw = TRUE) |>
      animation_slider(currentvalue = list(prefix = "Period: ")) |>
      animation_button(x = 1, xanchor = "right", y = 1.12, yanchor = "top") |>
      config(displaylogo = FALSE)
  })

  heatmap_period_data <- reactive({
    df <- model_data()
    cols <- feature_columns(transformed = TRUE)
    validate(need(nrow(df) >= 2L, "At least two complete cells are required for clustering."))

    matrix_values <- scale(as.matrix(df[cols]))
    fill_limit <- max(abs(matrix_values), na.rm = TRUE)
    df$.heatmap_row_id <- seq_len(nrow(df))
    df$.heatmap_q_many <- matrix_values[, 1L]
    df$.heatmap_number_plural <- matrix_values[, 2L]
    df$.heatmap_det_a <- matrix_values[, 3L]

    periods <- levels(droplevels(df$analysis_period))
    out <- lapply(periods, function(period) {
      period_df <- df[as.character(df$analysis_period) == period, , drop = FALSE]
      if (nrow(period_df) > input$heatmap_max) {
        period_df <- period_df[order(period_df$min_primary_total, decreasing = TRUE), , drop = FALSE]
        period_df <- period_df[seq_len(input$heatmap_max), , drop = FALSE]
      }

      semantic_levels <- levels(droplevels(period_df$semantics))
      row_order <- unlist(lapply(semantic_levels, function(semantic) {
        group_df <- period_df[as.character(period_df$semantics) == semantic, , drop = FALSE]
        if (nrow(group_df) <= 1L) return(group_df$lemma)
        group_matrix <- as.matrix(group_df[c(
          ".heatmap_q_many",
          ".heatmap_number_plural",
          ".heatmap_det_a"
        )])
        group_df$lemma[stats::hclust(stats::dist(group_matrix), method = "ward.D2")$order]
      }), use.names = FALSE)

      heat <- do.call(rbind, lapply(seq_along(primary_prefixes), function(i) {
        data.frame(
          lemma = period_df$lemma,
          semantics = period_df$semantics,
          feature = factor(
            c("many", "plural", "a")[[i]],
            levels = c("many", "plural", "a")
          ),
          value = period_df[[c(
            ".heatmap_q_many",
            ".heatmap_number_plural",
            ".heatmap_det_a"
          )[[i]]]],
          stringsAsFactors = FALSE
        )
      }))
      heat$lemma <- factor(heat$lemma, levels = rev(row_order))
      heat$semantics <- factor(
        as.character(heat$semantics),
        levels = semantic_levels
      )
      list(period = period, data = heat, rows = nrow(period_df), fill_limit = fill_limit)
    })
    names(out) <- periods
    out
  })

  build_period_heatmap <- function(period_result, show_legend = FALSE) {
    heat <- period_result$data
    ggplot(heat, aes(x = feature, y = lemma, fill = value)) +
      geom_tile(color = "white", linewidth = 0.2) +
      scale_fill_gradient2(
        low = "#2166AC",
        mid = "white",
        high = "#B2182B",
        midpoint = 0,
        limits = c(-period_result$fill_limit, period_result$fill_limit)
      ) +
      labs(
        title = period_result$period,
        subtitle = sprintf("%d lemmas", period_result$rows),
        x = NULL,
        y = NULL,
        fill = "Standardized\nsmoothed logit"
      ) +
      facet_grid(semantics ~ ., scales = "free_y", space = "free_y") +
      plot_theme() +
      theme(
        legend.position = if (isTRUE(show_legend)) "bottom" else "none",
        panel.border = element_rect(color = "grey20", fill = NA, linewidth = 0.8),
        strip.background = element_rect(color = "grey20", fill = "grey92", linewidth = 0.8),
        strip.text.y = element_text(angle = 0, face = "bold", size = 9),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        axis.text.y = element_text(size = 8),
        plot.title = element_text(face = "bold", size = 11),
        plot.subtitle = element_text(size = 8),
        panel.spacing.y = grid::unit(0.15, "lines"),
        plot.margin = margin(5.5, 5.5, 5.5, 5.5)
      )
  }

  output$heatmap_plots_ui <- renderUI({
    period_results <- heatmap_period_data()
    validate(need(length(period_results) > 0L, "No periods are available for the heatmap."))
    plot_ids <- paste0("heatmap_period_", seq_along(period_results))
    panel_width <- max(260L, min(420L, 220L + 4L * max(vapply(
      period_results,
      function(x) max(nchar(as.character(x$data$lemma)), na.rm = TRUE),
      numeric(1)
    ))))
    panel_height <- max(360L, min(1000L, 220L + 22L * max(vapply(
      period_results,
      function(x) x$rows,
      numeric(1)
    ))))

    tagList(
      tags$div(
        style = "overflow-x: auto; width: 100%;",
        tags$div(
          style = "display: flex; align-items: flex-start; gap: 12px; width: max-content;",
          lapply(seq_along(plot_ids), function(i) {
            tags$div(
              style = sprintf("width: %dpx; flex: 0 0 %dpx;", panel_width, panel_width),
              plotOutput(plot_ids[[i]], height = panel_height)
            )
          })
        )
      )
    )
  })

  observe({
    period_results <- heatmap_period_data()
    if (length(period_results) == 0L) return(invisible(NULL))
    for (i in seq_along(period_results)) {
      local({
        idx <- i
        plot_id <- paste0("heatmap_period_", idx)
        output[[plot_id]] <- renderPlot({
          results <- heatmap_period_data()
          validate(need(length(results) >= idx, "Heatmap period is no longer available."))
          build_period_heatmap(results[[idx]], show_legend = idx == length(results))
        })
      })
    }
  })

  tree_result <- reactive({
    df <- model_data()
    validate(need(requireNamespace("rpart", quietly = TRUE), "Install the rpart package to use this tab."))
    validate(need(nrow(df) >= 10L, "At least ten complete cells are required for the classification tree."))
    validate(need(nlevels(df$semantics) >= 2L, "At least two semantics categories are required."))

    tree_df <- data.frame(
      lemma = df$lemma,
      semantics = df$semantics,
      analysis_period = df$analysis_period,
      q_many = df$q_many_logit_smoothed,
      number_plural = df$number_plural_logit_smoothed,
      det_a = df$det_a_logit_smoothed
    )
    tree_formula <- if (length(unique(tree_df$analysis_period)) > 1L) {
      semantics ~ q_many + number_plural + det_a + analysis_period
    } else {
      semantics ~ q_many + number_plural + det_a
    }
    fit <- rpart::rpart(
      tree_formula,
      data = tree_df,
      method = "class",
      control = rpart::rpart.control(cp = 0.01, minsplit = 8, minbucket = 3)
    )

    lemmas <- unique(tree_df$lemma)
    predictions <- lapply(lemmas, function(held_out) {
      train <- tree_df[tree_df$lemma != held_out, , drop = FALSE]
      test <- tree_df[tree_df$lemma == held_out, , drop = FALSE]
      if (length(unique(train$semantics)) < 2L) {
        return(NULL)
      }
      fold_fit <- rpart::rpart(
        tree_formula,
        data = train,
        method = "class",
        control = rpart::rpart.control(cp = 0.01, minsplit = 8, minbucket = 3)
      )
      data.frame(
        actual = as.character(test$semantics),
        predicted = as.character(stats::predict(fold_fit, newdata = test, type = "class")),
        stringsAsFactors = FALSE
      )
    })
    predictions <- do.call(rbind, predictions)
    list(df = tree_df, fit = fit, predictions = predictions)
  })

  output$tree_text <- renderText({
    result <- tree_result()
    accuracy <- mean(result$predictions$actual == result$predictions$predicted)
    sprintf(
      "Cells used: %d\nLemmas used: %d\nLeave-one-lemma-out cell accuracy: %.1f%%",
      nrow(result$df),
      length(unique(result$df$lemma)),
      100 * accuracy
    )
  })

  output$tree_plot <- renderPlot({
    result <- tree_result()
    plot(result$fit, uniform = TRUE, margin = 0.08)
    text(result$fit, use.n = TRUE, all = TRUE, cex = 0.8)
  })

  output$tree_confusion <- renderDT({
    result <- tree_result()
    confusion <- as.data.frame(table(
      Actual = result$predictions$actual,
      Predicted = result$predictions$predicted
    ))
    datatable(confusion, options = list(dom = "t"), rownames = FALSE)
  })

  output$download_filtered <- downloadHandler(
    filename = function() {
      paste0("combined_aggregate_", input$granularity, "_filtered.csv")
    },
    content = function(file) {
      utils::write.csv(filtered_data(), file, row.names = FALSE, na = "")
    }
  )
}

shinyApp(ui, server)
