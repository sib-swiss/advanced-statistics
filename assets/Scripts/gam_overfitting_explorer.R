## ------------------------------------------------------------------------
## Overfitting explorer: GAM smoothness vs. train / test error
##
## Same idea as the polynomial-degree explorer, adapted for GAMs. Instead
## of polynomial degree, model complexity is controlled by the smoothing
## parameter (lambda) of a penalized spline: small lambda -> wiggly,
## flexible fit (overfitting risk); large lambda -> smooth, rigid fit
## (underfitting risk). The effective degrees of freedom (EDF) of the
## fitted smooth is the GAM's direct analog of "polynomial degree" and is
## displayed alongside train/test MSE.
##
## Run with:
##   install.packages(c("shiny", "mgcv", "ggplot2", "patchwork"))  # once
##   shiny::runApp("gam_overfitting_explorer.R")
## ------------------------------------------------------------------------

library(shiny)
library(mgcv)
library(ggplot2)
library(patchwork)

## ---- Data (identical to the polynomial explorer, for consistency) -------

x_train <- c(0.3, 1.1, 1.8, 2.6, 3.4, 4.1, 4.9, 5.7, 6.4, 7.2, 7.9, 8.7, 9.5)
y_train <- c(3.979, 3.965, 5.586, 4.16, -0.864, -1.42, -3.379, 0.367,
             -0.024, 0.897, -3.19, -5.808, -11.002)

x_test <- c(0.2, 0.609, 1.017, 1.426, 1.835, 2.243, 2.652, 3.061, 3.47,
            3.878, 4.287, 4.696, 5.104, 5.513, 5.922, 6.33, 6.739, 7.148,
            7.557, 7.965, 8.374, 8.783, 9.191, 9.6)
y_test <- c(3.029, 3.85, 5.351, 4.965, 5.134, 3.614, 2.973, 0.949, 0.058,
            -1.661, -1.842, -2.427, -1.476, -1.355, 0.095, 0.208, 0.996,
            -0.105, -0.652, -2.734, -4.233, -6.959, -8.645, -11.004)

true_fn <- function(x) 2 + 0.4 * x - 0.15 * x^2 + 3 * sin(1.1 * x)

x_dense <- seq(0, 10, length.out = 200)
true_curve <- true_fn(x_dense)

train_df <- data.frame(x = x_train, y = y_train)
test_df  <- data.frame(x = x_test,  y = y_test)
true_df  <- data.frame(x = x_dense, y = true_curve)

## Basis dimension: kept modest (k = 10) because we only have 13 unique
## training x-values -- mgcv requires k to stay below that. With sp -> 0,
## EDF approaches roughly k - 1, giving a near-interpolating wiggly fit,
## the GAM equivalent of a degree-12 polynomial.
K_BASIS <- 10

## ---- Core fit function: given a smoothing parameter, fit + score --------

fit_gam <- function(sp_val) {
  fit <- gam(y ~ s(x, k = K_BASIS, bs = "cr"), data = train_df, sp = sp_val)
  train_pred <- predict(fit, newdata = train_df)
  test_pred  <- predict(fit, newdata = test_df)
  list(
    fit = fit,
    edf = sum(fit$edf),
    train_mse = mean((train_pred - train_df$y)^2),
    test_mse  = mean((test_pred  - test_df$y)^2)
  )
}

## "Complexity" slider variable: complexity = -log10(sp), so INCREASING
## complexity means DECREASING sp -> wigglier fit -> overfitting risk,
## exactly mirroring "increasing polynomial degree" in the earlier demo.
complexity_to_sp <- function(complexity) 10^(-complexity)

## ---- Precompute the train/test error curve across the complexity range --

complexity_grid <- seq(-2, 4, length.out = 31)
grid_results <- lapply(complexity_grid, function(cplx) fit_gam(complexity_to_sp(cplx)))

error_df <- data.frame(
  complexity = complexity_grid,
  edf        = sapply(grid_results, function(f) f$edf),
  train_mse  = sapply(grid_results, function(f) f$train_mse),
  test_mse   = sapply(grid_results, function(f) f$test_mse)
)

MAX_ERR_DISPLAY <- 6.5
Y_RANGE <- c(-16, 16)

## ---- UI -------------------------------------------------------------------

ui <- fluidPage(
  titlePanel("Overfitting explorer: GAM smoothness vs. train / test error"),
  p(style = "color:#666; margin-top:-10px;",
    "Same 13 noisy training points and 24 held-out test points as the",
    " polynomial-degree demo. Here complexity is controlled by the",
    " smoothing parameter (lambda) of a penalized spline instead of by",
    " polynomial degree."),

  sidebarLayout(
    sidebarPanel(
      sliderInput("complexity", "Model complexity (\u2212log\u2081\u2080 \u03bb)",
                  min = -2, max = 4, value = 0, step = 0.2),
      br(),
      tableOutput("stats"),
      helpText("Left (low complexity, large \u03bb): the penalty dominates",
               " and the smooth is squeezed toward a straight line",
               " -- underfitting.",
               " Right (high complexity, tiny \u03bb): the penalty vanishes",
               " and the smooth is free to wiggle through almost every",
               " training point -- overfitting.",
               " EDF (effective degrees of freedom) is the GAM's direct",
               " analog of polynomial degree.")
    ),

    mainPanel(
      plotOutput("combinedPlot", height = "420px")
    )
  )
)

## ---- Server -----------------------------------------------------------------

server <- function(input, output, session) {

  current_fit <- reactive({
    fit_gam(complexity_to_sp(input$complexity))
  })

  fitted_curve <- reactive({
    pred <- predict(current_fit()$fit, newdata = data.frame(x = x_dense))
    data.frame(x = x_dense, y = pmin(pmax(pred, Y_RANGE[1]), Y_RANGE[2]))
  })

  output$stats <- renderTable({
    f <- current_fit()
    gap <- f$test_mse - f$train_mse
    regime <- if (f$edf <= 2.5) {
      "Underfitting"
    } else if (gap > 1.5) {
      "Overfitting"
    } else if (f$test_mse <= 0.35) {
      "Good fit"
    } else {
      "Reasonable fit"
    }
    data.frame(
      Metric = c("Effective df (EDF)", "Train MSE", "Test MSE", "Regime"),
      Value  = c(sprintf("%.2f", f$edf),
                 sprintf("%.2f", f$train_mse),
                 ifelse(f$test_mse > 999,
                        sprintf("%.2e", f$test_mse),
                        sprintf("%.2f", f$test_mse)),
                 regime)
    )
  }, colnames = FALSE)

  output$combinedPlot <- renderPlot({
    cplx <- input$complexity
    f <- current_fit()

    ## Left panel: data + fitted smooth vs. true function
    p1 <- ggplot() +
      geom_line(data = true_df, aes(x, y),
                color = "grey60", linetype = "dashed", linewidth = 0.6) +
      geom_line(data = fitted_curve(), aes(x, y),
                color = "#D85A30", linewidth = 1.1) +
      geom_point(data = train_df, aes(x, y),
                 color = "#378ADD", size = 2.6) +
      geom_point(data = test_df, aes(x, y),
                 shape = 21, color = "#D85A30", fill = NA,
                 stroke = 1, size = 2.4) +
      coord_cartesian(ylim = Y_RANGE) +
      labs(title = sprintf("Fitted GAM smooth (EDF \u2248 %.1f)", f$edf),
           subtitle = "Blue = train points   |   Orange ring = test points   |   Grey dashed = true function",
           x = "x", y = "y") +
      theme_minimal(base_size = 12)

    ## Right panel: train/test error vs. complexity, with a marker at the
    ## current slider position
    p2 <- ggplot(error_df, aes(x = complexity)) +
      geom_line(aes(y = pmin(train_mse, MAX_ERR_DISPLAY)),
                color = "#378ADD", linewidth = 1) +
      geom_line(aes(y = pmin(test_mse, MAX_ERR_DISPLAY)),
                color = "#D85A30", linewidth = 1) +
      geom_vline(xintercept = cplx, color = "grey60", linetype = "dotted") +
      geom_point(aes(x = cplx, y = pmin(f$train_mse, MAX_ERR_DISPLAY)),
                 color = "#378ADD", size = 3) +
      geom_point(aes(x = cplx, y = pmin(f$test_mse, MAX_ERR_DISPLAY)),
                 color = "#D85A30", size = 3) +
      coord_cartesian(ylim = c(0, MAX_ERR_DISPLAY)) +
      labs(title = "Train vs. test error by complexity",
           subtitle = "Blue = train MSE   |   Orange = test MSE   |   x-axis: \u2212log\u2081\u2080(\u03bb), higher = wigglier",
           x = "Complexity  (\u2212log\u2081\u2080 \u03bb)", y = "Mean squared error") +
      theme_minimal(base_size = 12)

    p1 + p2
  })
}

shinyApp(ui, server)
