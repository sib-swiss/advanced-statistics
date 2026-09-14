## ------------------------------------------------------------------------
## Overfitting explorer: polynomial degree vs. train / test error
##
## Interactive R/Shiny equivalent of the HTML widget. Same training and
## test points, same true function, same idea: as polynomial degree
## increases, training error falls monotonically while test error
## eventually rises -- the classic overfitting signature.
##
## Run with:
##   install.packages(c("shiny", "ggplot2", "patchwork"))  # once
##   shiny::runApp("overfitting_explorer.R")
## ------------------------------------------------------------------------

library(shiny)
library(ggplot2)
library(patchwork)

## ---- Data (identical to the HTML widget) --------------------------------

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

## ---- Precompute train/test MSE for every degree (for the error curve) ---

degrees <- 1:12

fit_and_score <- function(d) {
  fit <- lm(y ~ poly(x, d, raw = TRUE), data = train_df)
  train_pred <- predict(fit, newdata = train_df)
  test_pred  <- predict(fit, newdata = test_df)
  list(
    fit = fit,
    train_mse = mean((train_pred - train_df$y)^2),
    test_mse  = mean((test_pred  - test_df$y)^2)
  )
}

fits <- lapply(degrees, fit_and_score)
names(fits) <- as.character(degrees)

error_df <- data.frame(
  degree    = degrees,
  train_mse = sapply(fits, function(f) f$train_mse),
  test_mse  = sapply(fits, function(f) f$test_mse)
)

## Cap the y-axis of the error plot so a couple of exploding high-degree
## points don't crush the visible scale (same idea as the HTML widget).
MAX_ERR_DISPLAY <- 6.5

## Cap the y-axis of the fit plot for the same reason (wild extrapolation
## at high degree near the domain edges).
Y_RANGE <- c(-16, 16)

## ---- UI -------------------------------------------------------------------

ui <- fluidPage(
  titlePanel("Overfitting explorer: polynomial degree vs. train / test error"),
  p(style = "color:#666; margin-top:-10px;",
    "Same 13 noisy training points and 24 held-out test points throughout.",
    " Only the polynomial degree (model complexity) changes."),

  sidebarLayout(
    sidebarPanel(
      sliderInput("degree", "Model complexity (degree)",
                  min = 1, max = 12, value = 1, step = 1),
      br(),
      tableOutput("stats"),
      helpText("Train MSE keeps falling as degree increases.",
               "Test MSE bottoms out around degree 5-6, then rises",
               "-- and explodes by degree 11-12, where the curve is",
               "essentially interpolating noise.")
    ),

    mainPanel(
      plotOutput("combinedPlot", height = "420px")
    )
  )
)

## ---- Server -----------------------------------------------------------------

server <- function(input, output, session) {

  current_fit <- reactive({
    fits[[as.character(input$degree)]]
  })

  fitted_curve <- reactive({
    d <- input$degree
    pred <- predict(current_fit()$fit,
                     newdata = data.frame(x = x_dense))
    data.frame(x = x_dense, y = pmin(pmax(pred, Y_RANGE[1]), Y_RANGE[2]))
  })

  output$stats <- renderTable({
    f <- current_fit()
    gap <- f$test_mse - f$train_mse
    regime <- if (input$degree <= 2) {
      "Underfitting"
    } else if (gap > 1.5) {
      "Overfitting"
    } else if (f$test_mse <= 0.35) {
      "Good fit"
    } else {
      "Reasonable fit"
    }
    data.frame(
      Metric = c("Train MSE", "Test MSE", "Regime"),
      Value  = c(sprintf("%.2f", f$train_mse),
                 ifelse(f$test_mse > 999,
                        sprintf("%.2e", f$test_mse),
                        sprintf("%.2f", f$test_mse)),
                 regime)
    )
  }, colnames = FALSE)

  output$combinedPlot <- renderPlot({
    d <- input$degree
    f <- current_fit()

    ## Left panel: data + fitted curve vs. true function
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
      labs(title = paste0("Fitted polynomial (degree ", d, ")"),
           subtitle = "Blue = train points   |   Orange ring = test points   |   Grey dashed = true function",
           x = "x", y = "y") +
      theme_minimal(base_size = 12)

    ## Right panel: train/test error vs. degree, with a marker at current degree
    p2 <- ggplot(error_df, aes(x = degree)) +
      geom_line(aes(y = pmin(train_mse, MAX_ERR_DISPLAY)),
                color = "#378ADD", linewidth = 1) +
      geom_line(aes(y = pmin(test_mse, MAX_ERR_DISPLAY)),
                color = "#D85A30", linewidth = 1) +
      geom_vline(xintercept = d, color = "grey60", linetype = "dotted") +
      geom_point(aes(x = d, y = pmin(f$train_mse, MAX_ERR_DISPLAY)),
                 color = "#378ADD", size = 3) +
      geom_point(aes(x = d, y = pmin(f$test_mse, MAX_ERR_DISPLAY)),
                 color = "#D85A30", size = 3) +
      coord_cartesian(ylim = c(0, MAX_ERR_DISPLAY)) +
      labs(title = "Train vs. test error by degree",
           subtitle = "Blue = train MSE   |   Orange = test MSE",
           x = "Polynomial degree", y = "Mean squared error") +
      theme_minimal(base_size = 12)

    p1 + p2
  })
}

shinyApp(ui, server)
