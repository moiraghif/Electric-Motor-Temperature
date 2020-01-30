rm(list = ls())

library(tidyverse)

read_clean <- function(path)
    read_csv(path) %>%
        filter(Status == "COMPLETED")


plot_history <- function(data)
    ggplot(data, aes(`Trial-ID`, accumulate(Objective, min))) +
        geom_line(colour = "gray") +
        geom_point(size = 1.5) +
        xlab("Iteration") +
        ylab("Best found")

prepare_for_comparison <- function(rnn, lstm, gru, cnn)
    tibble(iteration = rnn$`Trial-ID`,
           rnn =  cummin(rnn$Objective),
           lstm = cummin(lstm$Objective),
           gru =  cummin(gru$Objective),
           cnn =  cummin(cnn$Objective))

plot_comparison <- function(data, y_min, y_max)
    ggplot(data, aes()) +
        # geom_line(aes(iteration,  rnn,  colour = "rnn")) +
        # geom_point(aes(iteration, rnn,  colour = "rnn")) +
        geom_line(aes(iteration,  lstm, colour = "lstm")) +
        geom_point(aes(iteration, lstm, colour = "lstm")) +
        geom_line(aes(iteration,  gru,  colour = "gru")) +
        geom_point(aes(iteration, gru,  colour = "gru")) +
        geom_line(aes(iteration,  cnn,  colour = "cnn")) +
        geom_point(aes(iteration, cnn,  colour = "cnn")) +
        xlab("Iteration") +
        ylab("Best found") +
        scale_fill_manual(values = c("rnn" = "#009E73",
                                     "lstm" = "#D55E00",
                                     "gru" = "#E69F00",
                                     "cnn" = "#56B4E9")) +
        ylim(y_min, y_max)


log_rnn  <- read_clean("./results/AutoML_RNN_MSE.csv")
log_lstm <- read_clean("./results/AutoML_LSTM.csv")
log_gru  <- read_clean("./results/AutoML_GRU.csv")
log_cnn  <- read_clean("./results/AutoML_CNN.csv")
prepare_for_comparison(log_rnn, log_lstm, log_gru, log_cnn) %>%
    plot_comparison(NA, 1e-5) %>%
    ggsave(filename = "./imgs/comparison_MSE.png", device = png(),
           width = 15, height = 5)


log_rnn  <- read_clean("./results/AutoML_RNN_CustomLoss.csv")
log_lstm <- read_clean("./results/AutoML_LSTM_new_loss.csv")
log_gru  <- read_clean("./results/AutoML_GRU_new_loss.csv")
log_cnn  <- read_clean("./results/AutoML_CNN_newLoss.csv")
prepare_for_comparison(log_rnn, log_lstm, log_gru, log_cnn) %>%
    plot_comparison(NA, 1e-5) %>%
    ggsave(filename = "./imgs/comparison_MSE_new_loss.png", device = png(),
           width = 15, height = 5)


plot_comparison <- function(data, y_min, y_max)
    ggplot(data, aes()) +
        geom_line(aes(iteration,  rnn,  colour = "rnn")) +
        geom_point(aes(iteration, rnn,  colour = "rnn")) +
        geom_line(aes(iteration,  lstm, colour = "lstm")) +
        geom_point(aes(iteration, lstm, colour = "lstm")) +
        geom_line(aes(iteration,  cnn,  colour = "cnn")) +
        geom_point(aes(iteration, cnn,  colour = "cnn")) +
        xlab("Iteration") +
        ylab("Best found") +
        scale_fill_manual(values = c("rnn" = "#009E73",
                                     "lstm" = "#D55E00",
                                     "gru" = "#E69F00",
                                     "cnn" = "#56B4E9")) #+
        #ylim(y_min, y_max)

log_rnn  <- read_clean("./results/AutoML_RNN2_MSE.csv")
log_lstm <- read_clean("./results/AutoML_LSTM_second_task.csv")
log_cnn  <- read_clean("./results/AutoML_CNN_reg_new.csv")
prepare_for_comparison(log_rnn, log_lstm, log_gru, log_cnn) %>%
    plot_comparison(1e-2, NA) %>%
    ggsave(filename = "./imgs/comparison_MSE_second.png", device = png(),
           width = 15, height = 5)


