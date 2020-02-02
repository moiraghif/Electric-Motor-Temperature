rm(list = ls())

library(tidyverse)



read_clean <- function(path)
    (read_csv(paste0("./results/AutoML_", path))) %>%
        filter(Status == "COMPLETED") %>%
        select(Objective)


get_min <- function(df)
    cummin(as.vector(df))$Objective[nrow(df)]


get_min_list <- function(files) {
    out <- 0
    for (file in files) out <- out + get_min(read_clean(file))
    return(out)
}


plot_comparison <- function(data, loss.name, task) {
    filename <- paste0("./imgs/performance_parameters_",
                      sub(" ", "_", loss.name),
                      as.character(task), ".png")
    ggplot(data, aes(x = params, y = loss, colour = model)) +
    scale_x_log10() +
    scale_y_log10() +
    geom_point(size = 5) +
    xlab("Parameters of the model (log scale)") +
    ylab(paste(loss.name, "(log scale)")) +
    scale_fill_manual(values = c("rnn" = "#009E73",
                                 "lstm" = "#D55E00",
                                 "gru" = "#E69F00",
                                 "cnn" = "#56B4E9"))
    ggsave(filename = filename,
           device = png(),
           width = 5, height = 5)
}


## First Task, MSE
tibble(
    model = c("CNN", "GRU", "LSTM", "RNN"),
    loss = c(2.3870405136875294e-05,  # CNN (ok?)
             get_min_list(c("GRU.csv", "GRU_stator.csv")),  # GRU
             1.0090488300309295e-05,  # LSTM (ok?)
             0.004485802198374188),  # RNN (ok)
    params = c(777613, 24519, 62882, 2485)) %>%  # (@Momo: ok)
    plot_comparison("MSE", 1)

## First Task, PK Loss
tibble(
    model = c("CNN", "GRU", "LSTM", "RNN"),
    loss = c(get_min_list(c("CNN_newLoss.csv")),  # CNN
             get_min_list(c("GRU_new_loss.csv")),  # GRU
             get_min_list(c("LSTM_new_loss.csv")),  # LSTM
             0.002554067688305597),  # RNN (ok)
    params = c(11840,    # CNN (ok)
               7369,     # GRU (ok)
               97249,    # LSTM (ok)
               3190)) %>%  # RNN (ok)
    plot_comparison("Custom Loss", 1)

## Second Task, MSE
tibble(
    model = c("CNN", "GRU", "LSTM", "RNN"),
    loss = c(get_min_list(c("CNN.csv", "CNN_stator.csv")),  # CNN
             get_min_list(c("GRU.csv", "GRU_stator.csv")),  # GRU
             get_min_list(c("LSTM.csv", "LSTM_stator.csv")),  # LSTM
             0.02316238535651421),  # RNN (ok)
    params = c(777613,   # CNN (ok?)
               24519,    # GRU (ok?)
               62882,    # LSTM (ok?)
               1740)) %>%  # RNN (ok)
    plot_comparison("MSE", 2)

## Second Task, PK Loss
tibble(
    model = c("CNN", "GRU", "LSTM", "RNN"),
    loss = c(get_min_list(c("CNN.csv", "CNN_stator.csv")),  # CNN
             get_min_list(c("GRU.csv", "GRU_stator.csv")),  # GRU
             get_min_list(c("LSTM.csv", "LSTM_stator.csv")),  # LSTM
             0.02543090402930924),  # RNN (ok)
    params = c(777613,   # CNN
               24519,    # GRU
               62882,    # LSTM
               2274)) %>%  # RNN (ok)
    plot_comparison("Custom Loss", 2)
