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
             1.282211785752774e-05,  # GRU
             1.0090488300309295e-05,  # LSTM (ok?)
             0.004485802198374188),  # RNN (ok)
    params = c(820761, 54271, 133152, 2485)) %>%  # (@Momo: ok) needed to add the two models
    plot_comparison("MSE", 1)

## First Task, PK Loss
tibble(
    model = c("CNN", "GRU", "LSTM", "RNN"),
    loss = c(1.9762502941062025e-05,  # CNN
             2.226042532008789e-05,  # GRU
             2.4826416600684155e-05,  # LSTM
             0.002554067688305597),  # RNN (ok)
    params = c(25162,    # CNN (ok)
               5650,     # GRU (ok)
               11936,    # LSTM (ok)
               3190)) %>%  # RNN (ok)
    plot_comparison("Custom Loss", 1)

## Second Task, MSE
tibble(
    model = c("CNN", "LSTM", "RNN"),
    loss = c(0.012622386947383202,  # CNN
             0.014896523450238093,  # LSTM
             0.02316238535651421),  # RNN (ok)
    params = c(9470,   # CNN (ok?)
               8564,    # LSTM (ok?)
               1740)) %>%  # RNN (ok)
    plot_comparison("MSE", 2)

## Second Task, PK Loss
tibble(
    model = c("CNN", "RNN"),
    loss = c(0.014130565648339465,  # CNN
             0.02543090402930924),  # RNN (ok)
    params = c(1872,   # CNN
               2274)) %>%  # RNN (ok)
    plot_comparison("Custom Loss", 2)
