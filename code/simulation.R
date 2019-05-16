all_res <- list(1:1000)
for (j in 1:1000) {
    cat(j, "\n")
    source("algo.R")
    
    all_res[[j]] <- final
}

result <- as_tibble(do.call(rbind, all_res)) %>%
    unnest(Major)
