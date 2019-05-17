sim_grid <- expand.grid(Mode = c("Reduced", "Complete"), Start = c("Trait Optimized", "Optimized", "Random"), stringsAsFactors = FALSE)
simulated <- TRUE
num_sims <- 1000

all_sims <- apply(sim_grid, 1, function(x) {
    all_res <- list(1:num_sims)
    
    game_mode <<- x[1]
    random_start <<- x[2]

    for (j in 1:num_sims) {
        cat(j, "\n")
        source("algo.R")
        
        all_res[[j]] <- final
    }
    
    result <- as_tibble(do.call(rbind, all_res)) %>%
        unnest(Major)
    
    return(result)
})

final_result <- tibble(
    Mode = sim_grid$Mode,
    Start = sim_grid$Start,
    Results = all_sims
)

save(final_result, file = "../simulation_results/result.RData")
