# Grid-World-Solver
Grid World Solver : SARSA and Q learning


## Using SARSA and Q-Learning algorithm to Grid world
Repo has 2 scripts, which is training exection file(Grid World Example_SARSA_QL.R) and source script(Grid World Solver.R)

As main script source script from directory where it exist, make sure that both scripts are on same directory

```r
#sourcing function
dirname(rstudioapi::getActiveDocumentContext()$path) %>% paste0('/Grid World Solver.R') %>%
  source
```
