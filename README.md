# simplan

**Goals**: 

1. A straightforward YAML-based "recipe" for simulation, called in a *simplan*.
2. An R-package for automatically creating the R script based on the simplan. 
3. R package for building a simplan through functions.
4. R package for elliciting distributions.

# Next Steps

1. Function for translating sequence nodes
2. Function for translating sequence chunks into internal for loops.
3. Function for translating sequence initials
4. Function for printing code to R.

```{r}
plan <- read_simplan(file = 'inst/test_seq.yml')
print_file <- "test2.R"
translate_to_rscript(plan, print_file)
# Run the script
plot_nodes_raw_base(plan = plan, facet = FALSE)
```

```{r}
library(simplan)
plan <- read_simplan('inst/test_seq.yml')
plan <- bake_plan(plan)

plan <- add_node(
  plan = plan,
  name = "zambda",
  type = "SIMPLE",
  distribution = "normal",
  parameters = list(mean = 0, sd = 1),
  sequence_parameters = NULL,
  contortion = NULL,
  initials = NULL
)
plan <- bake_plan(plan)
plan <- unbake_plan(plan)
plan <- bake_plan(plan)
```

