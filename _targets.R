library(targets)

tar_option_set(
  packages = c("ggdag", "ggraph", "here", "tidygraph", "tidyverse")
)

options(clustermq.scheduler = "multicore")

tar_source()

list(
  tar_target(
    figures,
    make_dag_figures()
  )
)
