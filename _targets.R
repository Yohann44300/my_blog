library(targets)
library(ggplot2)
# This is an example _targets.R file. Every
# {targets} pipeline needs one.
# Use tar_script() to create _targets.R and tar_edit()
# to open it again for editing.
# Then, run tar_make() to run the pipeline
# and tar_read(summary) to view the results.
tar_source()

#First triangle with the following coordinates:
#A=(0,0), B=(0,1), C=(0.5, sqrt(3)/2)
first_triangle<-c(0,1,0,0,0.5,sqrt(3)/2)


list(
  tar_target("First_iteration", divide_triangle(first_triangle)),
  tar_target("First_plot", plot_triangles(First_iteration)),
  tar_target("Aire_1", area_list(First_iteration)),
  tar_target("Second_iteration", divide_list_triangle(First_iteration)),
  tar_target("Second_plot", plot_triangles(Second_iteration)),
  tar_target("Aire_2", area_list(Second_iteration)),
  tar_target("Third_iteration", divide_list_triangle(Second_iteration)),
  tar_target("Third_plot", plot_triangles(Third_iteration)),
  tar_target("Aire_3", area_list(Third_iteration)),
  tar_target("Fourth_iteration", divide_list_triangle(Third_iteration)),
  tar_target("Fourth_plot", plot_triangles(Fourth_iteration)),
  tar_target("Aire_4", area_list(Fourth_iteration)),
  tar_target("Render_post", tar_quarto(post.qmd))
)

