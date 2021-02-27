context("Testing print_akstats.R function")

set.seed(12)
#sample trajectories
df <- data.frame(id=c("1","2","3","4","5","6","7","8","9","10"),
                 t1=sample(0:10, 10, replace = TRUE),
                 t2=sample(0:10, 10, replace = TRUE),
                 t3=sample(0:10, 10, replace = TRUE),
                 t4=sample(0:10, 10, replace = TRUE),
                 t5=sample(0:10, 10, replace = TRUE),
                 t6=sample(0:10, 10, replace = TRUE),
                 t7=sample(0:10, 10, replace = TRUE))
df

#non_unique id field
df2 <- df
df2$id[3] <- 2

# single k value problem
clusterng_1 <- akclustr(df, k = c(3), id_field = TRUE, verbose=TRUE,
                      crit="Calinski_Harabasz", quality_plot=FALSE)

#derive cluster stats
stat_clustering_1 <- print_akstats(clusterng_1, k = 3, show_plots = FALSE)

#non-matching lengths
test_that('check error msgs output correctly', {
  expect_error(print_akstats(clusterng_1, showplots=FALSE, n_quant = 2,
                         prints_text(paste("*----Unequal number of clusters",
                                           "elements and trajectories----*",
                                           sep=" "))))
  #invalid quantile value
  expect_error(print_akstats(clusterng_1, showplots=FALSE, n_quant = 1,
                         prints_text(paste("*----Please, enter an integer",
                                            "between 2",
                                            "and 10 for the 'n_quant'",
                                            "argument'!!!----*",
                                           sep=" "))))

  #invalid quantile value
  expect_error(print_akstats(clusterng_1, df, showplots=FALSE, n_quant = 11,
                         prints_text(paste("*----Please, enter an",
                                            "integer between 2",
                                            "and 10 for the 'n_quant'",
                                            "argument'!!!----*",
                                           sep=" "))))

  #invalid quantile value
  expect_error(print_akstats(clusterng_1, df, showplots=FALSE, n_quant = 2,
                          prints_text(paste("(: The 'id_field' is not a",
                                            "unique field.",
                                            "Function terminated!!! :)",
                                            sep=" "))))

})


test_that('output clusters counts are accurate', {
  expect_equal(sum(as.numeric(stat_clustering_1$descriptive_stats$n)), 10)
  expect_equal(sum(as.numeric(stat_clustering_1$descriptive_stats$`n(%)`)), 100)
  expect_equal(sum(as.numeric(stat_clustering_1$change_stats$`%+ve Traj.`[1]),
          as.numeric(stat_clustering_1$change_stats$`%-ve Traj.`[1])), 100)
  expect_equal(sum(as.numeric(stat_clustering_1$change_stats$`%+ve Traj.`[2]),
          as.numeric(stat_clustering_1$change_stats$`%-ve Traj.`[2])), 100)
  expect_equal(sum(as.numeric(stat_clustering_1$change_stats$`%+ve Traj.`[3]),
          as.numeric(stat_clustering_1$change_stats$`%-ve Traj.`[3])), 100)
})


#ordering of clusters
incr <- function(a, b){
  if(a>=b){
    check <- 1}
  else{
    check <- 2
  }
  return(check)
}

test_that('testing cluster categories', {
  expect_equal(incr(as.numeric(stat_clustering_1$change_stats$`%-ve Traj.`[1]),
         as.numeric(stat_clustering_1$change_stats$`%+ve Traj.`[1])), 1)
  expect_equal(incr(as.numeric(stat_clustering_1$change_stats$`%+ve Traj.`[3]),
         as.numeric(stat_clustering_1$change_stats$`%-ve Traj.`[3])), 1)

})


