context("Testing rates.R function")

#utilise trajectory data ('traj.rda')
traj2 <- data_imputation(traj, id_field = TRUE, method = 2,
  replace_with = 1, fill_zeros = FALSE)

#create full population data from 'population.rda'
pop <- popl

pop2 <- as.data.frame(matrix(0, nrow(popl), ncol(traj)))
colnames(pop2) <- names(traj2)

pop2[,1] <- as.vector(as.character(pop[,1]))
pop2[,4] <- as.vector(as.character(pop[,2]))
pop2[,8] <- as.vector(as.character(pop[,3]))

list_ <- c(2, 3, 5, 6, 7, 9, 10) #vector of missing years
#fill the missing fields with 'NA'
  for(u_ in seq_len(length(list_))){
     pop2[,list_[u_]] <- "NA"
  }

#estimate missing fields
pop_imp_result <- data_imputation(pop2, id_field = TRUE, method = 2,
  replace_with = 1, fill_zeros = FALSE)

crime_rates <- rates(traj=traj2$CompleteData,
              denomin=pop_imp_result$CompleteData, multiplier = 200)

test_that('dimension of test data', {
  expect_equal(length(crime_rates), 4)

  expect_equal(length(crime_rates$common_ids), 9)
  expect_equal(length(crime_rates$ids_unique_to_traj_data), 1)
  expect_equal(length(crime_rates$ids_unique_to_denom_data), 2)
  expect_equal(ncol(crime_rates$rates_estimates), 10)
  expect_equal(nrow(crime_rates$rates_estimates), 9)
})

test_that('data types correct', {
  expect_is(crime_rates$rates_estimates,'data.frame')
  expect_is(crime_rates$common_ids, 'character')
  expect_is(crime_rates$ids_unique_to_traj_data, 'character')
  expect_is(crime_rates$ids_unique_to_denom_data, 'character')
})

test_that('no missing values in the solution', {
  expect_identical(crime_rates$rates_estimates, na.omit(crime_rates$rates_estimates))
})

test_that('uniqueness of solutions', {
  expect_false(isTRUE(all.equal(crime_rates$common_ids,
                                crime_rates$ids_unique_to_traj_data)))
  expect_false(isTRUE(all.equal(crime_rates$common_ids,
                                crime_rates$ids_unique_to_denom_data)))
  expect_false(isTRUE(all.equal(crime_rates$ids_unique_to_traj_data,
                                crime_rates$ids_unique_to_denom_data)))
})

test_that('output correct error messages', {
  expect_error(rates(traj=traj2, denomin=pop_imp_result,id_field=FALSE,
   prints_text("*---unique field must be set as 'TRUE'!---*")))
  expect_error(rates(traj=traj2, denomin=popl,id_field=TRUE,
   prints_text("(: The 'id_field' of the 'traj' object is not a",
               "unique field. Function terminated!!! :)", sep=" ")))

})


