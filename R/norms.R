###############################################################################################
#     Function check_values: return true if there is a value out of the range or not binary
#                 specify binary=T to test if it is binary
###############################################################################################

check_values = function(vec, range1=NULL, range2=NULL, binary = F){
  vec = vec %>% na.omit()
  if(binary){
    sum(!vec %in% c(0,1)) > 0
  }else{
    sum(!between(vec, range1, range2)) >0
  }
}



###############################################################################################
#     Function uds_z_single: calculate the z-score using EAS or NACC norms
#         -dat: a dataframe or tibble
#         -test: character, the name of the cognitive test
#         -norms: specify if you want to use EAS norms('eas') or NACC norms('nacc'), default is 'eas'
#         -impair_sd: a positive number, how many SD worse than the mean should be defined as impairment,
#                     default is 1.
#     for tr_a1 and tr_b1, it would be 1sd above the mean as a higher score in these two tests indicates a worse
#     performance. for all the other tests, it would be 1sd below the mean.
#         -verbose: logical, verbose =T adds z-scores and indicators of impairment,
#                   versbose =F additionally adds mean and sd
###############################################################################################


uds_z_single = function(dat, test, norms = 'eas', impair_sd = 1, verbose = T){
  if(!norms %in% c('eas', 'nacc')){
    stop("Please correct specify which norms to use, can be 'eas' or 'nacc'")
  }

  # use eas or nacc norms
  if(norms=='eas'){
    coef = norms_coef_eas %>% filter(cognitive_test == test)
  }
  if(norms=='nacc'){
    coef = norms_coef_nacc %>% filter(cognitive_test == test)
  }
  if(!nrow(coef)){stop(paste('the cognitive test', test, 'cannot be found in our norms. Please check!'))}
  # check if required covariates are in dat
  covariates = c('female', 'age', 'educyrs', 'black_race')
  cols = colnames(dat)
  if(sum(covariates %in% colnames(dat))!=length(covariates)){
    miss_covariate = paste(covariates[!covariates %in% cols], collapse = ',')
    stop(paste('covariates', miss_covariate, 'cannot be found in your data. Plese check!'))}
  # check if covariates are in the right numeric format
  covariate_ls = map(covariates, ~dat %>% pull(.x) %>% na.omit)
  names(covariate_ls) = covariates
  non_numeric = covariate_ls %>% map(~!is.numeric(.x)) %>% as.numeric()
  if(sum(non_numeric)>0){
    non_numeric_covariate = paste(covariates[non_numeric == 1], collapse = ',')
    stop(paste('covariates', non_numeric_covariate, 'need to be numeric. Please check!'))
  }

  # check if covariates only have reasonable values
  if(check_values(covariate_ls$female, binary = T))
  {stop('variable "female" should only contain 0 or 1. Please check!')}
  if(check_values(covariate_ls$age, 50, 110))
  {stop('variable "age" should be in the range of 50 and 110. Please check!')}
  if(check_values(covariate_ls$educyrs, 0, 40))
  {stop('variable "educyrs" should be in the range of 0 and 40. Please check!')}
  if(check_values(covariate_ls$black_race, binary = T))
  {stop('variable "black_race" should only contain 0 or 1. Please check!')}

  # check if the cognitive test only has reasonable values
  test_var = dat %>% pull(test)
  if(test == 'mocascore')
  {if (check_values(test_var, 0, 30)) stop('mocascore should be in the range of 0 and 30. Please check!')}
  if(test %in% c('verbatimi', 'verbatimd'))
  {if (check_values(test_var, 0, 44)) stop(paste(test, 'should be in the range of 0 and 44. Please check!'))}
  if(test %in% c('paraphrasei', 'paraphrased'))
  {if (check_values(test_var, 0, 25)) stop(paste(test, 'should be in the range of 0 and 25. Please check!'))}
  if(test %in% c('bensonscorei', 'bensonscored'))
  {if (check_values(test_var, 0, 17)) stop(paste(test, 'should be in the range of 0 and 17. Please check!'))}
  if(test %in% c('numspancorf', 'numspancorb'))
  {if (check_values(test_var, 0, 14)) stop(paste(test, 'should be in the range of 0 and 14. Please check!'))}
  if(test %in% c('fwords60sec', 'lwords60sec'))
  {if (check_values(test_var, 0, 40)) stop(paste(test, 'should be in the range of 0 and 40. Please check!'))}
  if(test == 'flword')
  {if (check_values(test_var, 0, 80)) stop('flword should be in the range of 0 and 80. Please check!')}
  if(test %in% c('animals60sec', 'lwords60sec'))
  {if (check_values(test_var, 0, 77)) stop(paste(test, 'should be in the range of 0 and 77. Please check!'))}
  if(test == 'tr_a1')
  {if (check_values(test_var, 0, 150)) stop('tr_a1 should be in the range of 0 and 150. Please check!')}
  if(test == 'tr_b1')
  {if (check_values(test_var, 0, 300)) stop('tr_b1 should be in the range of 0 and 300. Please check!')}

  dat_out = dat %>% bind_cols(coef) %>%
    mutate(mean = estimate_intercept + female*estimate_female + (age-77)*estimate_agecenter77 +
             (educyrs-16)*estimate_educenter16 + black_race*estimate_black_race) %>%
    mutate(z = (!!sym(test) - mean)/estimate_sigma)

  if(test %in% c('tr_a1', 'tr_b1')){
    dat_out = dat_out %>%
      mutate(z = -z)
  }

  dat_out = dat_out %>%
    mutate(!!paste0('impair_', impair_sd, 'sd_', test) := as.numeric((z<=-impair_sd))) %>%
    rename_at(vars(z), ~paste0(., '_', test)) %>%
    select(-colnames(select(coef, -estimate_sigma)))

  if(verbose){
    dat_out %>% select(-mean, -estimate_sigma)
  }else{
    dat_out %>%
      rename(sd = estimate_sigma) %>%
      rename_at(vars(mean, sd), ~paste0(., '_', test))
  }


}


#' Calculate the z-scores and impairment indicators of UDS3 cognitive tests using EAS or NACC norms
#'
#' This function generates z-scores and impairment indicators of UDS3 cognitive tests. Users can secify whether to use EAS or NACC norms.
#'
#' @param dat a dataframe or tibble, contains columns of demographic covariates and UDS3 cognitive tests
#' @param tests character or a character vector, the name of the cognitive test(s)
#' @param norms specify if you want to use EAS norms(norms='eas') or NACC norms(norms='nacc'), default is 'eas'
#' @param impair_sd a positive number, how many SD worse than the mean should be defined as impairment,
#                     default is 1.
#' @param verbose logical, verbose =T adds z-scores and indicators of impairment,
#                   versbose =F additionally adds mean and sd, default is F
#' @return a tibble(dataframe)
#' @export
#'
#'



uds_z = function(dat, tests, norms = 'eas', impair_sd = 1, verbose = T){
  if(!norms %in% c('eas', 'nacc')){
    stop("Please correct specify which norms to use, can be 'eas' or 'nacc'")
  }
  old_vars = colnames(dat)
  map(tests, ~uds_z_single(dat, .x, norms, impair_sd, verbose)) %>% reduce(full_join, by = old_vars)

}
