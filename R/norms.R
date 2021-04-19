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


#' Calculate the z-scores and impairment indicators of UDS3 cognitive tests
#'
#' \code{uds_z} generates demographically adjusted z-scores and impairment indicators of UDS3 cognitive tests. Users can specify whether
#' to use EAS or NACC norms.
#'
#' Below is the list of UDSNB 3.0 tests, the corresponding variables and item numbers on the UDSNB 3.0 form.\cr
#' - Montreal Cognitive Assessment(MoCA): \code{mocascore}(1f)\cr
#' - Craft Story 21 Recall(Immediate): \code{verbatimi}(3a), \code{paraphrasei}(3b)\cr
#' - Benson Complex Figure Copy: \code{bensonscorei}(4a) \cr
#' - Number Span Test-Forward: \code{numspancorf}(5a) \cr
#' - Number Span Test(Backward):\code{numspancorb}(6a) \cr
#' - Category Fluency:\code{animals60sec}(7a), \code{vegetables60sec}(7b) \cr
#' - Trail Making Test: \code{tr_a1}(8a), \code{tr_b1}(8b) \cr
#' - Craft Story 21 Recall(Delayed): \code{verbatimd}(9a), \code{paraphrased}(9b) \cr
#' - Benson Complex Figure Recall: \code{bensonscored}(10a) \cr
#' - Multilingual Naming Test (MINT): \code{minttotal}(11a) \cr
#' - Verbal Fluency(Phonemic Test): \code{fwords60sec}(12a), \code{lwords60sec}(12d), \code{flword}(12g)
#'
#' Regression estimates from normative data are used to generate gender, age, education, black race adjusted z-scores.
#' Use \code{?norms_coef_eas} and \code{?norms_coef_nacc} to see the documentation of regression estimates from the two norms.
#'
#' The mean is calculated as:
#' \deqn{\hat{Y}=\hat{b0}+ \hat{b1}*female + \hat{b_2}*(age-77) + \hat{b3}*(educyrs-16)+ \hat{b4}*black}
#' For trail A1 and trail B1, as a higher score indicates a worse performance, z-score is calculated as
#' \deqn{-(Y - \hat{Y})/\hat{\sigma}}
#' For all the other cognitive tests where a higher score indicates a better performance, z-score is calculated as
#' \deqn{(Y - \hat{Y})/\hat{\sigma}}
#' Thus, an impairment can be defined as a z-score below a certain cutoff.
#' For example, using a cutoff of -1 means that a subject is defined as impaired on this test if their performance is 1SD worse than the population mean.
#' The indicator of impairment is generated as:
#'  \deqn{impair =1 if z-score<=-1}
#' You can specify how many SD worse than the population mean should be defined as impairment by providing \code{impair_sd}(default is 1).
#' 1SD and 1.5SD are some common choices.
#'
#' @param dat a dataframe or tibble which contains columns of demographic covariates and UDS3 cognitive tests in numeric format.
#'
#'        It should have the four demographic variables named as \code{female}(binary, 1 indicates female), \code{age}(continuous),
#'        \code{educyrs}(continuous) and \code{black_race}(binary, 1 indicates non-hispanic black).
#'        Also, it should have at least one of the following 17 UDSNB 3.0 cognitive test variables(also see Details):
#'        \code{mocascore},\code{verbatimi},\code{paraphrasei},\code{verbatimd},\code{paraphrased},\code{bensonscorei},\code{bensonscored},\code{numspancorf},
#'        \code{numspancorb},\code{minttotal},\code{fwords60sec},\code{lwords60sec},\code{flword},\code{animals60sec},\code{vegetables60sec},\code{tr_a1},\code{tr_b1}.
#'
#'        All the demographic variables and cognitive test variables need to be numeric.
#' @param tests character or a character vector, the name of the cognitive test(s)
#' @param norms specify if you want to use EAS norms(norms='eas') or NACC norms(norms='nacc'), default is 'eas'
#' @param impair_sd a positive number, how many SD worse than the mean should be defined as impairment, default is 1.
#' @param verbose logical, versbose=F additionally adds mean and sd estimates for each cognitive test in the output dataframe, default is F
#' @return a tibble(dataframe) with the demographically adjusted z-score and impairment indicator
#'        for each of the specified cognitive tests.
#' @examples
#'# here is a sample datset used to calculate z-scores. You should prepare your dataset in this standard format.
#' head(sample_dat)

#' # calculate z-scores and the impairment indicators for tr_a1 and tr_b1 with NACC norms and 1.5 SD to define impairment.
#' uds_z(sample_dat, c('tr_a1','tr_b1'), norms = 'nacc', impair_sd = 1.5)
#' # calculate the z-score and the impaiment indicator for minttotal with EAS norms and 1SD to define impairment
#' # Also output mean and sd estimates in addition to the z-scores and the impairment indicators.
#' uds_z(sample_dat, 'minttotal', norms = 'eas', impair_sd = 1, verbose = F)
#' @seealso \url{https://github.com/JiyueQin/eas}
#' @author Jiyue Qin
#' @import dplyr
#' @import tibble
#' @import purrr
#' @import tidyr
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
