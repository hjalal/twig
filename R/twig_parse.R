#' Parses a twig model formula structure
#'
#' @param twig_obj a twig object containing the equations for the Markov model
#' @param params a list containing the model parameters
#' @description parses the string formulae and computes the numerical outcomes
#' @return a nuemerical twig object structure 
#' @export
#' 
#' @examples 
#' print("See Vignettes")
twig_parse <- function(x, ...) UseMethod("twig_parse")


#' Parses a twig Markov object 
#' @description given twig equations and a list of parameters it evaluates the model equations and returns the numerical values
#'
#' @param model_struc a twig representing the model structure
#' @param params a parameter list containing the model parameters
#'
#' @return model_num_struc
#' @export
#'
#' @examples 
#' print("See Vignettes")

twig_parse.twig_markov <- function(model_struc, params = NULL){
  if(is.null(params)){
    warning("No parameters were provided. Will use the parameters from the global environment. 
            If instead you want to evaluate the model with specific parameter values, please provide 
            them here as list.")
  } else {
    list2env(params)
  }
  # for Markov structure, parse P, p0, Payoffs, event_payoff and replace
  model_num_str <- model_struc
  payoff_names <- model_num_str$payoff_names
  model_num_str$markov_eqns <- model_num_str$markov_eqns %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(probs = eval(parse(text = probs)))
  
  model_num_str$payoff_eqns <- model_num_str$payoff_eqns %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(dplyr::across(payoff_names, ~ eval(parse(text = .x))))
  
  class(model_num_str) <- "twig_markov"
  return(model_num_str)
}

#' Parses a twig Decision Tree object 
#' @description given twig equations and a list of parameters it evaluates the model equations and returns the numerical values
#'
#' @param model_struc a twig representing the model structure
#' @param params a parameter list containing the model parameters
#'
#' @return model_num_struc
#' @export
#'
#' @examples 
#' print("See Vignettes")

twig_parse.twig_decision <- function(model_struc, params = NULL){
  # if(is.null(params)){
  #   warning("No parameters were provided. Will use the parameters from the global environment. 
  #           If instead you want to evaluate the model with specific parameter values, please provide 
  #           them here as list.")
  # } else {
  #   list2env(params)
  # }
  # # for Decison structure, parse P and Payoffs 
  # 
  # summary_formulae <- model_struc$summary_formulae
  # payoff_names <- model_struc$payoff_names
  # n <- nrow(summary_formulae)
  # summary_results <- summary_formulae
  # for (payoff_name in payoff_names){
  #   for (i in 1:n){
  #     summary_results[[payoff_name]][i] <- eval(parse(text = summary_formulae[[payoff_name]][i]))
  #   }
  # }
  # 
  # class(model_num_str) <- "twig_decision"
  # return(model_num_str)
  # # 
}

