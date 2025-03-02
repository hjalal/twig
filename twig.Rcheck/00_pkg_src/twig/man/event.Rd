% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_twig_obj.R
\name{event}
\alias{event}
\title{Add an event layer to a twig object}
\usage{
event(name, options, probs, transitions)
}
\arguments{
\item{name}{A character string representing the name of the event. It doesn't need to be quoted.}

\item{options}{A character vector of possible outcomes for the event. They don't need to be included in quotes. One of these options must be none.}

\item{probs}{A character vector of probability function names for each outcome. They don't need to be included in quotes. One of these can be leftover for the remaining probability.}

\item{transitions}{A character vector of state transitions corresponding to each outcome. They don't need to be included in quotes. These could be event names or states if a states layer defined.
One of these can be stay for the Markov state to remain the same.}
}
\value{
A list representing the event layer.
}
\description{
This function creates an event layer that can be added to a twig object. The event layer defines the possible outcomes of an event, their probabilities, and the transitions between states.
}
\examples{
#' # Adding the event layer to a twig object
twig_obj <- twig() + event(name = event_progress, 
                           options = c(yes, none), 
                           probs = c(pProgress, leftover), 
                           transitions = c(Severe, stay))

event_layer <- event(name = "event_progress", 
                     options = c("yes", "none"), 
                     probs = c(pProgress, leftover), 
                     transitions = c("Severe", "stay"))

}
