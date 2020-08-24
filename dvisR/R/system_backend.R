system_options_default <- function(classifier = classifier_xgboost_closure(), ntrials = 20,
                                   learner_list = list(first_learner = learner_furtherest_distance, 
                                                       second_learner = learner_furtherest_distance),
                                   learner_options = list(first_learner = NA, 
                                                          second_learner = NA), 
                                   new_pairs_per_round = c(20,10),
                                   minimum_instances_first_phase = 10){
 .check_system_options(classifier, learner_list, learner_options, new_pairs_per_round, minimum_instances_first_phase)
 
 list(classifier = classifier, learner_list = learner_list,
      learner_options = learner_options, new_pairs_per_round = new_pairs_per_round,
      minimum_instances_first_phase = minimum_instances_first_phase)
}

######################

.check_system_options <- function(classifier, learner_list, learner_options, new_pairs_per_round, minimum_instances_first_phase){
 stopifnot(is.function(classifier))
 
 stopifnot(length(new_pairs_per_round) == 2, is.numeric(new_pairs_per_round), !is.matrix(new_pairs_per_round),
           all(new_pairs_per_round > 0), all(new_pairs_per_round %% 1 == 0))
 
 stopifnot(length(minimum_instances_first_phase) == 1, is.numeric(minimum_instances_first_phase), !is.matrix(minimum_instances_first_phase),
           minimum_instances_first_phase > 0, minimum_instances_first_phase %% 1 == 0)
}