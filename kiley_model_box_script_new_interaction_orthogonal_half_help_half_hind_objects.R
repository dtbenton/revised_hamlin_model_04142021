##############################################################
# Hamlin, Wynn, and Bloom (2007) simulation: 'Hill' paradigm #
##############################################################

# load linear-independent vector/matrix creating function
require(hier.part)

# This script includes code for two separate pretraining events, habituation events, and test events. 
# The pretraining events are presented in two blocks. During the first block, networks learn that
# agents have eyes and that half of the time those agents engage in concordant action and the remaining 
# half of the time those agents engage in discordant action. Networks also learn that objects-which equal in number
# the number of agents-neither engage in discordant or concordant action. 

# During the second pretraining event, networks learn that those agents that engaged previously in concordant action have
# the capacity for interaction and those agents that engaged previously 
# in discordant action *do not* have the capacity for interaction. Networks also learned during this phase that objects 
# have the capacity to engage in interaction half of the time; the remaining half of the time the unit that denotes interaction
# is not set to 'on'.



# The habituation and test events mimic those in Hamlin et al. (2007)

#############              
## OBJECTS ##
#############
objects_people_full = combos(14)$binary
modified_op_full = objects_people_full

# OBJECTS: replace "objects_pretrain_stim" with "objects_pretrain_stim"
objects_pretrain_stim = as.data.frame(matrix(NA, nrow=1000, ncol=14))
for(i in 1:nrow(modified_op_full)){
  if(modified_op_full[i,3]==1|modified_op_full[i,4]==1|modified_op_full[i,5]==1|modified_op_full[i,6]==1|modified_op_full[i,7]==1|modified_op_full[i,8]==1|modified_op_full[i,9]==1|modified_op_full[i,10]==1|modified_op_full[i,11]==1|modified_op_full[i,12]==1|modified_op_full[i,13]==1|modified_op_full[i,14]==1){
    objects_pretrain_stim[i,] = NA
  } else {
    objects_pretrain_stim[i,] = modified_op_full[i,]
  }
}
objects_pretrain_stim = na.omit(objects_pretrain_stim)
names(objects_pretrain_stim) = NULL
rownames(objects_pretrain_stim) = NULL

# 11000000000000

# CONCORDANT_AGENTs: replace "people_pretrain_stim" with "people_pretrain_stim"
people_pretrain_stim = as.data.frame(matrix(NA, nrow=1000, ncol=14))
for(i in 1:nrow(modified_op_full)){
  if(modified_op_full[i,5]==1|modified_op_full[i,6]==1|modified_op_full[i,7]==1|modified_op_full[i,8]==1|modified_op_full[i,9]==1|modified_op_full[i,10]==1|modified_op_full[i,11]==1|modified_op_full[i,12]==1|modified_op_full[i,13]==1|modified_op_full[i,14]==1){
    people_pretrain_stim[i,] = NA
  } else {
    people_pretrain_stim[i,] = modified_op_full[i,]
  }
}

people_pretrain_stim = na.omit(people_pretrain_stim)
names(people_pretrain_stim) = NULL
rownames(people_pretrain_stim) = NULL

#11110000000000

# HElPER: 
helper_stim = as.data.frame(matrix(NA, nrow=1000, ncol=14))
for(i in 1:nrow(modified_op_full)){
  if(modified_op_full[i,1]==1|modified_op_full[i,2]==1|modified_op_full[i,3]==1|modified_op_full[i,4]==1|modified_op_full[i,5]==1|modified_op_full[i,6]==1|modified_op_full[i,7]==1|modified_op_full[i,8]==1|modified_op_full[i,11]==1|modified_op_full[i,12]==1|modified_op_full[i,13]==1|modified_op_full[i,14]==1){
    helper_stim[i,] = NA
  } else {
    helper_stim[i,] = modified_op_full[i,]
  }
}

helper_stim = na.omit(helper_stim)
names(helper_stim) = NULL
rownames(helper_stim) = NULL

#00000000110000

# CLIMBER: 
climber_stim = as.data.frame(matrix(NA, nrow=1000, ncol=14))
for(i in 1:nrow(modified_op_full)){
  if(modified_op_full[i,1]==1|modified_op_full[i,2]==1|modified_op_full[i,3]==1|modified_op_full[i,4]==1|modified_op_full[i,5]==1|modified_op_full[i,6]==1|modified_op_full[i,7]==1|modified_op_full[i,8]==1|modified_op_full[i,9]==1|modified_op_full[i,10]==1|modified_op_full[i,13]==1|modified_op_full[i,14]==1){
    climber_stim[i,] = NA
  } else {
    climber_stim[i,] = modified_op_full[i,]
  }
}

climber_stim = na.omit(climber_stim)
names(climber_stim) = NULL
rownames(climber_stim) = NULL

#00000000001100


# HINDERER: 
hinderer_stim = as.data.frame(matrix(NA, nrow=1000, ncol=14))
for(i in 1:nrow(modified_op_full)){
  if(modified_op_full[i,1]==1|modified_op_full[i,2]==1|modified_op_full[i,3]==1|modified_op_full[i,4]==1|modified_op_full[i,5]==1|modified_op_full[i,6]==1|modified_op_full[i,7]==1|modified_op_full[i,8]==1|modified_op_full[i,9]==1|modified_op_full[i,10]==1|modified_op_full[i,11]==1|modified_op_full[i,12]==1){
    hinderer_stim[i,] = NA
  } else {
    hinderer_stim[i,] = modified_op_full[i,]
  }
}

hinderer_stim = na.omit(hinderer_stim)
names(hinderer_stim) = NULL
rownames(hinderer_stim) = NULL

#00000000000011

# PRETRAINING SET: PEOPLE
pretrain_people_1 = people_pretrain_stim[1:2,] # concordant action
pretrain_people_2 = people_pretrain_stim[3:4,] # concordant action
pretrain_people_3 = people_pretrain_stim[5:6,] # discordant action
pretrain_people_4 = people_pretrain_stim[7:8,] # discordant action

pretrain_people_5 = people_pretrain_stim[9:10,]  #Use this data frame for the people-object-help-non-interactions
pretrain_people_6 = people_pretrain_stim[11:12,]  #Use this data frame for the people-object-hind-non-interactions

# PRETRAINING SET: OBJECTS
pretrain_objects_1 = objects_pretrain_stim[1:4,] # no action 
pretrain_objects_2 = objects_pretrain_stim[5:8,] # no action 
names(pretrain_objects_1) = NULL
rownames(pretrain_objects_1) = NULL
names(pretrain_objects_2) = NULL
rownames(pretrain_objects_2) = NULL

pretrain_objects_5 = objects_pretrain_stim[9:10,] # Use this data frame for the people-object-help-non-interactions
pretrain_objects_6 = objects_pretrain_stim[11:12,] # Use this data frame for the people-object-hind-non-interactions

# HAB/TESTING SET: OBJECTS
hab_test_object_1 = helper_stim[1:2,]
hab_test_object_2 = climber_stim[1:2,]
hab_test_object_3 = hinderer_stim[1:2,]

# EMPTY PERSON
empty_character = data.frame(x = c('0 0 0 0 0 0 0 0 0 0 0 0'))

names(empty_character) = NULL
colnames(empty_character) = NULL
rownames(empty_character) = NULL


###############
# interaction #
###############
interaction_vec = data.frame(x = c('1 0', '0 1'))

# interaction = c(1,0)
# non-interaction = c(0,1)


names(interaction_vec) = NULL
colnames(interaction_vec) = NULL
rownames(interaction_vec) = NULL


########
# eyes #
########
eyes_vec = data.frame(x = c('1 0', '0 1'))

# has eyes = c(1,0)
# doesn't have eyes = c(0,1)


names(eyes_vec) = NULL
colnames(eyes_vec) = NULL
rownames(eyes_vec) = NULL



empty_eyes_vec = data.frame(x = c('0 0'))


names(empty_eyes_vec) = NULL
colnames(empty_eyes_vec) = NULL
rownames(empty_eyes_vec) = NULL



############
# mountain #
############
mountain = data.frame(x = c('0 0 0 0 0 0 0 0 0 1 0 0 0 1 1 0 0 1 1 1 0 1 1 1 1'))

names(mountain) = NULL
colnames(mountain) = NULL
rownames(mountain) = NULL


##########
# motion # 
##########
motion = data.frame(x = c('1 0 0 0 0',
                          '0 1 0 0 0',
                          '0 0 1 0 0',
                          '0 0 0 1 0',
                          '0 0 0 0 1'))


names(motion) = NULL
colnames(motion) = NULL
rownames(motion) = NULL


empty_motion = data.frame(x = c('0 0 0 0 0'))
names(empty_motion) = NULL
colnames(empty_motion) = NULL
rownames(empty_motion) = NULL


sink('pretraining_people_concordant_discordant.ex')
k = 1
for(i in 1:nrow(pretrain_people_1)){
  for(j in 1:nrow(pretrain_people_2)){
    ## People/Peope Opening Boxes Together ##
    # TIME STEP 1
    # INPUT
    cat(paste("name: PeoplePeopleConcordant", k, "\n", sep=""))
    cat(paste("3", "\n", sep="\t"))
    cat(paste("I:", "\n", sep="\t"))
    
    # Person 1
    cat(paste("(Slot_A)", sep="\t"))
    print(pretrain_people_1[i,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_1)", sep="\t"))
    print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2
    cat(paste("(Slot_B)", sep="\t"))
    print(pretrain_people_2[j,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_2)", sep="\t"))
    print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Motion Input
    # Person 1 Motion
    cat(paste("(Motion_IN_1)", sep="\t"))
    print(motion[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2 Motion
    cat(paste("(Motion_IN_2)", sep="\t"))
    print(motion[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    
    # OUTPUT
    cat(paste("T:", "\n", sep="\t"))
    # Person 1
    cat(paste("(Slot_A_OUT)", sep="\t"))
    print(pretrain_people_1[i,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_1_OUT)", sep="\t"))
    print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2
    cat(paste("(Slot_B_OUT)", sep="\t"))
    print(pretrain_people_2[j,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_2_OUT)", sep="\t"))
    print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Motion Output
    # Person 1 Motion
    cat(paste("(Motion_OUT_1)", sep="\t"))
    print(motion[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2 Motion
    cat(paste("(Motion_OUT_2)", sep="\t"))
    print(motion[3,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    
    # TIME STEP 2
    # INPUT
    
    cat(paste("I:", "\n", sep="\t"))
    
    # Person 1
    cat(paste("(Slot_A)", sep="\t"))
    print(pretrain_people_1[i,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_1)", sep="\t"))
    print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2
    cat(paste("(Slot_B)", sep="\t"))
    print(pretrain_people_2[j,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_2)", sep="\t"))
    print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Motion Input
    # Person 1 Motion
    cat(paste("(Motion_IN_1)", sep="\t"))
    print(motion[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2 Motion
    cat(paste("(Motion_IN_2)", sep="\t"))
    print(motion[3,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    
    # OUTPUT
    cat(paste("T:", "\n", sep="\t"))
    # Person 1
    cat(paste("(Slot_A_OUT)", sep="\t"))
    print(pretrain_people_1[i,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_1_OUT)", sep="\t"))
    print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2
    cat(paste("(Slot_B_OUT)", sep="\t"))
    print(pretrain_people_2[j,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_2_OUT)", sep="\t"))
    print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Motion Output
    # Person 1 Motion
    cat(paste("(Motion_OUT_1)", sep="\t"))
    print(motion[3,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2 Motion
    cat(paste("(Motion_OUT_2)", sep="\t"))
    print(motion[4,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    
    # TIME STEP 3
    # INPUT
    
    cat(paste("I:", "\n", sep="\t"))
    
    # Person 1
    cat(paste("(Slot_A)", sep="\t"))
    print(pretrain_people_1[i,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_1)", sep="\t"))
    print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2
    cat(paste("(Slot_B)", sep="\t"))
    print(pretrain_people_2[j,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_2)", sep="\t"))
    print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Motion Input
    # Person 1 Motion
    cat(paste("(Motion_IN_1)", sep="\t"))
    print(motion[3,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2 Motion
    cat(paste("(Motion_IN_2)", sep="\t"))
    print(motion[4,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    
    # OUTPUT
    cat(paste("T:", "\n", sep="\t"))
    # Person 1
    cat(paste("(Slot_A_OUT)", sep="\t"))
    print(pretrain_people_1[i,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_1_OUT)", sep="\t"))
    print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2
    cat(paste("(Slot_B_OUT)", sep="\t"))
    print(pretrain_people_2[j,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_2_OUT)", sep="\t"))
    print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Motion Output
    # Person 1 Motion
    cat(paste("(Motion_OUT_1)", sep="\t"))
    print(motion[4,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2 Motion
    cat(paste("(Motion_OUT_2)", sep="\t"))
    print(motion[5,1], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste(";", sep="\t"))
    cat("\n")
    
    cat(paste("name: PeoplePeopleDiscordant", k, "\n", sep=""))
    cat(paste("3", "\n", sep="\t"))
    cat(paste("I:", "\n", sep="\t"))
    
    # Person 1
    cat(paste("(Slot_A)", sep="\t"))
    print(pretrain_people_3[i,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_1)", sep="\t"))
    print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2
    cat(paste("(Slot_B)", sep="\t"))
    print(pretrain_people_4[j,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_2)", sep="\t"))
    print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Motion Input
    # Person 1 Motion
    cat(paste("(Motion_IN_1)", sep="\t"))
    print(motion[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2 Motion
    cat(paste("(Motion_IN_2)", sep="\t"))
    print(motion[5,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    
    # OUTPUT
    cat(paste("T:", "\n", sep="\t"))
    # Person 1
    cat(paste("(Slot_A_OUT)", sep="\t"))
    print(pretrain_people_3[i,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_1_OUT)", sep="\t"))
    print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2
    cat(paste("(Slot_B_OUT)", sep="\t"))
    print(pretrain_people_4[j,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_2_OUT)", sep="\t"))
    print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Motion Output
    # Person 1 Motion
    cat(paste("(Motion_OUT_1)", sep="\t"))
    print(motion[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2 Motion
    cat(paste("(Motion_OUT_2)", sep="\t"))
    print(motion[4,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    
    # TIME STEP 2
    # INPUT
    
    cat(paste("I:", "\n", sep="\t"))
    
    # Person 1
    cat(paste("(Slot_A)", sep="\t"))
    print(pretrain_people_3[i,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_1)", sep="\t"))
    print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2
    cat(paste("(Slot_B)", sep="\t"))
    print(pretrain_people_4[j,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_2)", sep="\t"))
    print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Motion Input
    # Person 1 Motion
    cat(paste("(Motion_IN_1)", sep="\t"))
    print(motion[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2 Motion
    cat(paste("(Motion_IN_2)", sep="\t"))
    print(motion[4,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    
    # OUTPUT
    cat(paste("T:", "\n", sep="\t"))
    # Person 1
    cat(paste("(Slot_A_OUT)", sep="\t"))
    print(pretrain_people_3[i,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_1_OUT)", sep="\t"))
    print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2
    cat(paste("(Slot_B_OUT)", sep="\t"))
    print(pretrain_people_4[j,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_2_OUT)", sep="\t"))
    print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Motion Output
    # Person 1 Motion
    cat(paste("(Motion_OUT_1)", sep="\t"))
    print(motion[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2 Motion
    cat(paste("(Motion_OUT_2)", sep="\t"))
    print(motion[3,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    
    # TIME STEP 3
    # INPUT
    
    cat(paste("I:", "\n", sep="\t"))
    
    # Person 1
    cat(paste("(Slot_A)", sep="\t"))
    print(pretrain_people_3[i,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_1)", sep="\t"))
    print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2
    cat(paste("(Slot_B)", sep="\t"))
    print(pretrain_people_4[j,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_2)", sep="\t"))
    print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Motion Input
    # Person 1 Motion
    cat(paste("(Motion_IN_1)", sep="\t"))
    print(motion[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2 Motion
    cat(paste("(Motion_IN_2)", sep="\t"))
    print(motion[3,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    
    # OUTPUT
    cat(paste("T:", "\n", sep="\t"))
    # Person 1
    cat(paste("(Slot_A_OUT)", sep="\t"))
    print(pretrain_people_3[i,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_1_OUT)", sep="\t"))
    print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2
    cat(paste("(Slot_B_OUT)", sep="\t"))
    print(pretrain_people_4[j,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_2_OUT)", sep="\t"))
    print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Motion Output
    # Person 1 Motion
    cat(paste("(Motion_OUT_1)", sep="\t"))
    print(motion[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2 Motion
    cat(paste("(Motion_OUT_2)", sep="\t"))
    print(motion[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste(";", sep="\t"))
    cat("\n")
    
    k = k+1
  }
}
sink()


###################################################
# PEOPLE  INTERACTING/NONINTERACTING WITH INFANTS #
###################################################

sink('pretraining_people_interaction_true_false.ex')
k1 = 1
for(i in 1:nrow(pretrain_people_1)){
  ## People/Peope Opening Boxes Together ##
  # TIME STEP 1
  # INPUT
  cat(paste("name: PeopleInteractionCapacityTRUE", k1, "\n", sep=""))
  cat(paste("1", "\n", sep="\t"))
  cat(paste("I:", "\n", sep="\t"))
  
  # Person 1
  cat(paste("(Slot_A)", sep="\t"))
  print(pretrain_people_1[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste("(Eyes_1)", sep="\t"))
  print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
  
  
  # OUTPUT
  cat(paste("T:", "\n", sep="\t"))
  # Person 1
  cat(paste("(Slot_A_OUT)", sep="\t"))
  print(pretrain_people_1[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste("(Eyes_1_OUT)", sep="\t"))
  print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Interaction 
  cat(paste("(Interaction_OUT)", sep="\t"))
  print(interaction_vec[1,], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste(";", sep="\t"))
  cat("\n")
  
  cat(paste("name: PeopleInteractionCapacityFALSE", k1, "\n", sep=""))
  cat(paste("1", "\n", sep="\t"))
  cat(paste("I:", "\n", sep="\t"))
  
  # Person 1
  cat(paste("(Slot_A)", sep="\t"))
  print(pretrain_people_3[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste("(Eyes_1)", sep="\t"))
  print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
  
  
  # OUTPUT
  cat(paste("T:", "\n", sep="\t"))
  # Person 1
  cat(paste("(Slot_A_OUT)", sep="\t"))
  print(pretrain_people_3[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste("(Eyes_1_OUT)", sep="\t"))
  print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Interaction 
  cat(paste("(Interaction_OUT)", sep="\t"))
  print(interaction_vec[2,], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste(";", sep="\t"))
  cat("\n")
  
  k1 = k1+1
}
sink()




################
# OBJECT FILES #
################
sink('pretraining_object.ex')
k3 = 1
for(i in 1:nrow(pretrain_objects_1)){
  for(j in 1:nrow(pretrain_objects_2)){
    cat(paste("name: ObjectObjectNoAction", k3, "\n", sep=""))
    cat(paste("3", "\n", sep="\t"))
    cat(paste("I:", "\n", sep="\t"))
    
    # Person 1
    cat(paste("(Slot_A)", sep="\t"))
    print(pretrain_objects_1[i,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_1)", sep="\t"))
    print(eyes_vec[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2
    cat(paste("(Slot_B)", sep="\t"))
    print(pretrain_objects_2[j,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_2)", sep="\t"))
    print(eyes_vec[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Motion Input
    # Object 1 Motion
    cat(paste("(Motion_IN_1)", sep="\t"))
    print(motion[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Object 2 Motion
    cat(paste("(Motion_IN_2)", sep="\t"))
    print(motion[3,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # OUTPUT
    cat(paste("T:", "\n", sep="\t"))
    # Person 1
    cat(paste("(Slot_A_OUT)", sep="\t"))
    print(pretrain_objects_1[i,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_1_OUT)", sep="\t"))
    print(eyes_vec[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2
    cat(paste("(Slot_B_OUT)", sep="\t"))
    print(pretrain_objects_2[j,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_2_OUT)", sep="\t"))
    print(eyes_vec[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Motion Output
    # Object 1 Motion
    cat(paste("(Motion_OUT_1)", sep="\t"))
    print(motion[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Object 2 Motion
    cat(paste("(Motion_OUT_2)", sep="\t"))
    print(motion[3,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    
    # TIME STEP 2
    cat(paste("I:", "\n", sep="\t"))
    
    # Person 1
    cat(paste("(Slot_A)", sep="\t"))
    print(pretrain_objects_1[i,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_1)", sep="\t"))
    print(eyes_vec[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2
    cat(paste("(Slot_B)", sep="\t"))
    print(pretrain_objects_2[j,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_2)", sep="\t"))
    print(eyes_vec[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Motion Input
    # Object 1 Motion
    cat(paste("(Motion_IN_1)", sep="\t"))
    print(motion[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Object 2 Motion
    cat(paste("(Motion_IN_2)", sep="\t"))
    print(motion[3,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # OUTPUT
    cat(paste("T:", "\n", sep="\t"))
    # Person 1
    cat(paste("(Slot_A_OUT)", sep="\t"))
    print(pretrain_objects_1[i,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_1_OUT)", sep="\t"))
    print(eyes_vec[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2
    cat(paste("(Slot_B_OUT)", sep="\t"))
    print(pretrain_objects_2[j,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_2_OUT)", sep="\t"))
    print(eyes_vec[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Motion Output
    # Object 1 Motion
    cat(paste("(Motion_OUT_1)", sep="\t"))
    print(motion[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Object 2 Motion
    cat(paste("(Motion_OUT_2)", sep="\t"))
    print(motion[3,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # TIME STEP 3
    cat(paste("I:", "\n", sep="\t"))
    
    # Person 1
    cat(paste("(Slot_A)", sep="\t"))
    print(pretrain_objects_1[i,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_1)", sep="\t"))
    print(eyes_vec[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2
    cat(paste("(Slot_B)", sep="\t"))
    print(pretrain_objects_2[j,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_2)", sep="\t"))
    print(eyes_vec[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Motion Input
    # Object 1 Motion
    cat(paste("(Motion_IN_1)", sep="\t"))
    print(motion[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Object 2 Motion
    cat(paste("(Motion_IN_2)", sep="\t"))
    print(motion[3,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # OUTPUT
    cat(paste("T:", "\n", sep="\t"))
    # Person 1
    cat(paste("(Slot_A_OUT)", sep="\t"))
    print(pretrain_objects_1[i,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_1_OUT)", sep="\t"))
    print(eyes_vec[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2
    cat(paste("(Slot_B_OUT)", sep="\t"))
    print(pretrain_objects_2[j,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_2_OUT)", sep="\t"))
    print(eyes_vec[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Motion Output
    # Object 1 Motion
    cat(paste("(Motion_OUT_1)", sep="\t"))
    print(motion[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Object 2 Motion
    cat(paste("(Motion_OUT_2)", sep="\t"))
    print(motion[3,1], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste(";", sep="\t"))
    cat("\n")
    k3 = k3+1
  }
}
sink()



#######################################################################
# PEOPLE-OBJECT HELP-HIND NON-INTERACTION NONINTERACTING WITH INFANTS #
#######################################################################
sink('pretraining_people_object_help_hind.ex')
k11 = 1
for(i in 1:nrow(pretrain_objects_1)){
  for(j in 1:nrow(pretrain_objects_2)){
    ## People/Peope Opening Boxes Together ##
    # TIME STEP 1
    # INPUT
    cat(paste("name: PeopleObjectHelp", k11, "\n", sep=""))
    cat(paste("3", "\n", sep="\t"))
    cat(paste("I:", "\n", sep="\t"))
    
    # Person 1
    cat(paste("(Slot_A)", sep="\t"))
    print(pretrain_people_5[i,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_1)", sep="\t"))
    print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2
    cat(paste("(Slot_B)", sep="\t"))
    print(pretrain_objects_5[j,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_2)", sep="\t"))
    print(eyes_vec[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Motion Input
    # Person 1 Motion
    cat(paste("(Motion_IN_1)", sep="\t"))
    print(motion[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2 Motion
    cat(paste("(Motion_IN_2)", sep="\t"))
    print(motion[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    
    # OUTPUT
    cat(paste("T:", "\n", sep="\t"))
    # Person 1
    cat(paste("(Slot_A_OUT)", sep="\t"))
    print(pretrain_people_5[i,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_1_OUT)", sep="\t"))
    print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2
    cat(paste("(Slot_B_OUT)", sep="\t"))
    print(pretrain_objects_5[j,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_2_OUT)", sep="\t"))
    print(eyes_vec[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Motion Output
    # Person 1 Motion
    cat(paste("(Motion_OUT_1)", sep="\t"))
    print(motion[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2 Motion
    cat(paste("(Motion_OUT_2)", sep="\t"))
    print(motion[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    
    # TIME STEP 2
    # INPUT
    
    cat(paste("I:", "\n", sep="\t"))
    
    # Person 1
    cat(paste("(Slot_A)", sep="\t"))
    print(pretrain_people_5[i,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_1)", sep="\t"))
    print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2
    cat(paste("(Slot_B)", sep="\t"))
    print(pretrain_objects_5[j,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_2)", sep="\t"))
    print(eyes_vec[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Motion Input
    # Person 1 Motion
    cat(paste("(Motion_IN_1)", sep="\t"))
    print(motion[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2 Motion
    cat(paste("(Motion_IN_2)", sep="\t"))
    print(motion[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    
    # OUTPUT
    cat(paste("T:", "\n", sep="\t"))
    # Person 1
    cat(paste("(Slot_A_OUT)", sep="\t"))
    print(pretrain_people_5[i,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_1_OUT)", sep="\t"))
    print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2
    cat(paste("(Slot_B_OUT)", sep="\t"))
    print(pretrain_objects_5[j,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_2_OUT)", sep="\t"))
    print(eyes_vec[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Motion Output
    # Person 1 Motion
    cat(paste("(Motion_OUT_1)", sep="\t"))
    print(motion[3,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2 Motion
    cat(paste("(Motion_OUT_2)", sep="\t"))
    print(motion[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    
    # TIME STEP 3
    # INPUT
    
    cat(paste("I:", "\n", sep="\t"))
    
    # Person 1
    cat(paste("(Slot_A)", sep="\t"))
    print(pretrain_people_5[i,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_1)", sep="\t"))
    print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2
    cat(paste("(Slot_B)", sep="\t"))
    print(pretrain_objects_5[j,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_2)", sep="\t"))
    print(eyes_vec[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Motion Input
    # Person 1 Motion
    cat(paste("(Motion_IN_1)", sep="\t"))
    print(motion[3,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2 Motion
    cat(paste("(Motion_IN_2)", sep="\t"))
    print(motion[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    
    # OUTPUT
    cat(paste("T:", "\n", sep="\t"))
    # Person 1
    cat(paste("(Slot_A_OUT)", sep="\t"))
    print(pretrain_people_5[i,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_1_OUT)", sep="\t"))
    print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2
    cat(paste("(Slot_B_OUT)", sep="\t"))
    print(pretrain_objects_5[j,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_2_OUT)", sep="\t"))
    print(eyes_vec[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Motion Output
    # Person 1 Motion
    cat(paste("(Motion_OUT_1)", sep="\t"))
    print(motion[4,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2 Motion
    cat(paste("(Motion_OUT_2)", sep="\t"))
    print(motion[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste(";", sep="\t"))
    cat("\n")
    
    cat(paste("name: PeopleObjectHind", k11, "\n", sep=""))
    cat(paste("3", "\n", sep="\t"))
    cat(paste("I:", "\n", sep="\t"))
    
    # Person 1
    cat(paste("(Slot_A)", sep="\t"))
    print(pretrain_people_6[i,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_1)", sep="\t"))
    print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2
    cat(paste("(Slot_B)", sep="\t"))
    print(pretrain_objects_6[j,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_2)", sep="\t"))
    print(eyes_vec[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Motion Input
    # Person 1 Motion
    cat(paste("(Motion_IN_1)", sep="\t"))
    print(motion[5,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2 Motion
    cat(paste("(Motion_IN_2)", sep="\t"))
    print(motion[3,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    
    # OUTPUT
    cat(paste("T:", "\n", sep="\t"))
    # Person 1
    cat(paste("(Slot_A_OUT)", sep="\t"))
    print(pretrain_people_6[i,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_1_OUT)", sep="\t"))
    print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2
    cat(paste("(Slot_B_OUT)", sep="\t"))
    print(pretrain_objects_6[j,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_2_OUT)", sep="\t"))
    print(eyes_vec[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Motion Output
    # Person 1 Motion
    cat(paste("(Motion_OUT_1)", sep="\t"))
    print(motion[4,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2 Motion
    cat(paste("(Motion_OUT_2)", sep="\t"))
    print(motion[3,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    
    # TIME STEP 2
    # INPUT
    
    cat(paste("I:", "\n", sep="\t"))
    
    # Person 1
    cat(paste("(Slot_A)", sep="\t"))
    print(pretrain_people_6[i,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_1)", sep="\t"))
    print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2
    cat(paste("(Slot_B)", sep="\t"))
    print(pretrain_objects_6[j,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_2)", sep="\t"))
    print(eyes_vec[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Motion Input
    # Person 1 Motion
    cat(paste("(Motion_IN_1)", sep="\t"))
    print(motion[4,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2 Motion
    cat(paste("(Motion_IN_2)", sep="\t"))
    print(motion[3,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    
    # OUTPUT
    cat(paste("T:", "\n", sep="\t"))
    # Person 1
    cat(paste("(Slot_A_OUT)", sep="\t"))
    print(pretrain_people_6[i,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_1_OUT)", sep="\t"))
    print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2
    cat(paste("(Slot_B_OUT)", sep="\t"))
    print(pretrain_objects_6[j,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_2_OUT)", sep="\t"))
    print(eyes_vec[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Motion Output
    # Person 1 Motion
    cat(paste("(Motion_OUT_1)", sep="\t"))
    print(motion[3,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2 Motion
    cat(paste("(Motion_OUT_2)", sep="\t"))
    print(motion[3,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    
    # TIME STEP 3
    # INPUT
    
    cat(paste("I:", "\n", sep="\t"))
    
    # Person 1
    cat(paste("(Slot_A)", sep="\t"))
    print(pretrain_people_6[i,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_1)", sep="\t"))
    print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2
    cat(paste("(Slot_B)", sep="\t"))
    print(pretrain_objects_6[j,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_2)", sep="\t"))
    print(eyes_vec[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Motion Input
    # Person 1 Motion
    cat(paste("(Motion_IN_1)", sep="\t"))
    print(motion[3,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2 Motion
    cat(paste("(Motion_IN_2)", sep="\t"))
    print(motion[3,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    
    # OUTPUT
    cat(paste("T:", "\n", sep="\t"))
    # Person 1
    cat(paste("(Slot_A_OUT)", sep="\t"))
    print(pretrain_people_6[i,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_1_OUT)", sep="\t"))
    print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2
    cat(paste("(Slot_B_OUT)", sep="\t"))
    print(pretrain_objects_6[j,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_2_OUT)", sep="\t"))
    print(eyes_vec[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Motion Output
    # Person 1 Motion
    cat(paste("(Motion_OUT_1)", sep="\t"))
    print(motion[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2 Motion
    cat(paste("(Motion_OUT_2)", sep="\t"))
    print(motion[3,1], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste(";", sep="\t"))
    cat("\n")
    
    k11 = k11+1
  }
}
sink()





###################################################
# PEOPLE  INTERACTING/NONINTERACTING WITH INFANTS #
###################################################

sink('pretraining_people_object_noninteraction.ex')
k10 = 1
for(i in 1:nrow(pretrain_people_1)){
  ## People/Peope Opening Boxes Together ##
  # TIME STEP 1
  # INPUT
  cat(paste("name: PeopleObjectHelpNonInteraction", k10, "\n", sep=""))
  cat(paste("1", "\n", sep="\t"))
  cat(paste("I:", "\n", sep="\t"))
  
  # Person 1
  cat(paste("(Slot_A)", sep="\t"))
  print(pretrain_people_5[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste("(Eyes_1)", sep="\t"))
  print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
  
  
  # OUTPUT
  cat(paste("T:", "\n", sep="\t"))
  # Person 1
  cat(paste("(Slot_A_OUT)", sep="\t"))
  print(pretrain_people_5[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste("(Eyes_1_OUT)", sep="\t"))
  print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Interaction 
  cat(paste("(Interaction_OUT)", sep="\t"))
  print(interaction_vec[2,], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste(";", sep="\t"))
  cat("\n")
  
  cat(paste("name: PeopleObjectHindNonInteraction", k10, "\n", sep=""))
  cat(paste("1", "\n", sep="\t"))
  cat(paste("I:", "\n", sep="\t"))
  
  # Person 1
  cat(paste("(Slot_A)", sep="\t"))
  print(pretrain_people_6[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste("(Eyes_1)", sep="\t"))
  print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
  
  
  # OUTPUT
  cat(paste("T:", "\n", sep="\t"))
  # Person 1
  cat(paste("(Slot_A_OUT)", sep="\t"))
  print(pretrain_people_6[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste("(Eyes_1_OUT)", sep="\t"))
  print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Interaction 
  cat(paste("(Interaction_OUT)", sep="\t"))
  print(interaction_vec[2,], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste(";", sep="\t"))
  cat("\n")
  
  k10 = k10+1
}
sink()


#######################################
# OBJECT  NONINTERACTING WITH INFANTS #
#######################################

sink('pretraining_object_interaction.ex')
k4 = 1
for(i in 1:nrow(pretrain_objects_1)){
  ## People/Peope Opening Boxes Together ##
  # TIME STEP 1
  # INPUT
  cat(paste("name: ObjectInteractionCapacityFALSE", k4, "\n", sep=""))
  cat(paste("1", "\n", sep="\t"))
  cat(paste("I:", "\n", sep="\t"))
  
  # Person 1
  cat(paste("(Slot_A)", sep="\t"))
  print(pretrain_objects_1[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste("(Eyes_1)", sep="\t"))
  print(eyes_vec[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
  
  
  # OUTPUT
  cat(paste("T:", "\n", sep="\t"))
  # Person 1
  cat(paste("(Slot_A_OUT)", sep="\t"))
  print(pretrain_objects_1[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste("(Eyes_1_OUT)", sep="\t"))
  print(eyes_vec[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Interaction 
  cat(paste("(Interaction_OUT)", sep="\t"))
  print(interaction_vec[2,], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste(";", sep="\t"))
  cat("\n")
  
  k4 = k4+1
}
sink()

####################
# habituation file #
####################
sink('habituation.ex')
k5 = 1
for(i in 1:nrow(hab_test_object_1)){
  for(j in 1:nrow(hab_test_object_2)){
    cat(paste("name: HelpingEvent", k5, "\n", sep=""))
    cat(paste("3", "\n", sep="\t"))
    cat(paste("I:", "\n", sep="\t"))
    
    # Person 1
    cat(paste("(Slot_A)", sep="\t"))
    print(hab_test_object_1[i,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_1)", sep="\t"))
    print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2
    cat(paste("(Slot_B)", sep="\t"))
    print(hab_test_object_2[j,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_2)", sep="\t"))
    print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Motion Input
    # Person 1 Motion
    cat(paste("(Motion_IN_1)", sep="\t"))
    print(motion[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2 Motion
    cat(paste("(Motion_IN_2)", sep="\t"))
    print(motion[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # box
    cat(paste("(Mountain_IN)", sep="\t"))
    print(mountain[1,1], sep = "\t", quote = FALSE, row.names = FALSE)

    # OUTPUT
    cat(paste("T:", "\n", sep="\t"))
    # Person 1
    cat(paste("(Slot_A_OUT)", sep="\t"))
    print(hab_test_object_1[i,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_1_OUT)", sep="\t"))
    print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2
    cat(paste("(Slot_B_OUT)", sep="\t"))
    print(hab_test_object_2[j,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_2_OUT)", sep="\t"))
    print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Motion Output
    # Person 1 Motion
    cat(paste("(Motion_OUT_1)", sep="\t"))
    print(motion[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2 Motion
    cat(paste("(Motion_OUT_2)", sep="\t"))
    print(motion[3,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # box
    cat(paste("(Mountain_OUT)", sep="\t"))
    print(mountain[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    
    
    # TIME STEP 2
    cat(paste("I:", "\n", sep="\t"))
    
    # Person 1
    cat(paste("(Slot_A)", sep="\t"))
    print(hab_test_object_1[i,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_1)", sep="\t"))
    print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2
    cat(paste("(Slot_B)", sep="\t"))
    print(hab_test_object_2[j,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_2)", sep="\t"))
    print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Motion Input
    # Person 1 Motion
    cat(paste("(Motion_IN_1)", sep="\t"))
    print(motion[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2 Motion
    cat(paste("(Motion_IN_2)", sep="\t"))
    print(motion[3,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # box
    cat(paste("(Mountain_IN)", sep="\t"))
    print(mountain[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    
    
    # OUTPUT
    cat(paste("T:", "\n", sep="\t"))
    # Person 1
    cat(paste("(Slot_A_OUT)", sep="\t"))
    print(hab_test_object_1[i,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_1_OUT)", sep="\t"))
    print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2
    cat(paste("(Slot_B_OUT)", sep="\t"))
    print(hab_test_object_2[j,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_2_OUT)", sep="\t"))
    print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Motion Output
    # Person 1 Motion
    cat(paste("(Motion_OUT_1)", sep="\t"))
    print(motion[3,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2 Motion
    cat(paste("(Motion_OUT_2)", sep="\t"))
    print(motion[4,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # box
    cat(paste("(Mountain_OUT)", sep="\t"))
    print(mountain[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    
    
    # TIME STEP 3
    cat(paste("I:", "\n", sep="\t"))
    
    # Person 1
    cat(paste("(Slot_A)", sep="\t"))
    print(hab_test_object_1[i,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_1)", sep="\t"))
    print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2
    cat(paste("(Slot_B)", sep="\t"))
    print(hab_test_object_2[j,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_2)", sep="\t"))
    print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Motion Input
    # Person 1 Motion
    cat(paste("(Motion_IN_1)", sep="\t"))
    print(motion[3,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2 Motion
    cat(paste("(Motion_IN_2)", sep="\t"))
    print(motion[4,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # box
    cat(paste("(Mountain_IN)", sep="\t"))
    print(mountain[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    
    # OUTPUT
    cat(paste("T:", "\n", sep="\t"))
    # Person 1
    cat(paste("(Slot_A_OUT)", sep="\t"))
    print(hab_test_object_1[i,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_1_OUT)", sep="\t"))
    print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2
    cat(paste("(Slot_B_OUT)", sep="\t"))
    print(hab_test_object_2[j,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_2_OUT)", sep="\t"))
    print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Motion Output
    # Person 1 Motion
    cat(paste("(Motion_OUT_1)", sep="\t"))
    print(motion[4,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2 Motion
    cat(paste("(Motion_OUT_2)", sep="\t"))
    print(motion[5,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # box
    cat(paste("(Mountain_OUT)", sep="\t"))
    print(mountain[1,1], sep = "\t", quote = FALSE, row.names = FALSE)

    cat(paste(";", sep="\t"))
    cat("\n")
    
    
    cat(paste("name: HinderingEvent", k, "\n", sep=""))
    cat(paste("4", "\n", sep="\t"))
    cat(paste("I:", "\n", sep="\t"))
    
    
    # Person 1
    cat(paste("(Slot_A)", sep="\t"))
    print(hab_test_object_3[i,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_1)", sep="\t"))
    print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2
    cat(paste("(Slot_B)", sep="\t"))
    print(hab_test_object_2[j,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_2)", sep="\t"))
    print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Motion Input
    # Person 1 Motion
    cat(paste("(Motion_IN_1)", sep="\t"))
    print(motion[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2 Motion
    cat(paste("(Motion_IN_2)", sep="\t"))
    print(motion[5,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # box
    cat(paste("(Mountain_IN)", sep="\t"))
    print(mountain[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    
    # OUTPUT
    cat(paste("T:", "\n", sep="\t"))
    # Person 1
    cat(paste("(Slot_A_OUT)", sep="\t"))
    print(hab_test_object_3[i,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_1_OUT)", sep="\t"))
    print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2
    cat(paste("(Slot_B_OUT)", sep="\t"))
    print(hab_test_object_2[j,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_2_OUT)", sep="\t"))
    print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Motion Output
    # Person 1 Motion
    cat(paste("(Motion_OUT_1)", sep="\t"))
    print(motion[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2 Motion
    cat(paste("(Motion_OUT_2)", sep="\t"))
    print(motion[4,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # box
    cat(paste("(Mountain_OUT)", sep="\t"))
    print(mountain[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    
    # TIME STEP 2
    cat(paste("I:", "\n", sep="\t"))
    
    # Person 1
    cat(paste("(Slot_A)", sep="\t"))
    print(hab_test_object_3[i,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_1)", sep="\t"))
    print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2
    cat(paste("(Slot_B)", sep="\t"))
    print(hab_test_object_2[j,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_2)", sep="\t"))
    print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Motion Input
    # Person 1 Motion
    cat(paste("(Motion_IN_1)", sep="\t"))
    print(motion[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2 Motion
    cat(paste("(Motion_IN_2)", sep="\t"))
    print(motion[4,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # box
    cat(paste("(Mountain_IN)", sep="\t"))
    print(mountain[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    
    # OUTPUT
    cat(paste("T:", "\n", sep="\t"))
    # Person 1
    cat(paste("(Slot_A_OUT)", sep="\t"))
    print(hab_test_object_3[i,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_1_OUT)", sep="\t"))
    print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2
    cat(paste("(Slot_B_OUT)", sep="\t"))
    print(hab_test_object_2[j,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_2_OUT)", sep="\t"))
    print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Motion Output
    # Person 1 Motion
    cat(paste("(Motion_OUT_1)", sep="\t"))
    print(motion[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2 Motion
    cat(paste("(Motion_OUT_2)", sep="\t"))
    print(motion[3,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # box
    cat(paste("(Mountain_OUT)", sep="\t"))
    print(mountain[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    
    # TIME STEP 3
    cat(paste("I:", "\n", sep="\t"))
    
    # Person 1
    cat(paste("(Slot_A)", sep="\t"))
    print(hab_test_object_3[i,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_1)", sep="\t"))
    print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2
    cat(paste("(Slot_B)", sep="\t"))
    print(hab_test_object_2[j,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_2)", sep="\t"))
    print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Motion Input
    # Person 1 Motion
    cat(paste("(Motion_IN_1)", sep="\t"))
    print(motion[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2 Motion
    cat(paste("(Motion_IN_2)", sep="\t"))
    print(motion[3,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # box
    cat(paste("(Mountain_IN)", sep="\t"))
    print(mountain[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    
    # OUTPUT
    cat(paste("T:", "\n", sep="\t"))
    # Person 1
    cat(paste("(Slot_A_OUT)", sep="\t"))
    print(hab_test_object_3[i,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_1_OUT)", sep="\t"))
    print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2
    cat(paste("(Slot_B_OUT)", sep="\t"))
    print(hab_test_object_2[j,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_2_OUT)", sep="\t"))
    print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Motion Output
    # Person 1 Motion
    cat(paste("(Motion_OUT_1)", sep="\t"))
    print(motion[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2 Motion
    cat(paste("(Motion_OUT_2)", sep="\t"))
    print(motion[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # box
    cat(paste("(Mountain_OUT)", sep="\t"))
    print(mountain[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste(";", sep="\t"))
    cat("\n")
    
    k5 = k5+1
    
  }
}
sink()






sink('habituation_control.ex')
k5 = 1
for(i in 1:nrow(hab_test_object_1)){
  for(j in 1:nrow(hab_test_object_2)){
    cat(paste("name: HelpingEvent", k5, "\n", sep=""))
    cat(paste("3", "\n", sep="\t"))
    cat(paste("I:", "\n", sep="\t"))
    
    # Person 1
    cat(paste("(Slot_A)", sep="\t"))
    print(hab_test_object_1[i,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_1)", sep="\t"))
    print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2
    cat(paste("(Slot_B)", sep="\t"))
    print(hab_test_object_2[j,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_2)", sep="\t"))
    print(eyes_vec[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Motion Input
    # Person 1 Motion
    cat(paste("(Motion_IN_1)", sep="\t"))
    print(motion[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2 Motion
    cat(paste("(Motion_IN_2)", sep="\t"))
    print(motion[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # box
    cat(paste("(Mountain_IN)", sep="\t"))
    print(mountain[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    
    # OUTPUT
    cat(paste("T:", "\n", sep="\t"))
    # Person 1
    cat(paste("(Slot_A_OUT)", sep="\t"))
    print(hab_test_object_1[i,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_1_OUT)", sep="\t"))
    print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2
    cat(paste("(Slot_B_OUT)", sep="\t"))
    print(hab_test_object_2[j,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_2_OUT)", sep="\t"))
    print(eyes_vec[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Motion Output
    # Person 1 Motion
    cat(paste("(Motion_OUT_1)", sep="\t"))
    print(motion[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2 Motion
    cat(paste("(Motion_OUT_2)", sep="\t"))
    print(motion[3,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # box
    cat(paste("(Mountain_OUT)", sep="\t"))
    print(mountain[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    
    # TIME STEP 2
    cat(paste("I:", "\n", sep="\t"))
    
    # Person 1
    cat(paste("(Slot_A)", sep="\t"))
    print(hab_test_object_1[i,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_1)", sep="\t"))
    print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2
    cat(paste("(Slot_B)", sep="\t"))
    print(hab_test_object_2[j,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_2)", sep="\t"))
    print(eyes_vec[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Motion Input
    # Person 1 Motion
    cat(paste("(Motion_IN_1)", sep="\t"))
    print(motion[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2 Motion
    cat(paste("(Motion_IN_2)", sep="\t"))
    print(motion[3,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # box
    cat(paste("(Mountain_IN)", sep="\t"))
    print(mountain[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # OUTPUT
    cat(paste("T:", "\n", sep="\t"))
    # Person 1
    cat(paste("(Slot_A_OUT)", sep="\t"))
    print(hab_test_object_1[i,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_1_OUT)", sep="\t"))
    print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2
    cat(paste("(Slot_B_OUT)", sep="\t"))
    print(hab_test_object_2[j,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_2_OUT)", sep="\t"))
    print(eyes_vec[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Motion Output
    # Person 1 Motion
    cat(paste("(Motion_OUT_1)", sep="\t"))
    print(motion[3,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2 Motion
    cat(paste("(Motion_OUT_2)", sep="\t"))
    print(motion[4,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # box
    cat(paste("(Mountain_OUT)", sep="\t"))
    print(mountain[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    
    # TIME STEP 3
    cat(paste("I:", "\n", sep="\t"))
    
    # Person 1
    cat(paste("(Slot_A)", sep="\t"))
    print(hab_test_object_1[i,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_1)", sep="\t"))
    print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2
    cat(paste("(Slot_B)", sep="\t"))
    print(hab_test_object_2[j,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_2)", sep="\t"))
    print(eyes_vec[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Motion Input
    # Person 1 Motion
    cat(paste("(Motion_IN_1)", sep="\t"))
    print(motion[3,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2 Motion
    cat(paste("(Motion_IN_2)", sep="\t"))
    print(motion[4,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # box
    cat(paste("(Mountain_IN)", sep="\t"))
    print(mountain[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    
    # OUTPUT
    cat(paste("T:", "\n", sep="\t"))
    # Person 1
    cat(paste("(Slot_A_OUT)", sep="\t"))
    print(hab_test_object_1[i,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_1_OUT)", sep="\t"))
    print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2
    cat(paste("(Slot_B_OUT)", sep="\t"))
    print(hab_test_object_2[j,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_2_OUT)", sep="\t"))
    print(eyes_vec[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Motion Output
    # Person 1 Motion
    cat(paste("(Motion_OUT_1)", sep="\t"))
    print(motion[4,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2 Motion
    cat(paste("(Motion_OUT_2)", sep="\t"))
    print(motion[5,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # box
    cat(paste("(Mountain_OUT)", sep="\t"))
    print(mountain[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste(";", sep="\t"))
    cat("\n")
    
    
    cat(paste("name: HinderingEvent", k5, "\n", sep=""))
    cat(paste("4", "\n", sep="\t"))
    cat(paste("I:", "\n", sep="\t"))
    
    
    # Person 1
    cat(paste("(Slot_A)", sep="\t"))
    print(hab_test_object_3[i,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_1)", sep="\t"))
    print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2
    cat(paste("(Slot_B)", sep="\t"))
    print(hab_test_object_2[j,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_2)", sep="\t"))
    print(eyes_vec[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Motion Input
    # Person 1 Motion
    cat(paste("(Motion_IN_1)", sep="\t"))
    print(motion[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2 Motion
    cat(paste("(Motion_IN_2)", sep="\t"))
    print(motion[5,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # box
    cat(paste("(Mountain_IN)", sep="\t"))
    print(mountain[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    
    # OUTPUT
    cat(paste("T:", "\n", sep="\t"))
    # Person 1
    cat(paste("(Slot_A_OUT)", sep="\t"))
    print(hab_test_object_3[i,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_1_OUT)", sep="\t"))
    print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2
    cat(paste("(Slot_B_OUT)", sep="\t"))
    print(hab_test_object_2[j,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_2_OUT)", sep="\t"))
    print(eyes_vec[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Motion Output
    # Person 1 Motion
    cat(paste("(Motion_OUT_1)", sep="\t"))
    print(motion[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2 Motion
    cat(paste("(Motion_OUT_2)", sep="\t"))
    print(motion[4,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # box
    cat(paste("(Mountain_OUT)", sep="\t"))
    print(mountain[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    
    # TIME STEP 2
    cat(paste("I:", "\n", sep="\t"))
    
    # Person 1
    cat(paste("(Slot_A)", sep="\t"))
    print(hab_test_object_3[i,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_1)", sep="\t"))
    print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2
    cat(paste("(Slot_B)", sep="\t"))
    print(hab_test_object_2[j,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_2)", sep="\t"))
    print(eyes_vec[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Motion Input
    # Person 1 Motion
    cat(paste("(Motion_IN_1)", sep="\t"))
    print(motion[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2 Motion
    cat(paste("(Motion_IN_2)", sep="\t"))
    print(motion[4,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # box
    cat(paste("(Mountain_IN)", sep="\t"))
    print(mountain[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    
    # OUTPUT
    cat(paste("T:", "\n", sep="\t"))
    # Person 1
    cat(paste("(Slot_A_OUT)", sep="\t"))
    print(hab_test_object_3[i,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_1_OUT)", sep="\t"))
    print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2
    cat(paste("(Slot_B_OUT)", sep="\t"))
    print(hab_test_object_2[j,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_2_OUT)", sep="\t"))
    print(eyes_vec[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Motion Output
    # Person 1 Motion
    cat(paste("(Motion_OUT_1)", sep="\t"))
    print(motion[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2 Motion
    cat(paste("(Motion_OUT_2)", sep="\t"))
    print(motion[3,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # box
    cat(paste("(Mountain_OUT)", sep="\t"))
    print(mountain[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    
    # TIME STEP 3
    cat(paste("I:", "\n", sep="\t"))
    
    # Person 1
    cat(paste("(Slot_A)", sep="\t"))
    print(hab_test_object_3[i,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_1)", sep="\t"))
    print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2
    cat(paste("(Slot_B)", sep="\t"))
    print(hab_test_object_2[j,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_2)", sep="\t"))
    print(eyes_vec[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Motion Input
    # Person 1 Motion
    cat(paste("(Motion_IN_1)", sep="\t"))
    print(motion[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2 Motion
    cat(paste("(Motion_IN_2)", sep="\t"))
    print(motion[3,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # box
    cat(paste("(Mountain_IN)", sep="\t"))
    print(mountain[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    
    # OUTPUT
    cat(paste("T:", "\n", sep="\t"))
    # Person 1
    cat(paste("(Slot_A_OUT)", sep="\t"))
    print(hab_test_object_3[i,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_1_OUT)", sep="\t"))
    print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2
    cat(paste("(Slot_B_OUT)", sep="\t"))
    print(hab_test_object_2[j,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_2_OUT)", sep="\t"))
    print(eyes_vec[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Motion Output
    # Person 1 Motion
    cat(paste("(Motion_OUT_1)", sep="\t"))
    print(motion[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2 Motion
    cat(paste("(Motion_OUT_2)", sep="\t"))
    print(motion[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # box
    cat(paste("(Mountain_OUT)", sep="\t"))
    print(mountain[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste(";", sep="\t"))
    cat("\n")
    
    k5 = k5+1
    
  }
}
sink()



####################
# test file #
####################

sink('test.ex')
k6 = 1
for(i in 1:nrow(hab_test_object_1)){
  cat(paste("name: Helper", k6, "\n", sep=""))
  cat(paste("1", "\n", sep="\t"))
  cat(paste("I:", "\n", sep="\t"))
  
  # Person 1
  cat(paste("(Slot_A)", sep="\t"))
  print(hab_test_object_1[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste("(Eyes_1)", sep="\t"))
  print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # OUTPUT
  cat(paste("T:", "\n", sep="\t"))
  # Person 1
  cat(paste("(Slot_A_OUT)", sep="\t"))
  print(hab_test_object_1[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste("(Eyes_1_OUT)", sep="\t"))
  print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Person 2
  cat(paste("(Interaction_OUT)", sep="\t"))
  print(interaction_vec[1,], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste(";", sep="\t"))
  cat("\n")
  
  
  cat(paste("name: Hinderer", k, "\n", sep=""))
  cat(paste("1", "\n", sep="\t"))
  cat(paste("I:", "\n", sep="\t"))
  
  # Person 1
  cat(paste("(Slot_A)", sep="\t"))
  print(hab_test_object_3[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste("(Eyes_1)", sep="\t"))
  print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # OUTPUT
  cat(paste("T:", "\n", sep="\t"))
  # Person 1
  cat(paste("(Slot_A_OUT)", sep="\t"))
  print(hab_test_object_3[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste("(Eyes_1_OUT)", sep="\t"))
  print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Person 2
  cat(paste("(Interaction_OUT)", sep="\t"))
  print(interaction_vec[2,], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste(";", sep="\t"))
  cat("\n")
  
  k6 = k6+1
}
sink()

sink('test_no_eyes.ex')
k7 = 1
for(i in 1:nrow(hab_test_object_1)){
  cat(paste("name: Helper", k7, "\n", sep=""))
  cat(paste("1", "\n", sep="\t"))
  cat(paste("I:", "\n", sep="\t"))
  
  # Person 1
  cat(paste("(Slot_A)", sep="\t"))
  print(hab_test_object_1[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste("(Eyes_1)", sep="\t"))
  print(eyes_vec[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # OUTPUT
  cat(paste("T:", "\n", sep="\t"))
  # Person 1
  cat(paste("(Slot_A_OUT)", sep="\t"))
  print(hab_test_object_1[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste("(Eyes_1_OUT)", sep="\t"))
  print(eyes_vec[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Person 2
  cat(paste("(Interaction_OUT)", sep="\t"))
  print(interaction_vec[1,], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste(";", sep="\t"))
  cat("\n")
  
  
  cat(paste("name: Hinderer", k7, "\n", sep=""))
  cat(paste("1", "\n", sep="\t"))
  cat(paste("I:", "\n", sep="\t"))
  
  # Person 1
  cat(paste("(Slot_A)", sep="\t"))
  print(hab_test_object_3[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste("(Eyes_1)", sep="\t"))
  print(eyes_vec[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # OUTPUT
  cat(paste("T:", "\n", sep="\t"))
  # Person 1
  cat(paste("(Slot_A_OUT)", sep="\t"))
  print(hab_test_object_3[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste("(Eyes_1_OUT)", sep="\t"))
  print(eyes_vec[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Person 2
  cat(paste("(Interaction_OUT)", sep="\t"))
  print(interaction_vec[2,], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste(";", sep="\t"))
  cat("\n")
  
  k7 = k7+1
}
sink()


sink('test_control.ex')
k6 = 1
for(i in 1:nrow(hab_test_object_1)){
  cat(paste("name: Helper", k6, "\n", sep=""))
  cat(paste("1", "\n", sep="\t"))
  cat(paste("I:", "\n", sep="\t"))
  
  # Person 1
  cat(paste("(Slot_A)", sep="\t"))
  print(hab_test_object_1[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste("(Eyes_1)", sep="\t"))
  print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # OUTPUT
  cat(paste("T:", "\n", sep="\t"))
  # Person 1
  cat(paste("(Slot_A_OUT)", sep="\t"))
  print(hab_test_object_1[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste("(Eyes_1_OUT)", sep="\t"))
  print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Person 2
  cat(paste("(Interaction_OUT)", sep="\t"))
  print(interaction_vec[1,], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste(";", sep="\t"))
  cat("\n")
  
  
  cat(paste("name: Hinderer", k, "\n", sep=""))
  cat(paste("1", "\n", sep="\t"))
  cat(paste("I:", "\n", sep="\t"))
  
  # Person 1
  cat(paste("(Slot_A)", sep="\t"))
  print(hab_test_object_3[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste("(Eyes_1)", sep="\t"))
  print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # OUTPUT
  cat(paste("T:", "\n", sep="\t"))
  # Person 1
  cat(paste("(Slot_A_OUT)", sep="\t"))
  print(hab_test_object_3[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste("(Eyes_1_OUT)", sep="\t"))
  print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Person 2
  cat(paste("(Interaction_OUT)", sep="\t"))
  print(interaction_vec[1,], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste(";", sep="\t"))
  cat("\n")
  
  k6 = k6+1
}
sink()