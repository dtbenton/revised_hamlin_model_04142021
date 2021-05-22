##############################################################
# Hamlin, Wynn, and Bloom (2007) simulation: 'Hill' paradigm #
##############################################################

# load linear-independent vector/matrix creating function
require(hier.part)
library(hier.part)
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

# CONCORDANT ACTION AGENTS
concordant_agent_pretrain_stim = as.data.frame(matrix(NA, nrow=1000, ncol=14))
for(i in 1:nrow(modified_op_full)){
  if(modified_op_full[i,3]==1|modified_op_full[i,4]==1|modified_op_full[i,5]==1|modified_op_full[i,6]==1|modified_op_full[i,7]==1|modified_op_full[i,8]==1|modified_op_full[i,9]==1|modified_op_full[i,10]==1|modified_op_full[i,11]==1|modified_op_full[i,12]==1|modified_op_full[i,13]==1|modified_op_full[i,14]==1){
    concordant_agent_pretrain_stim[i,] = NA
  } else {
    concordant_agent_pretrain_stim[i,] = modified_op_full[i,]
  }
}

concordant_agent_pretrain_stim = na.omit(concordant_agent_pretrain_stim)
names(concordant_agent_pretrain_stim) = NULL
rownames(concordant_agent_pretrain_stim) = NULL
#11000000000000


# DISCORDANT ACTION AGENTS
discordant_agent_pretrain_stim = as.data.frame(matrix(NA, nrow=1000, ncol=14))
for(i in 1:nrow(modified_op_full)){
  if(modified_op_full[i,1]==1|modified_op_full[i,2]==1|modified_op_full[i,5]==1|modified_op_full[i,6]==1|modified_op_full[i,7]==1|modified_op_full[i,8]==1|modified_op_full[i,9]==1|modified_op_full[i,10]==1|modified_op_full[i,11]==1|modified_op_full[i,12]==1|modified_op_full[i,13]==1|modified_op_full[i,14]==1){
    discordant_agent_pretrain_stim[i,] = NA
  } else {
    discordant_agent_pretrain_stim[i,] = modified_op_full[i,]
  }
}

discordant_agent_pretrain_stim = na.omit(discordant_agent_pretrain_stim)
names(discordant_agent_pretrain_stim) = NULL
rownames(discordant_agent_pretrain_stim) = NULL
#0011000000000000

# OBJECTS
objects_pretrain_stim = as.data.frame(matrix(NA, nrow=1000, ncol=14))
for(i in 1:nrow(modified_op_full)){
  if(modified_op_full[i,1]==1|modified_op_full[i,2]==1|modified_op_full[i,3]==1|modified_op_full[i,4]==1|modified_op_full[i,9]==1|modified_op_full[i,10]==1|modified_op_full[i,11]==1|modified_op_full[i,12]==1|modified_op_full[i,13]==1|modified_op_full[i,14]==1){
    objects_pretrain_stim[i,] = NA
  } else {
    objects_pretrain_stim[i,] = modified_op_full[i,]
  }
}
objects_pretrain_stim = na.omit(objects_pretrain_stim)
names(objects_pretrain_stim) = NULL
rownames(objects_pretrain_stim) = NULL
# 00001111000000

# CLIMBER: 
hab_test_object_2 = as.data.frame(matrix(NA, nrow=1000, ncol=14))
for(i in 1:nrow(modified_op_full)){
  if(modified_op_full[i,1]==1|modified_op_full[i,2]==1|modified_op_full[i,3]==1|modified_op_full[i,4]==1|modified_op_full[i,5]==1|modified_op_full[i,6]==1|modified_op_full[i,7]==1|modified_op_full[i,8]==1|modified_op_full[i,11]==1|modified_op_full[i,12]==1|modified_op_full[i,13]==1|modified_op_full[i,14]==1){
    hab_test_object_2[i,] = NA
  } else {
    hab_test_object_2[i,] = modified_op_full[i,]
  }
}

hab_test_object_2 = na.omit(hab_test_object_2)
names(hab_test_object_2) = NULL
rownames(hab_test_object_2) = NULL
#00000000110000

# HElPER: 
hab_test_object_1 = as.data.frame(matrix(NA, nrow=1000, ncol=14))
for(i in 1:nrow(modified_op_full)){
  if(modified_op_full[i,1]==1|modified_op_full[i,2]==1|modified_op_full[i,3]==1|modified_op_full[i,4]==1|modified_op_full[i,5]==1|modified_op_full[i,6]==1|modified_op_full[i,7]==1|modified_op_full[i,8]==1|modified_op_full[i,9]==1|modified_op_full[i,10]==1|modified_op_full[i,13]==1|modified_op_full[i,14]==1){
    hab_test_object_1[i,] = NA
  } else {
    hab_test_object_1[i,] = modified_op_full[i,]
  }
}

hab_test_object_1 = na.omit(hab_test_object_1)
names(hab_test_object_1) = NULL
rownames(hab_test_object_1) = NULL
#00000000001100


# HINDERER: 
hab_test_object_3 = as.data.frame(matrix(NA, nrow=1000, ncol=14))
for(i in 1:nrow(modified_op_full)){
  if(modified_op_full[i,1]==1|modified_op_full[i,2]==1|modified_op_full[i,3]==1|modified_op_full[i,4]==1|modified_op_full[i,5]==1|modified_op_full[i,6]==1|modified_op_full[i,7]==1|modified_op_full[i,8]==1|modified_op_full[i,9]==1|modified_op_full[i,10]==1|modified_op_full[i,11]==1|modified_op_full[i,12]==1){
    hab_test_object_3[i,] = NA
  } else {
    hab_test_object_3[i,] = modified_op_full[i,]
  }
}

hab_test_object_3 = na.omit(hab_test_object_3)
names(hab_test_object_3) = NULL
rownames(hab_test_object_3) = NULL
#00000000000011

# PRETRAINING SET #
# AGENTS
pretrain_concordant_1 = concordant_agent_pretrain_stim[1:2,]
pretrain_concordant_2 = concordant_agent_pretrain_stim[3,] 

pretrain_discordant_1 = discordant_agent_pretrain_stim[1:2,]
pretrain_discordant_2 = discordant_agent_pretrain_stim[3,] 

# OBJECTS
pretrain_objects_1 = objects_pretrain_stim[1:2,] 
pretrain_objects_2 = objects_pretrain_stim[3,] 

pretrain_objects_3 = objects_pretrain_stim[4:5,]  
pretrain_objects_4 = objects_pretrain_stim[6,] 

names(pretrain_objects_1) = NULL
rownames(pretrain_objects_1) = NULL
names(pretrain_objects_2) = NULL
rownames(pretrain_objects_2) = NULL
names(pretrain_objects_3) = NULL
rownames(pretrain_objects_3) = NULL
names(pretrain_objects_4) = NULL
rownames(pretrain_objects_4) = NULL

# HAB/TESTING SET: OBJECTS
hab_test_object_1 = hab_test_object_1[1:2,]
hab_test_object_2 = hab_test_object_2[1:2,]
hab_test_object_3 = hab_test_object_3[1:2,]


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

# eyes = c(1,0)
# no-eyes = c(0,1)


names(eyes_vec) = NULL
colnames(eyes_vec) = NULL
rownames(eyes_vec) = NULL




################
# memory trace #
################
memory_trace_vec = data.frame(x = c('1 0 0', '0 1 0', '0 0 1'))

# agents that previously engaged in concordant action with another agent = c(1,0,0)
# agents that previously engaged in discordant action with another agent = c(0,1,0)
# agents that previously engaged in no action with another object = c(0,0,1)


names(memory_trace_vec) = NULL
colnames(memory_trace_vec) = NULL
rownames(memory_trace_vec) = NULL


##########
# action #
##########
action_vec = data.frame(x = c('1 0 0', '0 1 0', '0 0 1'))

# concordant action = c(1,0,0)
# discordant action = c(0,1,0)
# no action = c(0,0,1)

names(action_vec) = NULL
colnames(action_vec) = NULL
rownames(action_vec) = NULL

#########################
# self-propelled motion #
#########################
propel_vec = data.frame(x = c('1 0', '0 1'))

# self-propelled motion = c(1,0)
# caused motion = c(0,1)


names(propel_vec) = NULL
colnames(propel_vec) = NULL
rownames(propel_vec) = NULL


############
# mountain #
############
mountain = data.frame(x = c('0 0 0 0 0 0 0 0 0 1 0 0 0 1 1 0 0 1 1 1 0 1 1 1 1'))

names(mountain) = NULL
colnames(mountain) = NULL
rownames(mountain) = NULL


###################
# pretraining set #
###################
sink('pretraining_action.ex')
cat(paste("defT:-", "\n", sep=""))
k = 1
for(i in 1:nrow(pretrain_concordant_1)){
  for(j in 1:nrow(pretrain_concordant_2)){
    # CONCORDANT AGENTS AND CONCORDANT ACTION
    cat(paste("name: ConcordantAgentAction", k, "\n", sep=""))
    cat(paste("I:", "\n", sep="\t"))
    
    # Person 1
    cat(paste("(Slot_A)", sep="\t"))
    print(pretrain_concordant_1[i,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_1)", sep="\t"))
    print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2
    cat(paste("(Slot_B)", sep="\t"))
    print(pretrain_concordant_2[j,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_2)", sep="\t"))
    print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Motion Input
    cat(paste("(Motion_IN)", sep="\t"))
    print(action_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Propulsion Input
    cat(paste("(Propel_IN)", sep="\t"))
    print(propel_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    
    
    cat(paste("T:", "\n", sep="\t"))
    # Person 1
    cat(paste("(Slot_A_OUT)", sep="\t"))
    print(pretrain_concordant_1[i,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_1_OUT)", sep="\t"))
    print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2
    cat(paste("(Slot_B_OUT)", sep="\t"))
    print(pretrain_concordant_2[j,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_2_OUT)", sep="\t"))
    print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Motion Input
    cat(paste("(Motion_OUT)", sep="\t"))
    print(action_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Propulsion Input
    cat(paste("(Propel_OUT)", sep="\t"))
    print(propel_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste(";", sep="\t"))
    cat("\n")
    
    
    
    # DISCORDANT AGENTS AND DISCORDANT ACTION
    cat(paste("name: DiscordantAgentAction", k, "\n", sep=""))
    cat(paste("I:", "\n", sep="\t"))
    
    # Person 1
    cat(paste("(Slot_A)", sep="\t"))
    print(pretrain_discordant_1[i,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_1)", sep="\t"))
    print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2
    cat(paste("(Slot_B)", sep="\t"))
    print(pretrain_discordant_2[j,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_2)", sep="\t"))
    print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Motion Input
    cat(paste("(Motion_IN)", sep="\t"))
    print(action_vec[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Propulsion Input
    cat(paste("(Propel_IN)", sep="\t"))
    print(propel_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    
    
    cat(paste("T:", "\n", sep="\t"))
    # Person 1
    cat(paste("(Slot_A_OUT)", sep="\t"))
    print(pretrain_discordant_1[i,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_1_OUT)", sep="\t"))
    print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2
    cat(paste("(Slot_B_OUT)", sep="\t"))
    print(pretrain_discordant_2[j,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_2_OUT)", sep="\t"))
    print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Motion Input
    cat(paste("(Motion_OUT)", sep="\t"))
    print(action_vec[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Propulsion Input
    cat(paste("(Propel_OUT)", sep="\t"))
    print(propel_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste(";", sep="\t"))
    cat("\n")
    
    
    
    # OBJECTS AND NO ACTION I
    cat(paste("name: ObjectAction", k, "\n", sep=""))
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
    cat(paste("(Motion_IN)", sep="\t"))
    print(action_vec[3,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Propulsion Input
    cat(paste("(Propel_IN)", sep="\t"))
    print(propel_vec[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    
    
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
    
    # Motion Input
    cat(paste("(Motion_OUT)", sep="\t"))
    print(action_vec[3,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Propulsion Input
    cat(paste("(Propel_OUT)", sep="\t"))
    print(propel_vec[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste(";", sep="\t"))
    cat("\n")
    
    
    # OBJECTS AND NO ACTION II
    cat(paste("name: ObjectAction", k, "\n", sep=""))
    cat(paste("I:", "\n", sep="\t"))
    
    # Person 1
    cat(paste("(Slot_A)", sep="\t"))
    print(pretrain_objects_3[i,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_1)", sep="\t"))
    print(eyes_vec[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2
    cat(paste("(Slot_B)", sep="\t"))
    print(pretrain_objects_4[j,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_2)", sep="\t"))
    print(eyes_vec[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Motion Input
    cat(paste("(Motion_IN)", sep="\t"))
    print(action_vec[3,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Propulsion Input
    cat(paste("(Propel_IN)", sep="\t"))
    print(propel_vec[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    
    
    cat(paste("T:", "\n", sep="\t"))
    # Person 1
    cat(paste("(Slot_A_OUT)", sep="\t"))
    print(pretrain_objects_3[i,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_1_OUT)", sep="\t"))
    print(eyes_vec[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2
    cat(paste("(Slot_B_OUT)", sep="\t"))
    print(pretrain_objects_4[j,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_2_OUT)", sep="\t"))
    print(eyes_vec[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Motion Input
    cat(paste("(Motion_OUT)", sep="\t"))
    print(action_vec[3,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Propulsion Input
    cat(paste("(Propel_OUT)", sep="\t"))
    print(propel_vec[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste(";", sep="\t"))
    cat("\n")
    
    
    
    # AGENT/OBJECT ACTION I
    cat(paste("name: AgentObjectAction", k, "\n", sep=""))
    cat(paste("I:", "\n", sep="\t"))
    
    # Person 1
    cat(paste("(Slot_A)", sep="\t"))
    print(pretrain_concordant_2[j,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_1)", sep="\t"))
    print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2
    cat(paste("(Slot_B)", sep="\t"))
    print(pretrain_objects_1[i,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_2)", sep="\t"))
    print(eyes_vec[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Motion Input
    cat(paste("(Motion_IN)", sep="\t"))
    print(action_vec[3,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Propulsion Input
    cat(paste("(Propel_IN)", sep="\t"))
    print(propel_vec[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    
    
    cat(paste("T:", "\n", sep="\t"))
    # Person 1
    cat(paste("(Slot_A_OUT)", sep="\t"))
    print(pretrain_concordant_2[j,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_1_OUT)", sep="\t"))
    print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2
    cat(paste("(Slot_B_OUT)", sep="\t"))
    print(pretrain_objects_1[i,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_2_OUT)", sep="\t"))
    print(eyes_vec[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Motion Input
    cat(paste("(Motion_OUT)", sep="\t"))
    print(action_vec[3,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Propulsion Input
    cat(paste("(Propel_OUT)", sep="\t"))
    print(propel_vec[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste(";", sep="\t"))
    cat("\n")
    
    
    # AGENT/OBJECT ACTION II
    cat(paste("name: AgentObjectAction", k, "\n", sep=""))
    cat(paste("I:", "\n", sep="\t"))
    
    # Person 1
    cat(paste("(Slot_A)", sep="\t"))
    print(pretrain_concordant_2[j,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_1)", sep="\t"))
    print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2
    cat(paste("(Slot_B)", sep="\t"))
    print(pretrain_objects_3[i,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_2)", sep="\t"))
    print(eyes_vec[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Motion Input
    cat(paste("(Motion_IN)", sep="\t"))
    print(action_vec[3,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Propulsion Input
    cat(paste("(Propel_IN)", sep="\t"))
    print(propel_vec[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    
    
    cat(paste("T:", "\n", sep="\t"))
    # Person 1
    cat(paste("(Slot_A_OUT)", sep="\t"))
    print(pretrain_concordant_2[j,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_1_OUT)", sep="\t"))
    print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2
    cat(paste("(Slot_B_OUT)", sep="\t"))
    print(pretrain_objects_3[i,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_2_OUT)", sep="\t"))
    print(eyes_vec[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Motion Input
    cat(paste("(Motion_OUT)", sep="\t"))
    print(action_vec[3,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Propulsion Input
    cat(paste("(Propel_OUT)", sep="\t"))
    print(propel_vec[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste(";", sep="\t"))
    cat("\n")
    
    
    
    
    # AGENT/OBJECT ACTION III
    cat(paste("name: AgentObjectAction", k, "\n", sep=""))
    cat(paste("I:", "\n", sep="\t"))
    
    # Person 1
    cat(paste("(Slot_A)", sep="\t"))
    print(pretrain_concordant_1[i,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_1)", sep="\t"))
    print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2
    cat(paste("(Slot_B)", sep="\t"))
    print(pretrain_objects_2[j,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_2)", sep="\t"))
    print(eyes_vec[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Motion Input
    cat(paste("(Motion_IN)", sep="\t"))
    print(action_vec[3,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Propulsion Input
    cat(paste("(Propel_IN)", sep="\t"))
    print(propel_vec[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    
    
    cat(paste("T:", "\n", sep="\t"))
    # Person 1
    cat(paste("(Slot_A_OUT)", sep="\t"))
    print(pretrain_concordant_1[i,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_1_OUT)", sep="\t"))
    print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2
    cat(paste("(Slot_B_OUT)", sep="\t"))
    print(pretrain_objects_2[j,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_2_OUT)", sep="\t"))
    print(eyes_vec[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Motion Input
    cat(paste("(Motion_OUT)", sep="\t"))
    print(action_vec[3,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Propulsion Input
    cat(paste("(Propel_OUT)", sep="\t"))
    print(propel_vec[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste(";", sep="\t"))
    cat("\n")
    
    
    
    
    # AGENT/OBJECT ACTION IV
    cat(paste("name: AgentObjectAction", k, "\n", sep=""))
    cat(paste("I:", "\n", sep="\t"))
    
    # Person 1
    cat(paste("(Slot_A)", sep="\t"))
    print(pretrain_concordant_1[i,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_1)", sep="\t"))
    print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2
    cat(paste("(Slot_B)", sep="\t"))
    print(pretrain_objects_4[j,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_2)", sep="\t"))
    print(eyes_vec[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Motion Input
    cat(paste("(Motion_IN)", sep="\t"))
    print(action_vec[3,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Propulsion Input
    cat(paste("(Propel_IN)", sep="\t"))
    print(propel_vec[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    
    
    cat(paste("T:", "\n", sep="\t"))
    # Person 1
    cat(paste("(Slot_A_OUT)", sep="\t"))
    print(pretrain_concordant_1[i,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_1_OUT)", sep="\t"))
    print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2
    cat(paste("(Slot_B_OUT)", sep="\t"))
    print(pretrain_objects_4[j,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_2_OUT)", sep="\t"))
    print(eyes_vec[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Motion Input
    cat(paste("(Motion_OUT)", sep="\t"))
    print(action_vec[3,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Propulsion Input
    cat(paste("(Propel_OUT)", sep="\t"))
    print(propel_vec[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste(";", sep="\t"))
    cat("\n")
    
    
    # AGENT/OBJECT ACTION V
    cat(paste("name: AgentObjectAction", k, "\n", sep=""))
    cat(paste("I:", "\n", sep="\t"))
    
    # Person 1
    cat(paste("(Slot_A)", sep="\t"))
    print(pretrain_discordant_2[j,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_1)", sep="\t"))
    print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2
    cat(paste("(Slot_B)", sep="\t"))
    print(pretrain_objects_1[i,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_2)", sep="\t"))
    print(eyes_vec[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Motion Input
    cat(paste("(Motion_IN)", sep="\t"))
    print(action_vec[3,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Propulsion Input
    cat(paste("(Propel_IN)", sep="\t"))
    print(propel_vec[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    
    
    cat(paste("T:", "\n", sep="\t"))
    # Person 1
    cat(paste("(Slot_A_OUT)", sep="\t"))
    print(pretrain_discordant_2[j,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_1_OUT)", sep="\t"))
    print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2
    cat(paste("(Slot_B_OUT)", sep="\t"))
    print(pretrain_objects_1[i,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_2_OUT)", sep="\t"))
    print(eyes_vec[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Motion Input
    cat(paste("(Motion_OUT)", sep="\t"))
    print(action_vec[3,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Propulsion Input
    cat(paste("(Propel_OUT)", sep="\t"))
    print(propel_vec[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste(";", sep="\t"))
    cat("\n")
    
    
    # AGENT/OBJECT ACTION VI
    cat(paste("name: AgentObjectAction", k, "\n", sep=""))
    cat(paste("I:", "\n", sep="\t"))
    
    # Person 1
    cat(paste("(Slot_A)", sep="\t"))
    print(pretrain_discordant_2[j,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_1)", sep="\t"))
    print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2
    cat(paste("(Slot_B)", sep="\t"))
    print(pretrain_objects_3[i,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_2)", sep="\t"))
    print(eyes_vec[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Motion Input
    cat(paste("(Motion_IN)", sep="\t"))
    print(action_vec[3,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Propulsion Input
    cat(paste("(Propel_IN)", sep="\t"))
    print(propel_vec[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    
    
    cat(paste("T:", "\n", sep="\t"))
    # Person 1
    cat(paste("(Slot_A_OUT)", sep="\t"))
    print(pretrain_discordant_2[j,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_1_OUT)", sep="\t"))
    print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2
    cat(paste("(Slot_B_OUT)", sep="\t"))
    print(pretrain_objects_3[i,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_2_OUT)", sep="\t"))
    print(eyes_vec[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Motion Input
    cat(paste("(Motion_OUT)", sep="\t"))
    print(action_vec[3,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Propulsion Input
    cat(paste("(Propel_OUT)", sep="\t"))
    print(propel_vec[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste(";", sep="\t"))
    cat("\n")
    
    
    # AGENT/OBJECT ACTION VII
    cat(paste("name: AgentObjectAction", k, "\n", sep=""))
    cat(paste("I:", "\n", sep="\t"))
    
    # Person 1
    cat(paste("(Slot_A)", sep="\t"))
    print(pretrain_discordant_1[i,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_1)", sep="\t"))
    print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2
    cat(paste("(Slot_B)", sep="\t"))
    print(pretrain_objects_2[j,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_2)", sep="\t"))
    print(eyes_vec[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Motion Input
    cat(paste("(Motion_IN)", sep="\t"))
    print(action_vec[3,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Propulsion Input
    cat(paste("(Propel_IN)", sep="\t"))
    print(propel_vec[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    
    
    cat(paste("T:", "\n", sep="\t"))
    # Person 1
    cat(paste("(Slot_A_OUT)", sep="\t"))
    print(pretrain_discordant_1[i,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_1_OUT)", sep="\t"))
    print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2
    cat(paste("(Slot_B_OUT)", sep="\t"))
    print(pretrain_objects_2[j,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_2_OUT)", sep="\t"))
    print(eyes_vec[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Motion Input
    cat(paste("(Motion_OUT)", sep="\t"))
    print(action_vec[3,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Propulsion Input
    cat(paste("(Propel_OUT)", sep="\t"))
    print(propel_vec[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste(";", sep="\t"))
    cat("\n")
    
    
    # AGENT/OBJECT ACTION VIII
    cat(paste("name: AgentObjectAction", k, "\n", sep=""))
    cat(paste("I:", "\n", sep="\t"))
    
    # Person 1
    cat(paste("(Slot_A)", sep="\t"))
    print(pretrain_discordant_1[i,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_1)", sep="\t"))
    print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2
    cat(paste("(Slot_B)", sep="\t"))
    print(pretrain_objects_4[j,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_2)", sep="\t"))
    print(eyes_vec[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Motion Input
    cat(paste("(Motion_IN)", sep="\t"))
    print(action_vec[3,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Propulsion Input
    cat(paste("(Propel_IN)", sep="\t"))
    print(propel_vec[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    
    
    cat(paste("T:", "\n", sep="\t"))
    # Person 1
    cat(paste("(Slot_A_OUT)", sep="\t"))
    print(pretrain_discordant_1[i,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_1_OUT)", sep="\t"))
    print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Person 2
    cat(paste("(Slot_B_OUT)", sep="\t"))
    print(pretrain_objects_4[j,], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste("(Eyes_2_OUT)", sep="\t"))
    print(eyes_vec[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Motion Input
    cat(paste("(Motion_OUT)", sep="\t"))
    print(action_vec[3,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Propulsion Input
    cat(paste("(Propel_OUT)", sep="\t"))
    print(propel_vec[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste(";", sep="\t"))
    cat("\n")
    
    
    k = k+1
  }
}
sink()





sink('pretraining_interaction.ex')
cat(paste("defT:-", "\n", sep=""))
k = 1
for(i in 1:nrow(pretrain_concordant_1)){
  # CONCORDANT AGENTS AND CONCORDANT ACTION
  cat(paste("name: ConcordantAgentPastConcordantInteraction", k, "\n", sep=""))
  cat(paste("I:", "\n", sep="\t"))
  
  # Person 1
  cat(paste("(Slot_A)", sep="\t"))
  print(pretrain_concordant_1[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste("(Eyes_1)", sep="\t"))
  print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste("(Memory_IN)", sep="\t"))
  print(memory_trace_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
  
  
  
  cat(paste("T:", "\n", sep="\t"))
  # Person 1
  cat(paste("(Slot_A_OUT)", sep="\t"))
  print(pretrain_concordant_1[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste("(Eyes_1_OUT)", sep="\t"))
  print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste("(Memory_OUT)", sep="\t"))
  print(memory_trace_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Interaction Output
  cat(paste("(Interaction_OUT)", sep="\t"))
  print(interaction_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste(";", sep="\t"))
  cat("\n")
  
  # Concordant agent that formerly engaged in no action with an object I
  
  cat(paste("name: ConcordantAgentPastNoActionNoInteraction", k, "\n", sep=""))
  cat(paste("I:", "\n", sep="\t"))
  
  # Person 1
  cat(paste("(Slot_A)", sep="\t"))
  print(pretrain_concordant_1[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste("(Eyes_1)", sep="\t"))
  print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste("(Memory_IN)", sep="\t"))
  print(memory_trace_vec[3,1], sep = "\t", quote = FALSE, row.names = FALSE)
  
  
  
  cat(paste("T:", "\n", sep="\t"))
  # Person 1
  cat(paste("(Slot_A_OUT)", sep="\t"))
  print(pretrain_concordant_1[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste("(Eyes_1_OUT)", sep="\t"))
  print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste("(Memory_OUT)", sep="\t"))
  print(memory_trace_vec[3,1], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Interaction Output
  cat(paste("(Interaction_OUT)", sep="\t"))
  print(interaction_vec[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste(";", sep="\t"))
  cat("\n")
  
  # Concordant agent that formerly engaged in no action with an object II
  
  cat(paste("name: ConcordantAgentPastNoActionInteraction", k, "\n", sep=""))
  cat(paste("I:", "\n", sep="\t"))
  
  # Person 1
  cat(paste("(Slot_A)", sep="\t"))
  print(pretrain_concordant_1[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste("(Eyes_1)", sep="\t"))
  print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste("(Memory_IN)", sep="\t"))
  print(memory_trace_vec[3,1], sep = "\t", quote = FALSE, row.names = FALSE)
  
  
  
  cat(paste("T:", "\n", sep="\t"))
  # Person 1
  cat(paste("(Slot_A_OUT)", sep="\t"))
  print(pretrain_concordant_1[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste("(Eyes_1_OUT)", sep="\t"))
  print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste("(Memory_OUT)", sep="\t"))
  print(memory_trace_vec[3,1], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Interaction Output
  cat(paste("(Interaction_OUT)", sep="\t"))
  print(interaction_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste(";", sep="\t"))
  cat("\n")
  
  
  
  
  # DISCORDANT AGENTS AND DISCORDANT ACTION
  cat(paste("name: DiscordantAgentPastDiscordantNoInteraction", k, "\n", sep=""))
  cat(paste("I:", "\n", sep="\t"))
  
  # Person 1
  cat(paste("(Slot_A)", sep="\t"))
  print(pretrain_discordant_1[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste("(Eyes_1)", sep="\t"))
  print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste("(Memory_IN)", sep="\t"))
  print(memory_trace_vec[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
  
  
  
  cat(paste("T:", "\n", sep="\t"))
  # Person 1
  cat(paste("(Slot_A_OUT)", sep="\t"))
  print(pretrain_discordant_1[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste("(Eyes_1_OUT)", sep="\t"))
  print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste("(Memory_OUT)", sep="\t"))
  print(memory_trace_vec[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Interaction Output
  cat(paste("(Interaction_OUT)", sep="\t"))
  print(interaction_vec[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste(";", sep="\t"))
  cat("\n")
  
  # Discordant agent that formerly engaged in no action with an object I
  
  cat(paste("name: DiscordantAgentPastNoActionNoInteraction", k, "\n", sep=""))
  cat(paste("I:", "\n", sep="\t"))
  
  # Person 1
  cat(paste("(Slot_A)", sep="\t"))
  print(pretrain_discordant_1[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste("(Eyes_1)", sep="\t"))
  print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste("(Memory_IN)", sep="\t"))
  print(memory_trace_vec[3,1], sep = "\t", quote = FALSE, row.names = FALSE)
  
  
  
  cat(paste("T:", "\n", sep="\t"))
  # Person 1
  cat(paste("(Slot_A_OUT)", sep="\t"))
  print(pretrain_discordant_1[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste("(Eyes_1_OUT)", sep="\t"))
  print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste("(Memory_OUT)", sep="\t"))
  print(memory_trace_vec[3,1], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Interaction Output
  cat(paste("(Interaction_OUT)", sep="\t"))
  print(interaction_vec[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste(";", sep="\t"))
  cat("\n")
  
  # Discordant agent that formerly engaged in no action with an object II
  
  cat(paste("name: DiscordantAgentPastNoActionInteraction", k, "\n", sep=""))
  cat(paste("I:", "\n", sep="\t"))
  
  # Person 1
  cat(paste("(Slot_A)", sep="\t"))
  print(pretrain_discordant_1[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste("(Eyes_1)", sep="\t"))
  print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste("(Memory_IN)", sep="\t"))
  print(memory_trace_vec[3,1], sep = "\t", quote = FALSE, row.names = FALSE)
  
  
  
  cat(paste("T:", "\n", sep="\t"))
  # Person 1
  cat(paste("(Slot_A_OUT)", sep="\t"))
  print(pretrain_discordant_1[i,], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste("(Eyes_1_OUT)", sep="\t"))
  print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste("(Memory_OUT)", sep="\t"))
  print(memory_trace_vec[3,1], sep = "\t", quote = FALSE, row.names = FALSE)
  
  # Interaction Output
  cat(paste("(Interaction_OUT)", sep="\t"))
  print(interaction_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste(";", sep="\t"))
  cat("\n")
  
  k = k+1
}
sink()






###################
# habituation set #
###################
sink('habituation.ex')
cat(paste("defT:-", "\n", sep=""))
k = 1
for(i in 1:nrow(hab_test_object_1)){
  for(j in 1:nrow(hab_test_object_2)){
    # HELPING EVENTS
    cat(paste("name: HelpingEvents", k, "\n", sep=""))
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
    cat(paste("(Motion_IN)", sep="\t"))
    print(action_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Propulsion Input
    cat(paste("(Propel_IN)", sep="\t"))
    print(propel_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Mountain
    cat(paste("(Mountain_IN)", sep="\t"))
    print(mountain[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    
    
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
    
    # Motion Input
    cat(paste("(Motion_OUT)", sep="\t"))
    print(action_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Propulsion Input
    cat(paste("(Propel_OUT)", sep="\t"))
    print(propel_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Mountain
    cat(paste("(Mountain_OUT)", sep="\t"))
    print(mountain[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste(";", sep="\t"))
    cat("\n")
    
    
    
    # HINDERING EVENTS
    cat(paste("name: HinderingEvents", k, "\n", sep=""))
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
    cat(paste("(Motion_IN)", sep="\t"))
    print(action_vec[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Propulsion Input
    cat(paste("(Propel_IN)", sep="\t"))
    print(propel_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Mountain
    cat(paste("(Mountain_IN)", sep="\t"))
    print(mountain[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    
    
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
    
    # Motion Input
    cat(paste("(Motion_OUT)", sep="\t"))
    print(action_vec[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Propulsion Input
    cat(paste("(Propel_OUT)", sep="\t"))
    print(propel_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Mountain
    cat(paste("(Mountain_OUT)", sep="\t"))
    print(mountain[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste(";", sep="\t"))
    cat("\n")
    
    k = k+1
  }
}
sink()

sink('habituation_no_eyes_no_sp.ex')
cat(paste("defT:-", "\n", sep=""))
k = 1
for(i in 1:nrow(hab_test_object_1)){
  for(j in 1:nrow(hab_test_object_1)){
    # HELPING EVENTS
    cat(paste("name: HelpingEvents", k, "\n", sep=""))
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
    cat(paste("(Motion_IN)", sep="\t"))
    print(action_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Propulsion Input
    cat(paste("(Propel_IN)", sep="\t"))
    print(propel_vec[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Mountain
    cat(paste("(Mountain_IN)", sep="\t"))
    print(mountain[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    
    
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
    
    # Motion Input
    cat(paste("(Motion_OUT)", sep="\t"))
    print(action_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Propulsion Input
    cat(paste("(Propel_OUT)", sep="\t"))
    print(propel_vec[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Mountain
    cat(paste("(Mountain_OUT)", sep="\t"))
    print(mountain[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste(";", sep="\t"))
    cat("\n")
    
    
    
    # HINDERING EVENTS
    cat(paste("name: HinderingEvents", k, "\n", sep=""))
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
    cat(paste("(Motion_IN)", sep="\t"))
    print(action_vec[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Propulsion Input
    cat(paste("(Propel_IN)", sep="\t"))
    print(propel_vec[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Mountain
    cat(paste("(Mountain_IN)", sep="\t"))
    print(mountain[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    
    
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
    
    # Motion Input
    cat(paste("(Motion_OUT)", sep="\t"))
    print(action_vec[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Propulsion Input
    cat(paste("(Propel_OUT)", sep="\t"))
    print(propel_vec[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Mountain
    cat(paste("(Mountain_OUT)", sep="\t"))
    print(mountain[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste(";", sep="\t"))
    cat("\n")
    
    k = k+1
  }
}
sink()



############
# test set #
############

sink('test.ex')
cat(paste("defT:-", "\n", sep=""))
k = 1
for(i in 1:nrow(hab_test_object_1)){
    # HELPER ENTITIES
    cat(paste("name: Helper", k, "\n", sep=""))
    cat(paste("I:", "\n", sep="\t"))
    
    # Person 1
    cat(paste("(Slot_A)", sep="\t"))
    print(hab_test_object_1[i,], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Eyes
    cat(paste("(Eyes_1)", sep="\t"))
    print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Memory Input
    cat(paste("(Memory_IN)", sep="\t"))
    print(memory_trace_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    
    
    cat(paste("T:", "\n", sep="\t"))
    # Person 1
    cat(paste("(Slot_A_OUT)", sep="\t"))
    print(hab_test_object_1[i,], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Eyes
    cat(paste("(Eyes_1_OUT)", sep="\t"))
    print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Interaction Output
    cat(paste("(Interaction_OUT)", sep="\t"))
    print(interaction_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    
    # Memory Output
    cat(paste("(Memory_OUT)", sep="\t"))
    print(memory_trace_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste(";", sep="\t"))
    cat("\n")
    
    
    
    # HINDERER ENTITIES
    cat(paste("name: Hinderer", k, "\n", sep=""))
    cat(paste("I:", "\n", sep="\t"))
    
    # Person 1
    cat(paste("(Slot_A)", sep="\t"))
    print(hab_test_object_3[i,], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Eyes
    cat(paste("(Eyes_1)", sep="\t"))
    print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Memory Input
    cat(paste("(Memory_IN)", sep="\t"))
    print(memory_trace_vec[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    
    
    cat(paste("T:", "\n", sep="\t"))
    # Person 1
    cat(paste("(Slot_A_OUT)", sep="\t"))
    print(hab_test_object_3[i,], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Eyes
    cat(paste("(Eyes_1_OUT)", sep="\t"))
    print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Interaction Output
    cat(paste("(Interaction_OUT)", sep="\t"))
    print(interaction_vec[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Memory Output
    cat(paste("(Memory_OUT)", sep="\t"))
    print(memory_trace_vec[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste(";", sep="\t"))
    cat("\n")
    k = k+1
  }
sink()


sink('test_control.ex')
cat(paste("defT:-", "\n", sep=""))
k = 1
for(i in 1:nrow(hab_test_object_1)){
    # HELPER ENTITIES
    cat(paste("name: Helper", k, "\n", sep=""))
    cat(paste("I:", "\n", sep="\t"))
    
    # Person 1
    cat(paste("(Slot_A)", sep="\t"))
    print(hab_test_object_1[i,], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Eyes
    cat(paste("(Eyes_1)", sep="\t"))
    print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Memory Input
    cat(paste("(Memory_IN)", sep="\t"))
    print(memory_trace_vec[3,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    
    
    cat(paste("T:", "\n", sep="\t"))
    # Person 1
    cat(paste("(Slot_A_OUT)", sep="\t"))
    print(hab_test_object_1[i,], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Eyes
    cat(paste("(Eyes_1_OUT)", sep="\t"))
    print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Interaction Input
    cat(paste("(Interaction_OUT)", sep="\t"))
    print(interaction_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    
    # Memory Output
    cat(paste("(Memory_OUT)", sep="\t"))
    print(memory_trace_vec[3,1], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste(";", sep="\t"))
    cat("\n")
    
    
    
    # HINDERER ENTITIES
    cat(paste("name: Hinderer", k, "\n", sep=""))
    cat(paste("I:", "\n", sep="\t"))
    
    # Person 1
    cat(paste("(Slot_A)", sep="\t"))
    print(hab_test_object_3[i,], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Eyes
    cat(paste("(Eyes_1)", sep="\t"))
    print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Memory Input
    cat(paste("(Memory_IN)", sep="\t"))
    print(memory_trace_vec[3,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    
    
    cat(paste("T:", "\n", sep="\t"))
    # Person 1
    cat(paste("(Slot_A_OUT)", sep="\t"))
    print(hab_test_object_3[i,], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Eyes
    cat(paste("(Eyes_1_OUT)", sep="\t"))
    print(eyes_vec[1,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Interaction Output
    cat(paste("(Interaction_OUT)", sep="\t"))
    print(interaction_vec[2,1], sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Memory Output
    cat(paste("(Memory_OUT)", sep="\t"))
    print(memory_trace_vec[3,1], sep = "\t", quote = FALSE, row.names = FALSE)
    cat(paste(";", sep="\t"))
    cat("\n")
    k = k+1
}
sink()