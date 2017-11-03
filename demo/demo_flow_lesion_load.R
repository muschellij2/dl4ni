flow <- create_flow(name = "lesion_load", 
                    inputs = list("T1", "FLAIR"))

flow %>% add_process(proc = function(T1) {return(TRUE)}, 
                     output = "T1_corrected")

flow %>% add_process(proc = function(FLAIR) {return(TRUE)}, 
                     output = "FLAIR_corrected")

flow %>% add_process(proc = function(FLAIR_corrected, T1_corrected) {return(TRUE)}, 
                     output = "FLAIR_in_T1_space")

flow %>% add_process(proc = function(T1_corrected) {return(TRUE)}, 
                     output = "brain_mask")

flow %>% add_process(proc = function(T1_corrected, brain_mask) {return(TRUE)}, 
                     output = "only_brain")

flow %>% add_process(proc = function(only_brain) {return(TRUE)}, 
                     output = "segmentation")

flow %>% add_process(proc = function(segmentation, FLAIR_in_T1_space) {return(TRUE)}, 
                     output = "WM_FLAIR")

flow %>% add_process(proc = function(WM_FLAIR) {return(TRUE)}, 
                     output = "lesion_map")

flow %>% add_process(proc = function(only_brain, segmentation) {return(TRUE)}, 
                     output = "parcellation")

flow %>% add_process(proc = function(lesion_map, parcellation) {return(TRUE)}, 
                     output = "lesion_by_region")

flow %>% plot_flow()

