#################################################################
#
#   MASH
#   MICRO Mosquito Populations
#   Class Definitions
#   David Smith, Hector Sanchez, Sean Wu
#   April 28, 2017
#
#################################################################


#################################################################
# Female Mosquito Population Class
#################################################################

# MicroMosquitoPopFemale:
MicroMosquitoPopFemale <- R6::R6Class(classname = "MicroMosquitoPopFemale",
                       portable = TRUE,
                       cloneable = FALSE,
                       lock_class = FALSE,
                       lock_objects = FALSE,

                       public = list(

                         ##############################################################
                         # Initializer
                         ##############################################################

                         # N: size of initial cohort
                         # ix_init should be a vector of initial site index
                         # genotype_init should be a vector of genotypes
                         # movement: movement object from MicroKernel_exactAll
                         # directory: directory to output data
                         initialize = function(N, time_init, ix_init, genotype_init, MBITES_PAR, module, movement, directory){

                             # Initialize population level fields prior to allocating container
                             private$MBITES_PAR = MBITES_PAR
                             private$movement = movement
                             private$directory = directory
                             switch(module,
                                BRO = {private$initState = "B"},
                                BROM = {private$initState = "M"},
                                BROS = {private$initState = "B"},
                                BROMS = {private$initState = "M"},
                                FULL = {private$initState = "M"},
                                {stop("unrecognized M-BITES lifecycle module selection")}
                              )

                             # allocate population
                             private$pop = vector(mode="list",length=(N*5))
                             if(length(ix_init) != length(genotype_init)){
                               stop("one or more of the input vectors to MicroMosquitoPopFemale initializer is not the same length")
                             }

                             # allocate initial cohort
                             for(ix in 1:N){
                               private$pop[[ix]] = MicroMosquitoFemale$new(id = paste0(time_init,"_",ix), time = time_init, ix = ix_init[ix], genotype = genotype_init[ix], state = private$initState)
                               private$pop[[ix]]$set_FemalePopPointer(self)
                             }
                             # find NULL indices
                             self$update_nullPop()

                         }, # end initializer

                         #################################################################
                         # Getters & Setters
                         #################################################################

                         get_MosquitoIxM = function(ixM = NULL){
                           if(is.null(ixM)){
                             return(private$pop)
                           } else {
                             return(private$pop[[ixM]])
                           }
                         },
                         get_MosquitoID = function(ID){
                           stop("this hasn't been written yet")
                         },

                         which_alive = function(){
                           return(
                             sum(vapply(X = private$pop, FUN = function(x){
                                  if(is.null(x)){
                                    return(FALSE)
                                  } else {
                                    if(x$isAlive()){
                                      return(TRUE)
                                    } else {
                                      return(FALSE)
                                    }
                                  }
                               }, FUN.VALUE = logical(1)))
                            )
                         },

                         # getter for nullPop
                         get_nullPop = function(){return(private$nullPop)},
                         # update nullPop
                         update_nullPop = function(){
                           private$nullPop = which(vapply(X = private$pop,FUN = is.null,FUN.VALUE = logical(1)))
                         },

                         # generic get_movement method; designed to be overwritten by module-specific parameteric getter
                         get_movement = function(){
                           return(private$movement)
                         },
                         set_movment = function(movement){
                           private$movement = movement
                         },

                         # return parameter list or element of list by name
                         get_MBITES_PAR = function(ixP = NULL){
                           if(is.null(ixP)){
                             return(private$MBITES_PAR)
                           } else {
                             return(private$MBITES_PAR[[ixP]])
                           }
                         },
                         set_MBITES_PAR = function(MBITES_PAR){
                           private$MBITES_PAR = MBITES_PAR
                         },

                         get_directory = function(){
                           return(private$directory)
                         },
                         set_directory = function(directory){
                           private$directory = directory
                         },

                         #################################################################
                         # Pointers
                         #################################################################

                         get_MalePopPointer = function(){
                           return(private$MalePopPointer)
                         },
                         set_MalePopPointer = function(MalePopPointer){
                           private$MalePopPointer = MalePopPointer
                         },

                         get_LandscapePointer = function(){
                           return(private$LandscapePointer)
                         },
                         set_LandscapePointer = function(LandscapePointer){
                           private$LandscapePointer = LandscapePointer
                         },

                         get_HumansPointer = function(){
                           return(private$HumansPointer)
                         },
                         set_HumansPointer = function(HumansPointer){
                           private$HumansPointer = HumansPointer
                         },

                         get_TilePointer = function(){
                           return(private$TilePointer)
                         },
                         set_TilePointer = function(TilePointer){
                           private$TilePointer = TilePointer
                         },

                         ##############################################################
                         # Cohort Methods
                         ##############################################################

                         # push_pop: from a single ImagoSlot; add a cohort.
                         push_pop = function(N, tEmerge, ix, genotype, damID, sireID){
                           if(N >= length(nullPop)){
                             self$extend_pop()
                           }
                           for(i in 1:N){
                             private$pop[[private$nullPop[i]]] = MicroMosquitoFemale$new(id = paste0(tEmerge,"_",nullPop[i]), time = tEmerge, ix = ix, genotype = genotype, state = private$initState)
                             private$pop[[private$nullPop[i]]]$set_FemalePopPointer(self)
                             private$pop[[private$nullPop[i]]]$set_MalePopPointer(private$MalePopPointer)
                             private$pop[[private$nullPop[i]]]$set_LandscapePointer(private$LandscapePointer)
                             private$pop[[private$nullPop[i]]]$set_HumansPointer(private$HumansPointer)
                             private$pop[[private$nullPop[i]]]$set_TilePointer(private$TilePointer)
                           }
                         },

                         # extend_pop: extend the pop vecor
                         extend_pop = function(){

                           N = length(private$pop)
                           extendN = N*2

                           for(ix in (N+1):extendN){
                             private$pop[[ix]] = NULL
                           }

                         },

                         # clear_pop: manage the pop vector (find dead mosquitoes; if 'con' is provided, write their histories out to JSON)
                         clear_pop = function(historyTrack = FALSE, bionomicsTrack = FALSE){

                           deadIx = which(vapply(X = private$pop, FUN = function(x){
                                if(is.null(x)){
                                  return(FALSE)
                                } else {
                                  if(x$isAlive()){
                                    return(FALSE)
                                  } else {
                                    return(TRUE)
                                  }
                                }
                             }, FUN.VALUE = logical(1)))

                          if(historyTrack){
                            histories = vector(mode="list",length=length(deadIx))
                            names(histories) = vapply(X = private$pop, FUN = function(x){x$get_id()}, FUN.VALUE = character(1))
                          }
                          if(bionomicsTrack){
                            bionomics = vector(mode="list",length=length(deadIx))
                            names(bionomics) = vapply(X = private$pop, FUN = function(x){x$get_id()}, FUN.VALUE = character(1))
                          }

                          for(ix in deadIx){
                            if(historyTrack){
                              histories[[ix]] = private$pop[[ix]]$get_history()
                            }
                            if(bionomicsTrack){
                              bionomics[[ix]] = private$pop[[ix]]$get_bionomics()
                            }
                            private$pop[[ix]] = NULL
                          }

                          # write the list out to JSON
                          if(historyTrack){
                            fileName = paste0("historyF",private$TilePointer$get_tNow(),".json")
                            con = file(description = paste0(private$directory,"MOSQUITO/",fileName),open = "wt")
                            writeLines(text = jsonlite::toJSON(x = histories,pretty = TRUE),con = con)
                            close(con)
                          }
                          if(bionomicsTrack){
                            fileName = paste0("bionomics",private$TilePointer$get_tNow(),".json")
                            con = file(description = paste0(private$directory,"MOSQUITO/",fileName),open = "wt")
                            writeLines(text = jsonlite::toJSON(x = histories,pretty = TRUE),con = con)
                            close(con)
                          }

                          # update_nullPop
                          self$update_nullPop()

                         }

                       ),

                       private = list(

                         # Fields
                         pop = NULL,               # mosquito population
                         nullPop = NULL,           # null entries in list for memory allocation
                         movement = NULL,          # movement object (type depends on specific SEARCH module)
                         initState = NULL,         # initial state for newly emerging females
                         MBITES_PAR = NULL,        # MBITES Parameters
                         directory = NULL,         # directory to export data

                         # Pointers
                         MalePopPointer = NULL,    # Point to MicroMosquitoPopMale in the same microsimulation Tile
                         LandscapePointer = NULL,  # Point to Landscape object in same microsimulation Tile
                         HumansPointer = NULL,     # Point to HumanPop object in same microsimulation Tile
                         TilePointer = NULL        # Point to enclosing microsimulation Tile

                       )
)


#################################################################
# Male Mosquito Population Class
#################################################################

MicroMosquitoPopMale <- R6::R6Class(classname = "MicroMosquitoPopMale",
                             portable = TRUE,
                             cloneable = FALSE,
                             lock_class = FALSE,
                             lock_objects = FALSE,

                             public = list(

                               #################################################################
                               # Getters & Setters
                               #################################################################

                               get_MosquitoIxM = function(ixM = NULL){
                                 if(is.null(ixM)){
                                   return(private$pop)
                                 } else {
                                   return(private$pop[[ixM]])
                                 }
                               },
                               get_MosquitoID = function(ID){
                                 stop("this hasn't been written yet")
                               },

                               # getter for nullPop
                               get_nullPop = function(){return(private$nullPop)},
                               # update nullPop
                               update_nullPop = function(){
                                 private$nullPop = which(vapply(X = private$pop,FUN = is.null,FUN.VALUE = logical(1)))
                               },

                               # generic get_movement method; designed to be overwritten by module-specific parameteric getter
                               get_movement = function(){
                                 return(private$movement)
                               },
                               set_movment = function(movement){
                                 private$movement = movement
                               },

                               # return parameter list or element of list by name
                               get_MBITES_PAR = function(ixP = NULL){
                                 if(is.null(ixP)){
                                   return(private$MBITES_PAR)
                                 } else {
                                   return(private$MBITES_PAR[[ixP]])
                                 }
                               },
                               set_MBITES_PAR = function(MBITES_PAR){
                                 private$MBITES_PAR = MBITES_PAR
                               },

                               #################################################################
                               # Pointers
                               #################################################################

                               get_LandscapePointer = function(){
                                 return(private$LandscapePointer)
                               },
                               set_LandscapePointer = function(LandscapePointer){
                                 private$LandscapePointer = LandscapePointer
                               },

                               get_HumansPointer = function(){
                                 return(private$HumansPointer)
                               },
                               set_HumansPointer = function(HumansPointer){
                                 private$HumansPointer = HumansPointer
                               },

                               get_TilePointer = function(){
                                 return(private$TilePointer)
                               },
                               set_TilePointer = function(TilePointer){
                                 private$TilePointer = TilePointer
                               }


                             ),

                             private = list(

                               # Fields
                               pop = NULL,               # mosquito population
                               nullPop = NULL,           # null entries in list for memory allocation
                               movement = NULL,          # movement object (type depends on specific SEARCH module)
                               MBITES_PAR = NULL,        # MBITES Parameters

                               # Pointers
                               LandscapePointer = NULL,  # Point to Landscape object in same microsimulation Tile
                               HumansPointer = NULL,     # Point to HumanPop object in same microsimulation Tile
                               TilePointer = NULL        # Point to enclosing microsimulation Tile

                             )
)
