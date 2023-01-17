library(fddm);
library(dplyr);


ll_fun <- function(pars, rt, resp, truth, err_tol) {
  #likelihood function with all parameters allowing to vary
  v <- numeric(length(rt))

  # the truth is "upper" so use vu
  v[truth == "upper"] <- pars[[1]]
  # the truth is "lower" so use vl
  v[truth == "lower"] <- pars[[2]]

  dens <- dfddm(rt = rt, response = resp, a = pars[[3]], v = v, t0 = pars[[4]],
                w = pars[[5]], sv = 0, err_tol = 1e-6, log = TRUE)

  return( ifelse(any(!is.finite(dens)), 1e6, -sum(dens)) )
}

ll_fun2 <- function(a) {

  return(
    function(pars, rt, resp, truth, err_tol) {
      v <- numeric(length(rt))

      # the truth is "upper" so use vu
      v[truth == "upper"] <- pars[[1]]
      # the truth is "lower" so use vl
      v[truth == "lower"] <- pars[[2]]

      dens <- dfddm(rt = rt, response = resp, a = a, v = v, t0 = pars[[3]],
                    w = pars[[4]], sv = 0, err_tol = 1e-6, log = TRUE)

      return( ifelse(any(!is.finite(dens)), 1e6, -sum(dens)) )
    }
  )

}


rt_fit_step1 <- function(data) {

  # expects the following

  df <- data # assume input data is already formatted

  # Preliminaries
  ids <- unique(df[["id"]])
  nids <- max(length(ids), 1) # if inds is null, there is only one individual
  ninit_vals <- 5

  # Initilize the output dataframe
  cnames <- c("ID", "Convergence", "Objective",
              "vu_fit", "vl_fit", "a_fit", "t0_fit", "w_fit")
  out <- data.frame(matrix(ncol = length(cnames), nrow = nids))
  colnames(out) <- cnames
  temp <- data.frame(matrix(ncol = length(cnames)-1, nrow = ninit_vals))
  colnames(temp) <- cnames[-1]

  # Loop through each individual and starting values
  for (i in 1:nids) {
    out[["ID"]][i] <- ids[i]

    # extract data for id i
    dfi <- df[df[["id"]] == ids[i],]
    rti <- dfi[["rt"]]
    respi <- dfi[["response"]]
    truthi <- dfi[["truth"]]

    # starting value for t0 must be smaller than the smallest rt
    min_rti <- min(rti)

    # create initial values for this individual
    init_vals <- data.frame(vu = rnorm(n = ninit_vals, mean = 4, sd = 2),
                            vl = rnorm(n = ninit_vals, mean = -4, sd = 2),
                            a  = runif(n = ninit_vals, min = 0.5, max = 5),
                            t0 = runif(n = ninit_vals, min = 0, max = min_rti),
                            w  = runif(n = ninit_vals, min = 0, max = 1))

    # loop through all of the starting values
    for (j in 1:ninit_vals) {
      mres <- nlminb(init_vals[j,], ll_fun,
                     rt = rti, resp = respi, truth = truthi,
                     # limits:   vu,   vl,   a,  t0, w
                     lower = c(-Inf, -Inf, .01,   0, 0),
                     upper = c( Inf,  Inf, Inf, Inf, 1))
      temp[["Convergence"]][j] <- mres[["convergence"]]
      temp[["Objective"]][j] <- mres[["objective"]]
      temp[j, -c(1, 2)] <- mres[["par"]]
    }

    # determine best fit for the individual
    min_idx <- which.min(temp[["Objective"]])
    out[i, -1] <- temp[min_idx,]
  }
  return(out)
}

rt_fit_step2 <- function(data, fit1) {

  # expects the following

  df <- data # assume input data is already formatted

  # Preliminaries
  ids <- unique(df[["id"]])
  nids <- max(length(ids), 1) # if inds is null, there is only one individual
  ninit_vals <- 5

  # Initilize the output dataframe
  cnames <- c("ID", "Convergence", "Objective",
              "vu_fit", "vl_fit", "t0_fit", "w_fit")
  out <- data.frame(matrix(ncol = length(cnames), nrow = nids))
  colnames(out) <- cnames
  temp <- data.frame(matrix(ncol = length(cnames)-1, nrow = ninit_vals))
  colnames(temp) <- cnames[-1]

  # Loop through each individual and starting values
  for (i in 1:nids) {
    out[["ID"]][i] <- ids[i]

    # extract data for id i
    dfi <- df[df[["id"]] == ids[i],]
    rti <- dfi[["rt"]]
    respi <- dfi[["response"]]
    truthi <- dfi[["truth"]]

    # starting value for t0 must be smaller than the smallest rt
    min_rti <- min(rti)

    # create initial values for this individual
    init_vals <- data.frame(vu = rnorm(n = ninit_vals, mean = 4, sd = 2),
                            vl = rnorm(n = ninit_vals, mean = -4, sd = 2),
                            t0 = runif(n = ninit_vals, min = 0, max = min_rti),
                            w  = runif(n = ninit_vals, min = 0, max = 1))

    #obtain a value for the individual from the previous fit
    this_subj = strsplit(ids[i],'[.]')[[1]][1]
    this_a = fit1%>%filter(ID==this_subj)%>%pull(a_fit)


    # loop through all of the starting values
    for (j in 1:ninit_vals) {
      mres <- nlminb(init_vals[j,], ll_fun2(this_a),
                     rt = rti, resp = respi, truth = truthi,
                     # limits:   vu,   vl,  t0, w
                     lower = c(-Inf, -Inf,   0, 0),
                     upper = c( Inf,  Inf, Inf, 1))
      temp[["Convergence"]][j] <- mres[["convergence"]]
      temp[["Objective"]][j] <- mres[["objective"]]
      temp[j, -c(1, 2)] <- mres[["par"]]
    }


    # determine best fit for the individual
    min_idx <- which.min(temp[["Objective"]])
    out[i, -1] <- temp[min_idx,]
  }
  return(out)
}

# E1.ddmdf <- E1.df %>%
#   filter(test_part %in% c('neutral','bird_loss','bird_bonus') & trial_type=='Disc') %>%
#   filter(RT>100 & RT<5000)%>%
#   mutate(id=paste(subj_id,test_part,sep='.'),
#          rt=RT/1000,
#          response=ifelse(resp,'upper','lower'),
#          truth=ifelse(stimulus=='bird','upper','lower')
#   ) %>%
#   dplyr::select(id,rt,response,truth,test_part, subj_id)

E1.ddmdf <- subj1 %>%
    mutate(id=paste(subj_idx,condition,sep='.'),
           response=ifelse(response=='bird','upper','lower'),
           truth=ifelse(stim=='bird','upper','lower'),
           test_part=condition,
           subj_id=subj_idx
    ) %>%
    dplyr::select(id,rt,response,truth,test_part, subj_id)

E1.initial_fit <- rt_fit_step1(E1.ddmdf %>%
                                 mutate(id=subj_id))

E1.fit <-rt_fit_step2(E1.ddmdf,
                      E1.initial_fit) %>%
  rowwise() %>%
  mutate(subj_id=strsplit(ID,'[.]')[[1]][1],
         test_part=strsplit(ID,'[.]')[[1]][2])

E1.starting_point <- E1.fit %>%
  dplyr::select(subj_id,test_part,w_fit) %>%
  spread(test_part,w_fit) %>%
  mutate(bonus_effect = bird_bonus - neutral,
         loss_effect = bird_loss - neutral)


E1.vu <- E1.fit %>%
  dplyr::select(subj_id,test_part,vu_fit) %>%
  spread(test_part,vu_fit) %>%
  mutate(bonus_effect = bird_bonus - neutral,
         loss_effect = bird_loss - neutral)

E1.vl <- E1.fit %>%
  dplyr::select(subj_id,test_part,vl_fit) %>%
  spread(test_part,vl_fit) %>%
  mutate(bonus_effect = bird_bonus - neutral,
         loss_effect = bird_loss - neutral)

E1.t0 <- E1.fit %>%
  dplyr::select(subj_id,test_part,t0_fit) %>%
  spread(test_part,t0_fit) %>%
  mutate(bonus_effect = bird_bonus - neutral,
         loss_effect = bird_loss - neutral)
