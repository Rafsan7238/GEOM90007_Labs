# Define a function named "cluster_trajectories" that clusters time series data.
# Parameters:
#   - data: Input data frame containing time series data
#   - k: Number of clusters to create
#   - normalize: Boolean flag indicating whether to normalize the data (default is FALSE)
cluster_trajectories <- function(data, k, normalize = FALSE) {
  
  # Transpose the input data for clustering
  data_trajectories_trans <- t(data[, .SD,
                                    .SDcols = colnames(data)[-1]])
  
  # Create a list of time series trajectories, removing rows with NAs
  data_trajectories_trans_list <- lapply(1:nrow(data_trajectories_trans), function(i) na.omit(data_trajectories_trans[i,]))
  names(data_trajectories_trans_list) <- colnames(data)[-1]
  
  # Calculate the length of each trajectory and store it in a named list
  n_list <- sapply(1:length(data_trajectories_trans_list), function(i) length(data_trajectories_trans_list[[i]]))
  names(n_list) <- names(data_trajectories_trans_list)
  
  # Remove trajectories with zero or one data point
  if (length(which(n_list %in% 0:1)) != 0) {
    data_trajectories_trans_list <- data_trajectories_trans_list[-which(n_list %in% 0:1)]
  }
  
  # Store the names of the remaining trajectories in a variable
  list_names <- names(data_trajectories_trans_list)
  
  # Normalize the data if requested
  if (normalize) {
    data_trajectories_trans_list <- lapply(names(data_trajectories_trans_list),
                                           function(i) norm_z(data_trajectories_trans_list[[i]]))
    names(data_trajectories_trans_list) <- list_names
  }
  
  # Perform hierarchical clustering on the normalized time series data
  hc_res <- tsclust(data_trajectories_trans_list,
                    type = "hierarchical",
                    k = k,
                    distance = "dtw_basic", # Distance metric for DTW
                    centroid = dba,         # Centroid calculation method
                    trace = FALSE,
                    control = hierarchical_control(method = "ward.D2"),  # Clustering method
                    args = tsclust_args(dist = list(norm = "L2"))        # Arguments for DTW
  )
  
  # Return the hierarchical clustering result
  return(hc_res)
}