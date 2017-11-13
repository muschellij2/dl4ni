#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//


// [[Rcpp::export]]
NumericVector get_windows_at(NumericVector V, int width, IntegerVector x, IntegerVector y, IntegerVector z) {
  
  // if (width % 2 == 0) width++;
  int real_width = width;
  int disp = 0;
  if (width % 2 == 0) {
    
    real_width++;
    disp = 1;
    
  }
  
  int radius = (real_width + 1) / 2;
  
  IntegerVector dims = V.attr("dim");
  int n_volumes = 1;
  
  if (dims.size() >= 4) {
    
    n_volumes = dims[3];
    
  }
  
  int n_neighbours = pow(width, 3) * n_volumes;
  
  // Count how many windows we'll have
  int count = 0;
  
  NumericMatrix res(x.size(), n_neighbours + 3);
  
  // For every seed voxel, compute the window
  for (int i = 0; i < x.size(); i++) {
    
    res(count, 0) = x[i];
    res(count, 1) = y[i];
    res(count, 2) = z[i];
    
    int inner_count = 0;
    
    for (int volume = 0; volume < n_volumes; volume++) {
      
      for (int dz = -radius + 1; dz < radius - disp; dz++) {
        
        for (int dy = -radius + 1; dy < radius - disp; dy++) {
          
          for (int dx = -radius + 1; dx < radius - disp; dx++) {
            
            if ((x[i] + dx >= 0) & (x[i] + dx < dims[0]) & (y[i] + dy >= 0) & (y[i] + dy < dims[1]) & (z[i] + dz >= 0) & (z[i] + dz < dims[2])) {
              
              int offset = (x[i] + dx) + dims[0] * (y[i] + dy) + dims[0] * dims[1] * (z[i] + dz);
              offset += volume * dims[0] * dims[1] * dims[2];
              
              res(count, inner_count + 3) = V[offset];
              
            }
            
            inner_count++;
            
          }
          
        }
        
      }
      
    }
    
    count++;
    
  }
  
  return res;
  
}


// [[Rcpp::export]]
void results_to_volume(NumericVector V, 
                       int width,
                       NumericVector res, 
                       NumericVector counts, 
                       IntegerVector x, IntegerVector y, IntegerVector z) {
  
  // if (width % 2 == 0) width++;
  int real_width = width;
  int disp = 0;
  if (width % 2 == 0) {
    
    real_width++;
    disp = 1;
    
  }
  
  int radius = (real_width + 1) / 2;
  
  IntegerVector dims = V.attr("dim");
  IntegerVector target_dims = res.attr("dim");

  for (int i = 0; i < x.size(); i++) {
    
    int inner_count = 0;
    
    for (int dz = -radius + 1; dz < radius - disp; dz++) {
      
      for (int dy = -radius + 1; dy < radius - disp; dy++) {
        
        for (int dx = -radius + 1; dx < radius - disp; dx++) {
          
          if ((x[i] + dx >= 0) & (x[i] + dx < target_dims[0]) & (y[i] + dy >= 0) & (y[i] + dy < target_dims[1]) & (z[i] + dz >= 0) & (z[i] + dz < target_dims[2])) {
            
            int offset = (x[i] + dx) + target_dims[0] * (y[i] + dy) + target_dims[0] * target_dims[1] * (z[i] + dz);
            
            res[offset] += V[i + dims[0] * inner_count];
            counts[offset] += 1;
            
          }
          
          inner_count++;
          
        }
        
      }
      
    }
    
  }
  
}

// [[Rcpp::export]]
void results_to_volume_label(NumericVector V, 
                             int width,
                             NumericVector res,  
                             IntegerVector x, IntegerVector y, IntegerVector z) {
  
  // if (width % 2 == 0) width++;
  int real_width = width;
  int disp = 0;
  if (width % 2 == 0) {
    
    real_width++;
    disp = 1;
    
  }
  
  int radius = (real_width + 1) / 2;
  
  IntegerVector dims = V.attr("dim");
  IntegerVector target_dims = res.attr("dim");
  
  // Rprintf("Vector dims = (%u,%u)\n", dims[0], dims[1]);
  
  for (int i = 0; i < x.size(); i++) {
    
    int inner_count = 0;
    
    for (int dz = -radius + 1; dz < radius - disp; dz++) {
      
      for (int dy = -radius + 1; dy < radius - disp; dy++) {
        
        for (int dx = -radius + 1; dx < radius - disp; dx++) {
          
          int offset = (x[i] + dx) + target_dims[0] * (y[i] + dy) + target_dims[0] * target_dims[1] * (z[i] + dz);
          
          if (V[i + dims[0] * inner_count] > res[offset])
            res[offset] = V[i + dims[0] * inner_count];
          inner_count++;
          
        }
        
      }
      
    }
    
  }
  
}

// [[Rcpp::export]]
void results_to_volume_label_with_distance(NumericVector V, 
                                           int width,
                                           NumericVector res,  
                                           NumericVector last_distance,
                                           IntegerVector x, IntegerVector y, IntegerVector z) {
  
  // if (width % 2 == 0) width++;
  int real_width = width;
  int disp = 0;
  if (width % 2 == 0) {
    
    real_width++;
    disp = 1;
    
  }
  
  int radius = (real_width + 1) / 2;
  
  IntegerVector dims = V.attr("dim");
  IntegerVector target_dims = res.attr("dim");
  
  // Rprintf("Vector dims = (%u,%u)\n", dims[0], dims[1]);
  
  for (int i = 0; i < x.size(); i++) {
    
    int inner_count = 0;
    
    for (int dz = -radius + 1; dz < radius - disp; dz++) {
      
      for (int dy = -radius + 1; dy < radius - disp; dy++) {
        
        for (int dx = -radius + 1; dx < radius - disp; dx++) {
          
          if ((x[i] + dx >= 0) & (x[i] + dx < target_dims[0]) & (y[i] + dy >= 0) & (y[i] + dy < target_dims[1]) & (z[i] + dz >= 0) & (z[i] + dz < target_dims[2])) {
            
            int offset = (x[i] + dx) + target_dims[0] * (y[i] + dy) + target_dims[0] * target_dims[1] * (z[i] + dz);
            
            if (dx * dx + dy * dy + dz * dz < last_distance[offset]) {
              
              res[offset] = V[i + dims[0] * inner_count];
              last_distance[offset] = dx * dx + dy * dy + dz * dz;
              
            }
            
          }
          
          inner_count++;
          
        }
        
      }
      
    }
    
  }
  
}


void regularize(double* input, int ndims, int* dims, double* kernel, int kernel_ndims, int kernel_width, double* output) {
  
  // We should have (ndims == kernel_ndims) or (ndims == kernel_ndims + 1)
  //
  
  if (ndims == 3) {
    
    if (kernel_width % 2 == 1) kernel_width--;
    int search_limit = kernel_width / 2;
    int limit = search_limit + 1;
    
    for (int x = limit; x < dims[0] - limit; x++) {
      
      for (int y = limit; y < dims[1] - limit; y++) {
        
        for (int z = limit; z < dims[2] - limit; z++) {
          
          int voxel = x + dims[0] * y + dims[0] * dims[1] * z;
          
          double cumul = 0.0;
          
          for (int xdisp = -search_limit; xdisp < search_limit; xdisp++) {
            
            for (int ydisp = -search_limit; ydisp < search_limit; ydisp++) {
              
              for (int zdisp = -search_limit; zdisp < search_limit; zdisp++) {
                
                int loc_kernel = (xdisp + search_limit) + kernel_width * (ydisp + search_limit) + kernel_width * kernel_width * (zdisp + search_limit);
                int loc_image = (x + xdisp) + dims[0] * (y + ydisp) + dims[0] * dims[1] * (z + zdisp);
                cumul += input[loc_image] * kernel[loc_kernel];
                
              }
              
            }
            
          }
          
          output[voxel] = cumul;
          
        }
        
      }
      
    }
    
  } else {
    
    if (kernel_width % 2 == 1) kernel_width--;
    int search_limit = kernel_width / 2;
    int limit = search_limit + 1;
    
    for (int x = limit; x < dims[0] - limit; x++) {
      
      for (int y = limit; y < dims[1] - limit; y++) {
        
        for (int z = limit; z < dims[2] - limit; z++) {
          
          for (int k = 0; k < dims[3]; k++) {
            
            int voxel = x + dims[0] * y + dims[0] * dims[1] * z +  dims[0] * dims[1] * dims[2] * k;
            
            double cumul = 0.0;
            
            for (int xdisp = -search_limit; xdisp < search_limit; xdisp++) {
              
              for (int ydisp = -search_limit; ydisp < search_limit; ydisp++) {
                
                for (int zdisp = -search_limit; zdisp < search_limit; zdisp++) {
                  
                  int loc_kernel = (xdisp + search_limit) + kernel_width * (ydisp + search_limit) + kernel_width * kernel_width * (zdisp + search_limit);
                  int loc_image = (x + xdisp) + dims[0] * (y + ydisp) + dims[0] * dims[1] * (z + zdisp) +  dims[0] * dims[1] * dims[2] * k;
                  cumul += input[loc_image] * kernel[loc_kernel];
                  
                }
                
              }
              
            }
            
            output[voxel] = cumul;
            
          }
          
        }
        
      }
      
    }
    
  }
  
}


//[[Rcpp::export]]
NumericVector regularize(NumericVector image, NumericVector kernel) {
  
  IntegerVector dims = image.attr("dim");
  int ndims = dims.size();
  
  IntegerVector kernel_dims = kernel.attr("dim");
  int kernel_ndims = kernel_dims.size();
  int kernel_width = kernel_dims[0];
  
  NumericVector segmentation(image.size());
  
  regularize(image.begin(), ndims, dims.begin(), kernel.begin(), kernel_ndims, kernel_width, segmentation.begin());
  
  segmentation.attr("dim") = dims;
  
  return segmentation;
  
}



void which_max(double* input, int ndims, int* dims, double* output) {
  
  // We should have (ndims == kernel_ndims) or (ndims == kernel_ndims + 1)
  //
  
  if (ndims == 3) {
    
    for (int x = 0; x < dims[0]; x++) {
      
      for (int y = 0; y < dims[1]; y++) {
        
        double max;
        int pos = 0;
        
        for (int z = 0; z < dims[2]; z++) {
          
          int voxel = x + dims[0] * y + dims[0] * dims[1] * z;
          
          if (z == 0) {
            
            max = input[voxel];
            
          }
          
          if (max < input[voxel]) {
            
            max = input[voxel];
            pos = z;
            
          }
          
        }
        
        output[x + dims[0] * y] = (double)pos;
        
      }
      
    }
    
  } else {
    
    
    for (int x = 0; x < dims[0]; x++) {
      
      for (int y = 0; y < dims[1]; y++) {
        
        for (int z = 0; z < dims[2]; z++) {
          
          double max;
          int pos = 0;
          
          for (int k = 0; k < dims[3]; k++) {
            
            int voxel = x + dims[0] * y + dims[0] * dims[1] * z +  dims[0] * dims[1] * dims[2] * k;
            
            if (k == 0) {
              
              max = input[voxel];
              
            }
            
            if (max < input[voxel]) {
              
              max = input[voxel];
              pos = k;
              
            }
            
          }
          
          output[x + dims[0] * y + dims[0] * dims[1] * z ] = (double)pos;
          
          
        }
        
      }
      
    }
    
  }
  
}

//[[Rcpp::export]]
NumericVector which_max(NumericVector image) {
  
  IntegerVector dims = image.attr("dim");
  int ndims = dims.size();
  int image_size = image.size();
  int new_size = (int)(image_size / dims[ndims - 1]);
  IntegerVector new_dims(ndims - 1);
  for (int d = 0; d < ndims - 1; d++) {
    
    new_dims[d] = dims[d];
    
  }
  
  NumericVector segmentation(new_size);
  
  which_max(image.begin(), ndims, dims.begin(), segmentation.begin());
  
  segmentation.attr("dim") = new_dims;
  
  return segmentation;
  
}

void to_categorical_volume_cpp(double* image, int ndims, int* dims, int* unique_labels, int n_classes, int* segmentation) {
  
  if (ndims == 3) {
    
    for (int x = 0; x < dims[0]; x++) {
      
      for (int y = 0; y < dims[1]; y++) {
        
        for (int z = 0; z < dims[2]; z++) {
          
          int voxel = x + dims[0] * y + dims[0] * dims[1] * z;
          
          int k = (int)image[voxel];
          int target_voxel = x + dims[0] * y + dims[0] * dims[1] * z +  dims[0] * dims[1] * dims[2] * k;
          segmentation[target_voxel] = 1;
          
        }
        
      }
      
    }
    
  }
  
  if (ndims == 4) {
    
    // First dimension is batch_size
    
    for (int batch = 0; batch < dims[0]; batch++) {
      
      for (int x = 0; x < dims[1]; x++) {
        
        for (int y = 0; y < dims[2]; y++) {
          
          for (int z = 0; z < dims[3]; z++) {
            
            int voxel = batch + x * dims[0] + dims[0] * dims[1] * y + dims[0] * dims[1] * dims[2] * z;
            
            int k = (int)image[voxel];
            int target_voxel = voxel +  dims[0] * dims[1] * dims[2] * dims[3] * k;
            segmentation[target_voxel] = 1;
            
          }
          
        }
        
      }
      
      
    }
    
  }
  
  
}


//[[Rcpp::export]]
IntegerVector to_categorical_volume_cpp(NumericVector image, IntegerVector unique_labels) {
  
  IntegerVector dims = image.attr("dim");
  int ndims = dims.size();
  int image_size = image.size();
  
  int n_classes = unique_labels.size();
  
  IntegerVector new_dims(ndims + 1);
  int new_size = (int)(image_size * n_classes);
  
  for (int d = 0; d < ndims; d++) {
    
    new_dims[d] = dims[d];
    
  }
  new_dims[ndims] = n_classes;
  
  IntegerVector segmentation(new_size);
  
  to_categorical_volume_cpp(image.begin(), ndims, dims.begin(), unique_labels.begin(), n_classes, segmentation.begin());
  
  segmentation.attr("dim") = new_dims;
  
  return segmentation;
  
  
}
