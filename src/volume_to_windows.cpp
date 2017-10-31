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
NumericVector volume_to_windows(NumericVector V, int stride, int width, int displacement = 0) {
  
  if (width % 2 == 0) width++;
  
  int radius = (width + 1) / 2;
  
  IntegerVector dims = V.attr("dim");
  
  int n_neighbours = pow(width, 3);
  
  
  // Count how many windows we'll have
  int count = 0;
  for (int z = displacement + radius - 1; z < dims[2] - width + radius; z += stride) {
    
    for (int y = displacement + radius - 1; y < dims[1] - width + radius; y += stride) {
      
      for (int x = displacement + radius - 1; x < dims[0] - width + radius; x += stride) {
        
        count++;
        
      }
      
    }
    
  }
  
  
  NumericMatrix res(count, n_neighbours + 3);
  count = 0;
  // For every seed voxel, compute the window
  for (int z = displacement + radius - 1; z < dims[2] - width + radius - 1; z += stride) {
    
    for (int y = displacement + radius - 1; y < dims[1] - width + radius - 1; y += stride) {
      
      for (int x = displacement + radius - 1; x < dims[0] - width + radius - 1; x += stride) {
        
        res(count, 0) = x;
        res(count, 1) = y;
        res(count, 2) = z;
        
        int inner_count = 0;
        
        for (int dz = -radius + 1; dz < radius; dz++) {
          
          for (int dy = -radius + 1; dy < radius; dy++) {
            
            for (int dx = -radius + 1; dx < radius; dx++) {
              
              int offset = (x + dx) + dims[0] * (y + dy) + dims[0] * dims[1] * (z + dz);
              
              res(count, inner_count + 3) = V[offset];
              inner_count++;
              
            }
            
          }
          
        }
        
        count++;
        
      }
      
    }
    
  }
  
  return res;
  
}


// [[Rcpp::export]]
NumericVector get_windows_at(NumericVector V, int width, IntegerVector x, IntegerVector y, IntegerVector z) {
  
  if (width % 2 == 0) width++;
  
  int radius = (width + 1) / 2;
  
  IntegerVector dims = V.attr("dim");
  
  int n_neighbours = pow(width, 3);
  
  // Count how many windows we'll have
  int count = 0;
  
  NumericMatrix res(x.size(), n_neighbours + 3);
  
  // For every seed voxel, compute the window
  for (int i = 0; i < x.size(); i++) {
    
    res(count, 0) = x[i];
    res(count, 1) = y[i];
    res(count, 2) = z[i];
    
    int inner_count = 0;
    
    for (int dz = -radius + 1; dz < radius; dz++) {
      
      for (int dy = -radius + 1; dy < radius; dy++) {
        
        for (int dx = -radius + 1; dx < radius; dx++) {
          
          if ((x[i] + dx >= 0) & (x[i] + dx < dims[0]) & (y[i] + dy >= 0) & (y[i] + dy < dims[1]) & (z[i] + dz >= 0) & (z[i] + dz < dims[2])) {
            
            int offset = (x[i] + dx) + dims[0] * (y[i] + dy) + dims[0] * dims[1] * (z[i] + dz);
            
            res(count, inner_count + 3) = V[offset];
            
          }
          
          inner_count++;
          
        }
        
      }
      
    }
    
    count++;
    
  }
  
  return res;
  
}


// [[Rcpp::export]]
NumericVector windows_to_volume(NumericVector V, int stride, int width, IntegerVector target_dims,
                                int displacement = 0) {
  
  if (width % 2 == 0) width++;
  
  int radius = (width + 1) / 2;
  
  IntegerVector dims = V.attr("dim");
  
  Rprintf("Vector dims = (%u,%u)\n", dims[0], dims[1]);
  
  NumericVector res(target_dims[0] * target_dims[1] * target_dims[2], 0.0);
  NumericVector counts(target_dims[0] * target_dims[1] * target_dims[2], 0.0);
  
  int count = 0;
  // For every seed voxel, compute the window
  for (int z = displacement + radius - 1; z < target_dims[2] - width + radius - 1; z += stride) {
    
    for (int y = displacement + radius - 1; y < target_dims[1] - width + radius - 1; y += stride) {
      
      for (int x = displacement + radius - 1; x < target_dims[0] - width + radius - 1; x += stride) {
        
        int inner_count = 0;
        
        for (int dz = -radius + 1; dz < radius; dz++) {
          
          for (int dy = -radius + 1; dy < radius; dy++) {
            
            for (int dx = -radius + 1; dx < radius; dx++) {
              
              int offset = (x + dx) + target_dims[0] * (y + dy) + target_dims[0] * target_dims[1] * (z + dz);
              
              if (offset >= target_dims[0] * target_dims[1] * target_dims[2] ) {
                
                Rprintf("Offset = %u\n", offset);
                Rprintf("Index = %u\n", count + dims[0] * inner_count);
                
              }
              
              // if (V[count + dims[0] * inner_count] > res[offset])
              res[offset] += V[count + dims[0] * inner_count];
              counts[offset] += 1;
              
              inner_count++;
              
            }
            
          }
          
        }
        
        count++;
        
      }
      
    }
    
  }
  
  for (int i = 0; i < res.size(); i++) {
    
    if (counts[i] > 0) {
      
      res[i] /= counts[i];
      
    }
    
  }
  
  res.attr("dim") = target_dims;
  
  return res;
  
}

// [[Rcpp::export]]
NumericVector windows_to_volume_label(NumericVector V, int stride, int width, IntegerVector target_dims,
                                      int displacement = 0) {
  
  if (width % 2 == 0) width++;
  int radius = (width + 1) / 2;
  
  IntegerVector dims = V.attr("dim");
  
  Rprintf("Vector dims = (%u,%u)\n", dims[0], dims[1]);
  
  NumericVector res(target_dims[0] * target_dims[1] * target_dims[2], 0.0);
  
  int count = 0;
  // For every seed voxel, compute the window
  for (int z = displacement + radius - 1; z < target_dims[2] - width + radius - 1; z += stride) {
    
    for (int y = displacement + radius - 1; y < target_dims[1] - width + radius - 1; y += stride) {
      
      for (int x = displacement + radius - 1; x < target_dims[0] - width + radius - 1; x += stride) {
        
        int inner_count = 0;
        
        for (int dz = -radius + 1; dz < radius; dz++) {
          
          for (int dy = -radius + 1; dy < radius; dy++) {
            
            for (int dx = -radius + 1; dx < radius; dx++) {
              
              int offset = (x + dx) + target_dims[0] * (y + dy) + target_dims[0] * target_dims[1] * (z + dz);
              
              if (offset >= target_dims[0] * target_dims[1] * target_dims[2] ) {
                
                Rprintf("Offset = %u\n", offset);
                Rprintf("Index = %u\n", count + dims[0] * inner_count);
                
              }
              
              if (V[count + dims[0] * inner_count] > res[offset])
                res[offset] = V[count + dims[0] * inner_count];
              
              inner_count++;
              
            }
            
          }
          
        }
        
        count++;
        
      }
      
    }
    
  }
  
  res.attr("dim") = target_dims;
  
  return res;
  
}


// [[Rcpp::export]]
void results_to_volume(NumericVector V, 
                       int width,
                       NumericVector res, 
                       NumericVector counts, 
                       IntegerVector x, IntegerVector y, IntegerVector z) {
  
  if (width % 2 == 0) width ++;
  int radius = (width + 1) / 2;
  
  IntegerVector dims = V.attr("dim");
  IntegerVector target_dims = res.attr("dim");
  
  // Rprintf("Vector dims = (%u,%u)\n", dims[0], dims[1]);
  
  for (int i = 0; i < x.size(); i++) {
    
    int inner_count = 0;
    
    for (int dz = -radius + 1; dz < radius; dz++) {
      
      for (int dy = -radius + 1; dy < radius; dy++) {
        
        for (int dx = -radius + 1; dx < radius; dx++) {
          
          if ((x[i] + dx >= 0) & (x[i] + dx < dims[0]) & (y[i] + dy >= 0) & (y[i] + dy < dims[1]) & (z[i] + dz >= 0) & (z[i] + dz < dims[2])) {
            
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
void results_to_fuzzy_volume(NumericVector V, 
                             int width,
                             int num_classes,
                             NumericVector res, 
                             NumericVector counts, 
                             IntegerVector x, IntegerVector y, IntegerVector z) {
  
  if (width % 2 == 0) width ++;
  int radius = (width + 1) / 2;
  
  IntegerVector dims = V.attr("dim");
  IntegerVector target_dims = res.attr("dim");
  
  // Rprintf("Vector dims = (%u,%u)\n", dims[0], dims[1]);
  
  for (int i = 0; i < x.size(); i++) {
    
    int inner_count = 0;
    
    for (int dz = -radius + 1; dz < radius; dz++) {
      
      for (int dy = -radius + 1; dy < radius; dy++) {
        
        for (int dx = -radius + 1; dx < radius; dx++) {
          
          int offset = (x[i] + dx) + target_dims[0] * (y[i] + dy) + target_dims[0] * target_dims[1] * (z[i] + dz);
          
          for (int k = 0; k < num_classes; k++) {
            
            int offset_4d = offset + k * target_dims[0] * target_dims[1] * target_dims[2];
            
            res[offset_4d] += V[i + dims[0] * inner_count];
            inner_count++;
            
            
          }
          
          counts[offset] += 1;
          
          
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
  
  if (width % 2 == 0) width ++;
  int radius = (width + 1) / 2;
  
  IntegerVector dims = V.attr("dim");
  IntegerVector target_dims = res.attr("dim");
  
  // Rprintf("Vector dims = (%u,%u)\n", dims[0], dims[1]);
  
  for (int i = 0; i < x.size(); i++) {
    
    int inner_count = 0;
    
    for (int dz = -radius + 1; dz < radius; dz++) {
      
      for (int dy = -radius + 1; dy < radius; dy++) {
        
        for (int dx = -radius + 1; dx < radius; dx++) {
          
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
  
  if (width % 2 == 0) width ++;
  int radius = (width + 1) / 2;
  
  IntegerVector dims = V.attr("dim");
  IntegerVector target_dims = res.attr("dim");
  
  // Rprintf("Vector dims = (%u,%u)\n", dims[0], dims[1]);
  
  for (int i = 0; i < x.size(); i++) {
    
    int inner_count = 0;
    
    for (int dz = -radius + 1; dz < radius; dz++) {
      
      for (int dy = -radius + 1; dy < radius; dy++) {
        
        for (int dx = -radius + 1; dx < radius; dx++) {
          
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

