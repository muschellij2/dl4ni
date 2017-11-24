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


int getIndex(int* size, int x, int y, int z) {
  
  int size_x = size[0], size_y = size[1], size_z = size[2];
  
  if (x >= size_x) x = size_x - 1;
  if (y >= size_y) y = size_y - 1;
  if (z >= size_z) z = size_z - 1;
  
  if (x < 0) x = 0;
  if (y < 0) y = 0;
  if (z < 0) z = 0;
  
  return z * size_x * size_y + y * size_x + x;
  
}

int get_index(int* size, int x, int y, int z) {
  
  int size_x = size[0], size_y = size[1], size_z = size[2];
  
  if (x >= size_x) return -1;
  if (y >= size_y) return -1;
  if (z >= size_z) return -1;
  
  if (x < 0) return -1;
  if (y < 0) return -1;
  if (z < 0) return -1;
  
  return z * size_x * size_y + y * size_x + x;
  
}

float evaluate_volume(double* vol, int* size, int x, int y, int z) {
  
  int index = get_index(size, x, y, z);
  float value = 0;
  
  if (index >= 0) value = vol[index];
  
  return value;
  
}

//Trilinear interpolation kernel,wraped from FLIRT
float trilinear_interpolation(float v000, float v001, float v010, 
                             float v011, float v100, float v101, 
                             float v110, float v111, 
                             float dx, float dy, float dz) {
  
  float temp1, temp2, temp3, temp4, temp5, temp6;
  
  temp1 = (v100 - v000) * dx + v000;
  temp2 = (v101 - v001) * dx + v001;
  temp3 = (v110 - v010) * dx + v010;
  temp4 = (v111 - v011) * dx + v011;
  
  // second order terms
  temp5 = (temp3 - temp1) * dy + temp1;
  temp6 = (temp4 - temp2) * dy + temp2;
  
  // final third order term
  float result = (temp6 - temp5) * dz + temp5;
  
  return result;
  
}

float nearest_interpolation(float v000, float v001, float v010, 
                              float v011, float v100, float v101, 
                              float v110, float v111, 
                              float dx, float dy, float dz) {
  
  if ((dx <= 0.5) & (dy <= 0.5) & (dz <= 0.5)) return v000;
  if ((dx <= 0.5) & (dy <= 0.5) & (dz >= 0.5)) return v001;
  if ((dx <= 0.5) & (dy >= 0.5) & (dz <= 0.5)) return v010;
  if ((dx <= 0.5) & (dy >= 0.5) & (dz >= 0.5)) return v011;
  
  if ((dx >= 0.5) & (dy <= 0.5) & (dz <= 0.5)) return v100;
  if ((dx >= 0.5) & (dy <= 0.5) & (dz >= 0.5)) return v101;
  if ((dx >= 0.5) & (dy >= 0.5) & (dz <= 0.5)) return v110;
  if ((dx >= 0.5) & (dy >= 0.5) & (dz >= 0.5)) return v111;

  return 0.0;
  
}


//Get the interoplated value,wraped from FLIRT
float interpolate(double* vol, int* vol_size, 
                  float point_x, float point_y, float point_z,
                  int method) {
  
  int ix, iy, iz;  
  ix = (int) floor(point_x);
  iy = (int) floor(point_y);
  iz = (int) floor(point_z);
  
  //left-top-front
  float dx = point_x - ix, dy = point_y - iy, dz = point_z - iz;
  
  float v000, v001, v010, v011, v100, v101, v110, v111;
  
  v000 = evaluate_volume(vol, vol_size, ix, iy, iz);
  v001 = evaluate_volume(vol, vol_size, ix, iy, iz + 1);
  v010 = evaluate_volume(vol, vol_size, ix, iy + 1, iz);
  v011 = evaluate_volume(vol, vol_size, ix, iy + 1, iz + 1);
  v100 = evaluate_volume(vol, vol_size, ix + 1, iy, iz);
  v101 = evaluate_volume(vol, vol_size, ix + 1, iy, iz + 1);
  v110 = evaluate_volume(vol, vol_size, ix + 1, iy + 1, iz);
  v111 = evaluate_volume(vol, vol_size, ix + 1, iy + 1, iz + 1);

  float result;  
  if (method == 3) {
    
    //Use trilinear interpolation
    result = trilinear_interpolation(v000, v001, v010, v011, v100, v101, v110, v111, dx, dy, dz);
    
  } else {
    
    result = nearest_interpolation(v000, v001, v010, v011, v100, v101, v110, v111, dx, dy, dz);
    
  }
  
  return result;
  
}

void transform_volume(double* source_vol, double* target_vol, int* source_dims, int* target_dims, double* matrix, int method) {
  
  for (int x = 0; x < target_dims[0]; x++) {
    
    for (int y = 0; y < target_dims[1]; y++) {
      
      for (int z = 0; z < target_dims[2]; z++) {
        
        float value;
        
        int index = getIndex(target_dims, x, y, z);
        
        float px = x - target_dims[0] / 2;
        float py = y - target_dims[1] / 2;
        float pz = z - target_dims[2] / 2;
        
        float source_x = matrix[0] * px + matrix[4] * py + matrix[8] * pz + matrix[12];
        float source_y = matrix[1] * px + matrix[5] * py + matrix[9] * pz + matrix[13];
        float source_z = matrix[2] * px + matrix[6] * py + matrix[10] * pz + matrix[14];

        source_x += source_dims[0] / 2;
        source_y += source_dims[1] / 2;
        source_z += source_dims[2] / 2;
        
        value = interpolate(source_vol, source_dims, source_x, source_y, source_z, method);
        
        target_vol[index] = value;
        
      }
      
    }
    
  }
  
}

// [[Rcpp::export]]
NumericVector transform_volume(NumericVector V, NumericVector M, IntegerVector target_dims, int method) {
  
  IntegerVector source_dims = V.attr("dim");
  
  NumericVector transformed_volume(target_dims[0] * target_dims[1] * target_dims[2]);
  
  transform_volume(V.begin(), 
                   transformed_volume.begin(), 
                   source_dims.begin(), 
                   target_dims.begin(), 
                   M.begin(),
                   method);

  transformed_volume.attr("dim") = target_dims;
  
  return(transformed_volume);
  
}
