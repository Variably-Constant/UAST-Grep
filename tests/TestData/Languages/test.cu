/**
 * CUDA Test File for UAST-Grep
 * Tests: kernels, device functions, memory, streams, templates
 */

#include <cuda_runtime.h>
#include <device_launch_parameters.h>
#include <stdio.h>
#include <stdlib.h>

// Constants
#define MAX_ITEMS 1024
#define BLOCK_SIZE 256
#define WARP_SIZE 32

// Error checking macro
#define CUDA_CHECK(call) \
    do { \
        cudaError_t err = call; \
        if (err != cudaSuccess) { \
            fprintf(stderr, "CUDA error at %s:%d: %s\n", \
                    __FILE__, __LINE__, cudaGetErrorString(err)); \
            exit(EXIT_FAILURE); \
        } \
    } while (0)

// Device constants
__constant__ float d_multiplier = 2.0f;
__constant__ int d_maxItems = MAX_ITEMS;

// Shared memory size
__shared__ float sharedData[BLOCK_SIZE];

// Enum for operations
enum Operation {
    OP_ADD,
    OP_MULTIPLY,
    OP_TRANSFORM
};

// Struct for data
struct DataItem {
    int id;
    float value;
    int processed;
};

// Device function - inline by default
__device__ float transform(float value) {
    if (value > 0.0f) {
        return value * 2.0f;
    } else if (value < 0.0f) {
        return value * -1.0f;
    } else {
        return 0.0f;
    }
}

// Device function with forced inline
__device__ __forceinline__ float add(float a, float b) {
    return a + b;
}

// Device function without inline
__device__ __noinline__ float complexOperation(float x, float y, float z) {
    float result = x * x + y * y + z * z;
    result = sqrtf(result);
    return result;
}

// Simple kernel
__global__ void vectorAdd(const float* a, const float* b, float* c, int n) {
    int idx = blockIdx.x * blockDim.x + threadIdx.x;

    if (idx < n) {
        c[idx] = a[idx] + b[idx];
    }
}

// Kernel with shared memory
__global__ void vectorAddShared(const float* a, const float* b, float* c, int n) {
    __shared__ float sharedA[BLOCK_SIZE];
    __shared__ float sharedB[BLOCK_SIZE];

    int idx = blockIdx.x * blockDim.x + threadIdx.x;
    int tid = threadIdx.x;

    // Load to shared memory
    if (idx < n) {
        sharedA[tid] = a[idx];
        sharedB[tid] = b[idx];
    }

    // Synchronize threads
    __syncthreads();

    // Compute
    if (idx < n) {
        c[idx] = sharedA[tid] + sharedB[tid];
    }
}

// Kernel with grid-stride loop
__global__ void vectorTransform(float* data, int n) {
    int idx = blockIdx.x * blockDim.x + threadIdx.x;
    int stride = blockDim.x * gridDim.x;

    // Grid-stride loop
    for (int i = idx; i < n; i += stride) {
        data[i] = transform(data[i]);
    }
}

// Reduction kernel
__global__ void reduce(float* input, float* output, int n) {
    extern __shared__ float sdata[];

    int tid = threadIdx.x;
    int idx = blockIdx.x * blockDim.x + threadIdx.x;

    // Load to shared memory
    sdata[tid] = (idx < n) ? input[idx] : 0.0f;
    __syncthreads();

    // Reduction in shared memory
    for (unsigned int s = blockDim.x / 2; s > 0; s >>= 1) {
        if (tid < s) {
            sdata[tid] += sdata[tid + s];
        }
        __syncthreads();
    }

    // Write result
    if (tid == 0) {
        output[blockIdx.x] = sdata[0];
    }
}

// Warp-level reduction using shuffle
__device__ float warpReduce(float val) {
    for (int offset = WARP_SIZE / 2; offset > 0; offset /= 2) {
        val += __shfl_down_sync(0xffffffff, val, offset);
    }
    return val;
}

// Block-level reduction using warp shuffle
__global__ void reduceWarp(float* input, float* output, int n) {
    int tid = threadIdx.x;
    int idx = blockIdx.x * blockDim.x + threadIdx.x;

    float val = (idx < n) ? input[idx] : 0.0f;

    // Warp-level reduction
    int lane = tid % WARP_SIZE;
    int wid = tid / WARP_SIZE;

    val = warpReduce(val);

    // Write warp results to shared memory
    __shared__ float warpSums[BLOCK_SIZE / WARP_SIZE];
    if (lane == 0) {
        warpSums[wid] = val;
    }
    __syncthreads();

    // Final reduction by first warp
    if (wid == 0) {
        val = (tid < blockDim.x / WARP_SIZE) ? warpSums[lane] : 0.0f;
        val = warpReduce(val);

        if (tid == 0) {
            output[blockIdx.x] = val;
        }
    }
}

// Template kernel
template <typename T>
__global__ void vectorScale(T* data, T scale, int n) {
    int idx = blockIdx.x * blockDim.x + threadIdx.x;

    if (idx < n) {
        data[idx] *= scale;
    }
}

// Template device function
template <typename T>
__device__ T clamp(T value, T minVal, T maxVal) {
    return max(minVal, min(value, maxVal));
}

// Kernel with dynamic parallelism (requires compute capability 3.5+)
#if __CUDA_ARCH__ >= 350
__global__ void parentKernel(float* data, int n) {
    int idx = blockIdx.x * blockDim.x + threadIdx.x;

    if (idx == 0 && n > BLOCK_SIZE) {
        // Launch child kernel
        int blocks = (n / 2 + BLOCK_SIZE - 1) / BLOCK_SIZE;
        vectorTransform<<<blocks, BLOCK_SIZE>>>(data + n/2, n - n/2);
        cudaDeviceSynchronize();
    }

    if (idx < n/2) {
        data[idx] = transform(data[idx]);
    }
}
#endif

// Atomic operations kernel
__global__ void histogram(const int* input, int* output, int n, int numBins) {
    int idx = blockIdx.x * blockDim.x + threadIdx.x;

    if (idx < n) {
        int bin = input[idx] % numBins;
        atomicAdd(&output[bin], 1);
    }
}

// Cooperative groups (CUDA 9+)
#include <cooperative_groups.h>
namespace cg = cooperative_groups;

__global__ void cooperativeKernel(float* data, int n) {
    cg::thread_block block = cg::this_thread_block();
    cg::grid_group grid = cg::this_grid();

    int idx = grid.thread_rank();

    if (idx < n) {
        data[idx] *= 2.0f;
    }

    // Grid-wide synchronization
    grid.sync();
}

// Host function to process data
void processData(float* h_data, int n) {
    float *d_data;
    size_t size = n * sizeof(float);

    // Allocate device memory
    CUDA_CHECK(cudaMalloc(&d_data, size));

    // Copy to device
    CUDA_CHECK(cudaMemcpy(d_data, h_data, size, cudaMemcpyHostToDevice));

    // Launch kernel
    int blocks = (n + BLOCK_SIZE - 1) / BLOCK_SIZE;
    vectorTransform<<<blocks, BLOCK_SIZE>>>(d_data, n);

    // Check for errors
    CUDA_CHECK(cudaGetLastError());
    CUDA_CHECK(cudaDeviceSynchronize());

    // Copy back to host
    CUDA_CHECK(cudaMemcpy(h_data, d_data, size, cudaMemcpyDeviceToHost));

    // Free device memory
    CUDA_CHECK(cudaFree(d_data));
}

// Host function with streams
void processWithStreams(float* h_data, int n, int numStreams) {
    cudaStream_t* streams = new cudaStream_t[numStreams];
    float** d_data = new float*[numStreams];

    int chunkSize = n / numStreams;
    size_t chunkBytes = chunkSize * sizeof(float);

    // Create streams and allocate memory
    for (int i = 0; i < numStreams; i++) {
        CUDA_CHECK(cudaStreamCreate(&streams[i]));
        CUDA_CHECK(cudaMalloc(&d_data[i], chunkBytes));
    }

    // Process chunks asynchronously
    for (int i = 0; i < numStreams; i++) {
        int offset = i * chunkSize;

        CUDA_CHECK(cudaMemcpyAsync(d_data[i], h_data + offset,
                                   chunkBytes, cudaMemcpyHostToDevice,
                                   streams[i]));

        int blocks = (chunkSize + BLOCK_SIZE - 1) / BLOCK_SIZE;
        vectorTransform<<<blocks, BLOCK_SIZE, 0, streams[i]>>>(d_data[i], chunkSize);

        CUDA_CHECK(cudaMemcpyAsync(h_data + offset, d_data[i],
                                   chunkBytes, cudaMemcpyDeviceToHost,
                                   streams[i]));
    }

    // Synchronize all streams
    CUDA_CHECK(cudaDeviceSynchronize());

    // Cleanup
    for (int i = 0; i < numStreams; i++) {
        CUDA_CHECK(cudaStreamDestroy(streams[i]));
        CUDA_CHECK(cudaFree(d_data[i]));
    }

    delete[] streams;
    delete[] d_data;
}

// Main function
int main(int argc, char* argv[]) {
    int n = MAX_ITEMS;

    // Allocate host memory
    float* h_data = (float*)malloc(n * sizeof(float));

    // Initialize data
    for (int i = 0; i < n; i++) {
        h_data[i] = (float)(i - n/2);
    }

    printf("UAST-Grep CUDA Test\n");
    printf("Processing %d items\n", n);

    // Process data
    processData(h_data, n);

    // Print results
    printf("First 10 results:\n");
    for (int i = 0; i < 10; i++) {
        printf("  [%d] = %.2f\n", i, h_data[i]);
    }

    // Cleanup
    free(h_data);

    printf("Processing complete\n");
    return 0;
}
