#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <CL/cl.h>

#include "helper.h"
#include "cbmp.h" // https://github.com/mattflow/cbmp/tree/master
#include "timing.h"

#define MAX_SOURCE_SIZE (0x100000)

struct timeval tv;
struct timeval tv_total_start, tv_total_end;
struct timeval tv_init_end;
struct timeval tv_h2d_start, tv_h2d_end;
struct timeval tv_d2h_start, tv_d2h_end;
struct timeval tv_kernel_start, tv_kernel_end;
struct timeval tv_mem_alloc_start, tv_mem_alloc_end;
struct timeval tv_close_start, tv_close_end;
float init_time = 0, mem_alloc_time = 0, h2d_time   = 0, kernel_time= 0,
      d2h_time  = 0, close_time     = 0, total_time = 0;


float eventTime(cl_event event, cl_command_queue command_queue) {
    cl_int error=0;
    cl_ulong eventStart,eventEnd;
    clFinish(command_queue);

    error = clGetEventProfilingInfo(event,CL_PROFILING_COMMAND_START,
                                    sizeof(cl_ulong),&eventStart,NULL);

    error = clGetEventProfilingInfo(event,CL_PROFILING_COMMAND_END,
                                    sizeof(cl_ulong),&eventEnd,NULL);

    return (float)((float)(eventEnd-eventStart)/1000000.0f);
}


int
main(int argc, char *argv[])
{
    // read image
    BMP* img;
    switch (argc) {
        case 1: {
            img = bopen("blocks.bmp");
        } break;

        case 2: {
            img = bopen(argv[1]);
        } break;

        default: {
            print_usage();
            exit(1);
        } break;
    }

    // time: total start
    gettimeofday(&tv_total_start, NULL);

    // define opencl variables
    cl_platform_id platform;
    cl_device_id device;
    cl_context context;
    cl_command_queue queue;
    cl_program program;
    cl_kernel blur;
    cl_event writeEvent, kernelEvent, readEvent;
    cl_int err;


    // query opencl platform and device
    err = clGetPlatformIDs(1, &platform, NULL);
    err = clGetDeviceIDs(platform, CL_DEVICE_TYPE_GPU, 1, &device, NULL);

    print_device_info(device);


    // create opencl context
    context = clCreateContext(NULL, 1, &device, NULL, NULL, &err);


    // create command queue
    queue = clCreateCommandQueue(context, device, 0, &err);

    // time: init end
    gettimeofday(&tv_init_end, NULL);
    tvsub(&tv_init_end, &tv_total_start, &tv);
    init_time = (float) tv.tv_sec * 1000.0f + (float) tv.tv_usec / 1000.0f;

    // load file and compile kernel code
    FILE *fp;
    char *source_str;
    size_t source_size;

    fp = fopen("blur.cl", "r");
    if (!fp) {
        printf("Failed to load kernel source.\n");
        exit(EXIT_FAILURE);
    }

    source_str = (char *)malloc(MAX_SOURCE_SIZE);
    source_size = fread(source_str, 1, MAX_SOURCE_SIZE, fp);
    fclose(fp);

    program = clCreateProgramWithSource(
        context, 1, (const char **)&source_str,
        &source_size, &err);

    free(source_str);

    err = clBuildProgram(program, 1, &device, NULL, NULL, NULL);
    if (err != CL_SUCCESS) {
        printf("Failed to build program.\n");
        printf("Error code: %d\n", err);
        exit(EXIT_FAILURE);
    }


    // create kernel
    blur = clCreateKernel(program, "blur", &err);

    if (err != CL_SUCCESS) {
        printf("Error: Creating kernel\n");
        exit(EXIT_FAILURE);
    }


    // kernel arguments
    float weights[9] = { 0.028532f, 0.067234f, 0.124009f, 0.179044f, 0.20236f, 0.179044f, 0.124009f, 0.067234f, 0.028532f };

    size_t img_size = (size_t) get_width(img) * (size_t) get_height(img);

    // time: mem alloc
    gettimeofday(&tv_mem_alloc_start, NULL);

    cl_mem imgPixels = clCreateBuffer(context, CL_MEM_READ_WRITE, sizeof(pixel) * img_size, NULL, &err);
    cl_mem clWeights = clCreateBuffer(context, CL_MEM_READ_ONLY, sizeof(float) * 9, NULL, &err);

    // time: mem alloc end
    gettimeofday(&tv_mem_alloc_end, NULL);
    tvsub(&tv_mem_alloc_end, &tv_mem_alloc_start, &tv);
    mem_alloc_time = (float) tv.tv_sec * 1000.0f + (float) tv.tv_usec / 1000.0f;

    err  = clEnqueueWriteBuffer(queue, imgPixels, CL_TRUE, 0, sizeof(pixel) * img_size, img->pixels, 0, NULL, &writeEvent);

    // time: write event 1
    h2d_time += eventTime(writeEvent, queue);
    clReleaseEvent(writeEvent);

    err |= clEnqueueWriteBuffer(queue, clWeights, CL_TRUE, 0, sizeof(float) * 9       , weights, 0, NULL, &writeEvent);

    // time: write event 1
    h2d_time += eventTime(writeEvent, queue);
    clReleaseEvent(writeEvent);

    if (err != CL_SUCCESS) {
        printf("Error: clEnqueueWriteBuffer for imgPixels and/or clWeights failed with error code %d\n", err);
        exit(EXIT_FAILURE);
    }

    err  = clSetKernelArg(blur, 0, sizeof(cl_mem), (void *)&imgPixels);
    err |= clSetKernelArg(blur, 1, sizeof(cl_mem), (void *)&clWeights);

    if (err != CL_SUCCESS) {
        printf("Error: Setting kernel arguments\n");
        exit(EXIT_FAILURE);
    }


    // Deploy
    size_t global_work_size[2] = { (size_t) get_width(img), (size_t) get_height(img) };
    err = clEnqueueNDRangeKernel(
        queue, blur, 2, NULL,
        global_work_size, NULL, 0, NULL, &kernelEvent
    );

    // time: kernel
    kernel_time += eventTime(kernelEvent, queue);
    clReleaseEvent(kernelEvent);

    if (err != CL_SUCCESS) {
        printf("Error: clEnqueueNDRangeKernel failed with error code %d\n", err);
        exit(EXIT_FAILURE);
    }


    // allocate memory and read from GPU to it
    pixel *blurred = (pixel *) malloc(sizeof(pixel) * img_size);

    err = clEnqueueReadBuffer(
        queue,
        imgPixels,
        CL_TRUE,
        0,
        sizeof(pixel) * img_size,
        blurred,
        0,
        NULL,
        &readEvent
    );

    // time: reading from gpu
    d2h_time +=eventTime(readEvent, queue);

    if (err != CL_SUCCESS) {
        printf("Error: clEnqueueReadBuffer failed with error code: %d\n", err);
        exit(EXIT_FAILURE);
    }

    free(img->pixels);
    img->pixels = blurred;
    bwrite(img, "blurred.bmp");
    bclose(img);

    // time: close start
    gettimeofday(&tv_close_start, NULL);

    // cleanup
    clReleaseKernel(blur);
    clReleaseProgram(program);
    clReleaseCommandQueue(queue);
    clReleaseContext(context);

    gettimeofday(&tv_close_end, NULL);
    tvsub(&tv_close_end, &tv_close_start, &tv);
    close_time += (float) tv.tv_sec * 1000.0f + (float) tv.tv_usec / 1000.0f;
    tvsub(&tv_close_end, &tv_total_start, &tv);
    total_time = (float) tv.tv_sec * 1000.0f + (float) tv.tv_usec / 1000.0f;

    printf("Init: %f\n", init_time);
    printf("MemAlloc: %f\n", mem_alloc_time);
    printf("HtoD: %f\n", h2d_time);
    printf("Exec: %f\n", kernel_time);
    printf("DtoH: %f\n", d2h_time);
    printf("Close: %f\n", close_time);
    printf("Total: %f\n", total_time);

    return EXIT_SUCCESS;
}
