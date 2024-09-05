#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <CL/cl.h>

#include "helper.h"
#include "cbmp.h" // https://github.com/mattflow/cbmp/tree/master

#define MAX_SOURCE_SIZE (0x100000)

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


    // define opencl variables
    cl_platform_id platform;
    cl_device_id device;
    cl_context context;
    cl_command_queue queue;
    cl_program program;
    cl_kernel blur;
    cl_event event = NULL;
    cl_int err;


    // query opencl platform and device
    err = clGetPlatformIDs(1, &platform, NULL);
    err = clGetDeviceIDs(platform, CL_DEVICE_TYPE_GPU, 1, &device, NULL);

    print_device_info(device);


    // create opencl context
    context = clCreateContext(NULL, 1, &device, NULL, NULL, &err);


    // create command queue
    queue = clCreateCommandQueue(context, device, 0, &err);


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

    cl_mem imgPixels = clCreateBuffer(context, CL_MEM_READ_WRITE, sizeof(pixel) * img_size, NULL, &err);
    cl_mem clWeights = clCreateBuffer(context, CL_MEM_READ_ONLY, sizeof(float) * 9, NULL, &err);

    err  = clEnqueueWriteBuffer(queue, imgPixels, CL_TRUE, 0, sizeof(pixel) * img_size, img->pixels, 0, NULL, NULL);
    err |= clEnqueueWriteBuffer(queue, clWeights, CL_TRUE, 0, sizeof(float) * 9       , weights, 0, NULL, NULL);
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
        global_work_size, NULL, 0, NULL, NULL
    );
    clWaitForEvents(1, &event);

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
        NULL
    );

    cl_ulong time_start, time_end;
    cl_ulong total_time;
    clGetEventProfilingInfo(event, CL_PROFILING_COMMAND_START, sizeof(cl_ulong), &time_start, NULL);
    clGetEventProfilingInfo(event, CL_PROFILING_COMMAND_END, sizeof(cl_ulong), &time_end, NULL);
    total_time = (time_end - time_start) / 1000000.0f; // convert nanoseconds to miliseconds
    printf("Kernel Execution Time: %f ms\n", total_time);

    if (err != CL_SUCCESS) {
        printf("Error: clEnqueueReadBuffer failed with error code: %d\n", err);
        exit(EXIT_FAILURE);
    }

    free(img->pixels);
    img->pixels = blurred;
    bwrite(img, "blurred.bmp");
    bclose(img);

    // cleanup
    clReleaseKernel(blur);
    clReleaseProgram(program);
    clReleaseCommandQueue(queue);
    clReleaseContext(context);


    return EXIT_SUCCESS;
}
