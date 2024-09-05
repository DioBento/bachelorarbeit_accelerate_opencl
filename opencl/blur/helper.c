#include "helper.h"


void
print_usage(void)
{
    printf("Usage: blur <file.bmp>\n");
    printf("\n");
    printf("If no input image is specified, the default 'blocks.bmp'\n");
    printf("in the current directory will be used (if available).\n");
}


void
print_device_info(cl_device_id device)
{
    // Implementation of print_device_info
    char buffer[1024];

    printf("########################################\n");
    clGetDeviceInfo(device, CL_DEVICE_VENDOR, sizeof(buffer), buffer, NULL);
    printf("Device Vendor  : %s\n", buffer);

    clGetDeviceInfo(device, CL_DEVICE_NAME, sizeof(buffer), buffer, NULL);
    printf("Device Name    : %s\n", buffer);

    clGetDeviceInfo(device, CL_DEVICE_VERSION, sizeof(buffer), buffer, NULL);
    printf("Device Version : %s\n", buffer);

    printf("########################################\n\n");
}
