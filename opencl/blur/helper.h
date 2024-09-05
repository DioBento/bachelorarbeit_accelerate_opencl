#ifndef HELPER_H
#define HELPER_H

#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <CL/cl.h>

#include "cbmp.h"

void print_usage(void);
void print_device_info(cl_device_id device);

#endif // HELPER_H
