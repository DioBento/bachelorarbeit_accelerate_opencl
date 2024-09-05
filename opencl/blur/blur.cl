struct pixel
{
    unsigned char red;
    unsigned char green;
    unsigned char blue;
    unsigned char alpha;
};


__kernel void blur( __global struct pixel *img
                  , __global float *weights )
{
    int x = get_global_id(0); // W
    int y = get_global_id(1); // H

    int width = get_global_size(0);
    int height = get_global_size(1);

    int index = y * width + x;

    float red_sum   = 0.0f;
    float green_sum = 0.0f;
    float blue_sum  = 0.0f;
    float alpha_sum = 0.0f;

    const int stencil_size = 4;

    // gaussianX
    for (int i = -stencil_size; i <= stencil_size; ++i) {
        int _x = x + i;

        // Clamp pixel to image boundaries
        _x = max(0, min(_x, width - 1));

        // calculate_pixel
        int id = y * width + _x;
        float weight = weights[i+stencil_size];

        red_sum   += (float) img[id].red   * weight;
        green_sum += (float) img[id].green * weight;
        blue_sum  += (float) img[id].blue  * weight;
        alpha_sum += (float) img[id].alpha * weight;
    }

    img[index].red   = (unsigned char) red_sum;
    img[index].green = (unsigned char) green_sum;
    img[index].blue  = (unsigned char) blue_sum;
    img[index].alpha = (unsigned char) alpha_sum;

    // reset sums for blurring in Y dim
    red_sum   = 0.0f;
    green_sum = 0.0f;
    blue_sum  = 0.0f;
    alpha_sum = 0.0f;

    // gaussianY
    for (int i = -stencil_size; i <= stencil_size; ++i) {
        int _y = y + i;

        // Clamp pixel to image boundaries
        _y = max(0, min(_y, height - 1));

        // calculate_pixel
        int id = _y * width + x;
        float weight = weights[i+stencil_size];

        red_sum   += (float) img[id].red   * weight;
        green_sum += (float) img[id].green * weight;
        blue_sum  += (float) img[id].blue  * weight;
        alpha_sum += (float) img[id].alpha * weight;
    }

    img[index].red   = (unsigned char) red_sum;
    img[index].green = (unsigned char) green_sum;
    img[index].blue  = (unsigned char) blue_sum;
    img[index].alpha = (unsigned char) alpha_sum;
}
