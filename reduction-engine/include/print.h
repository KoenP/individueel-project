#ifndef PRINT_H
#define PRINT_H

#include <stdint.h>
#include <stddef.h>
#include <stdio.h>
#include "cell.h"

void print_reduction_result(struct Cell* cell, char* output_type, int newline);

#endif
