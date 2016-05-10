#include "print.h"
#include "reduce.h"
#include <assert.h>

void reduce_print_list(struct Cell* cell, char* output_type) {
	printf("["); fflush(stdout);
	
	while (_reduce(cell)) {}
	assert(cell->tag == DATA);

	if (get_data_tag(cell) != 0) {
		assert(cell->tag == DATA);
		struct Cell* value = select_data_field(cell, 0);
		struct Cell* tail = select_data_field(cell, 1);
		reduce(value);
		assert(value->tag == DATA);
		print_reduction_result(value, output_type, 0);
		cell = tail;
		while (_reduce(cell)) {}
	}

	while (get_data_tag(cell) != 0) {
		assert(cell->tag == DATA);
		struct Cell* value = select_data_field(cell, 0);
		struct Cell* tail = select_data_field(cell, 1);
		reduce(value);
		assert(value->tag == DATA);
		printf(", ");
		print_reduction_result(value, output_type, 0);
		cell = tail;
		while (_reduce(cell)) {}
	}
	printf("]");
}
void print_reduction_result(struct Cell* cell, char* output_type, int newline) {
	assert(output_type[0] != 0);

	switch (output_type[0]) {
	case 'l':
		reduce_print_list(cell, output_type + 1);
		break;
	case 'i':
		reduce(cell);
		assert(cell->tag == DATA);
		printf("%d", get_data_num(cell));
		break;
	}
	if (newline) printf("\n");
}
