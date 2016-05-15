#ifndef REDUCE_H
#define REDUCE_H
#include <stdint.h>
#include <stddef.h>
#include "cell.h"

// TODO: wrap in container that also stores stack size.
struct SpineStack {
	struct Cell* cell;
	struct SpineStack* base;
};
struct SelectRedexResult {
	struct Cell* redex;
	struct SpineStack* spine_stack;
};

struct SpineStack* push_spine_stack(struct SpineStack* stack, struct Cell* cell);
struct SpineStack* pop_spine_stack(struct SpineStack* stack);
struct SpineStack* spine_stack_nth_node(struct SpineStack* stack, int n);
struct Cell* get_nth_argument(struct SpineStack* stack, int n);
void destroy_spine_stack(struct SpineStack* stack);
int spine_stack_size(struct SpineStack* stack);

// Returns the redex with the full spine stack, or (NULL,NULL) if there are no
// reductions to be done.
struct SelectRedexResult select_redex(struct SpineStack* spine_stack, struct Cell* cell);
int _reduce(struct Cell* redex);
struct Cell* reduce(struct Cell* redex);

#endif
