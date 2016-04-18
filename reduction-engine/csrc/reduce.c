#include "reduce.h"
#include "cell.h"
#include <stdlib.h>
#include <assert.h>
#include <stdio.h>

////////////////////////////////////////////////////////////////////////////////
// SPINE STACK MANIPULATION
////////////////////////////////////////////////////////////////////////////////
struct SpineStack* push_spine_stack(struct SpineStack* stack, struct Cell* cell) {
	assert(cell);
	struct SpineStack* top = malloc(sizeof(struct SpineStack));
	top->cell = cell;
	top->base = stack;
	return top;
}
struct SpineStack* pop_spine_stack(struct SpineStack* stack) {
	assert(stack && stack->cell);
	struct SpineStack* base = stack->base;
	free(stack);
	return base;
}
struct SpineStack* spine_stack_nth_node(struct SpineStack* stack, int n) {
	while (n-- > 0) stack = stack->base;
	return stack;
}
void destroy_spine_stack(struct SpineStack* stack) {
	while (stack) stack = pop_spine_stack(stack);
	assert(stack == NULL);
}
int spine_stack_size(struct SpineStack* stack) {
	// TODO: inefficient linear time algorithm.
	int size = 0;
	while (stack) {
		stack = stack->base;
		size++;
	}
	return size;
}

////////////////////////////////////////////////////////////////////////////////
// SELECT REDEX
////////////////////////////////////////////////////////////////////////////////
struct SelectRedexResult select_redex(struct SpineStack* spine_stack, struct Cell* cell) {
	assert(NULL != cell);
	struct SelectRedexResult result;
	result.redex = NULL;
	result.spine_stack = NULL;
	printf("Cell: %p\n", cell);

	switch (cell->tag) {

	// If the cell under examination is an application, we must further
	// unwind the spine while building up the spine stack.
	case APP:
		printf("APP\n");
		spine_stack = push_spine_stack(spine_stack, cell);
		return select_redex(spine_stack, cell->f1.ptr);

	// If the tip is a lambda abstraction...
	case ABSTR:
		printf("ABSTR\n");
		// If there are no arguments, then there is also no stack,
		// so we return an empty result.
		if (spine_stack == NULL) return result;

		// If there is at least one argument, the previously pushed
		// vertebra is the redex and the function should be pushed
		// on the stack.
		assert(spine_stack->cell);
		result.redex = spine_stack->cell;
		result.spine_stack = push_spine_stack(spine_stack, cell);
		return result;

	// If the cell at the tip of the spine is a data object, no further
	// reduction is needed. The spine stack is destroyed.
	case DATA:
		printf("DATA\n");
		destroy_spine_stack(spine_stack);
		return result;

	// If the tip is a builtin operator, we go back up the spine 
	// to find the redex, depending on the number of arguments required
	// to reduce the operator.
	case BUILTIN:
		printf("BUILTIN\n");
		int nargs = BUILTIN_ARGUMENTS[cell->f1.op];

		// If there are not enough arguments to the builtin function,
		// no further reduction can be done. The spine stack is
		// destroyed.
		if (spine_stack_size(spine_stack) < nargs) {
			destroy_spine_stack(spine_stack);
			return result;
		}

		// Move back up the spine to get to the redex.
		// (-1) because we have not yet added the tip of the spine
		// to the stack.
		result.redex = spine_stack_nth_node(spine_stack, nargs-1)->cell;

		// Complete the spine stack.
		result.spine_stack = push_spine_stack(spine_stack, cell);

		return result;

	// Constructors are a special kind of builtin operator.
	// The logic is similar to the BUILTIN case, only the number of
	// arguments to the constructor is stored in the CONSTR cell itself.
	case CONSTR:
		printf("CONSTR\n");
		StructuredDataTag tag = cell->f1.data_tag;
		int nfields = cell->f2.num;
		if (spine_stack_size(spine_stack) < nfields) {
			destroy_spine_stack(spine_stack);
			return result;
		}
		result.redex = spine_stack_nth_node(spine_stack, nfields-1)->cell;
		result.spine_stack = push_spine_stack(spine_stack, cell);
		return result;

	default:
		printf("DID NOT RECOGNIZE\n");
		return result;
	}
}

struct Cell* reduce_arg(struct SpineStack* spine_stack, int n) {
	return reduce(spine_stack_nth_node(spine_stack, n)->cell->f2.ptr);
}

struct Cell* reduce_builtin(struct SpineStack* spine_stack) {
	struct Cell* result = NULL;
	struct Cell* operator = spine_stack->cell;

	switch (operator->f1.op) {

	// Add two numbers.
	case PLUS: ;
		int sum = reduce_arg(spine_stack, 1)->f1.num
			+ reduce_arg(spine_stack, 2)->f1.num;
		result = make_empty_cell();
		set_cell_number(result, sum);
		break;

	case MINUS:
		break;

	case YCOMB:
		break;

	case SELECT: ;
		int index = reduce_arg(spine_stack, 1)->f1.num;
		struct Cell* data_cell = reduce_arg(spine_stack, 2);
		assert(data_cell->tag == DATA);
		result = select_data_field(data_cell, index);
		break;
	}

	return result;
}

struct Cell* reduce_constructor(struct SpineStack* spine_stack) {
	struct Cell* constr = spine_stack->cell;
	StructuredDataTag tag = get_constr_data_tag(constr);
	int nfields = get_constr_nfields(constr);

	struct Cell* result = make_empty_cell();
	set_cell_empty_data(result, tag, nfields);

	for (int i = 0; i < nfields; i++) {
		struct Cell* value = reduce_arg(spine_stack, i+1);
		set_data_field(result, i, value);
	}

	return result;
}

struct Cell* reduce(struct Cell* cell) {
	struct SelectRedexResult srr = select_redex(NULL, cell);
	printf("Reducing %p ...\n", cell);
	printf("Redex = %p\n", srr.redex);

	// If there is no redex, the expression is already fully reduced.
	if (srr.redex == NULL)
		return cell;

	struct Cell* operator = srr.spine_stack->cell;
	struct Cell* result = NULL;

	switch (operator->tag) {
	case BUILTIN:
		result = reduce_builtin(srr.spine_stack);
		break;
	case DATA:
		result = srr.redex;
		break;
	}

	destroy_spine_stack(srr.spine_stack);
	printf("RESULT = \n\t"); print_cell(result);
	return result;
}
