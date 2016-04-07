#include "reduce.h"
#include <stdlib.h>
#include <assert.h>
#include <stdio.h>

int* test(int* ptr) {
	int* result = malloc(sizeof(int));
	if (ptr == NULL) printf("ptr is null\n");
	*result = 5;
	return result;
}

////////////////////////////////////////////////////////////////////////////////
// CELL CONSTRUCTORS
////////////////////////////////////////////////////////////////////////////////
struct Cell* make_app(struct Cell* ptr1, struct Cell* ptr2) {
	struct Cell* c	= malloc(sizeof(struct Cell));
	c->tag		= APP;
	c->f1.ptr	= ptr1;
	c->f2.ptr	= ptr2;
	return c;
}
struct Cell* make_abstr(Symbol sym, struct Cell* body) {
	struct Cell* c	= malloc(sizeof(struct Cell));
	c->tag		= ABSTR;
	c->f1.sym	= sym;
	c->f2.ptr	= body;
	return c;
}
struct Cell* make_number(int num) {
	struct Cell* c	= malloc(sizeof(struct Cell));
	c->tag		= NUMBER;
	c->f1.num	= num;
	return c;
}
struct Cell* make_builtin(Builtin op) {
	struct Cell* c	= malloc(sizeof(struct Cell));
	c->tag		= BUILTIN;
	c->f1.op	= op;
	return c;
}

void _print_cell(struct Cell* cell) {
	switch (cell->tag) {
	case APP:
		printf("(");
		_print_cell(cell->f1.ptr);
		printf(" ");
		_print_cell(cell->f2.ptr);
		printf(")");
		break;
	case NUMBER:
		printf("%i", cell->f1.num);
		break;
	default:
		printf("Unrecognized tag: %i\n", cell->tag);
	}
}
void print_cell(struct Cell* cell) {
	_print_cell(cell);
	printf("\n");
}

////////////////////////////////////////////////////////////////////////////////
// CELL PREDICATES
////////////////////////////////////////////////////////////////////////////////
int is_application(struct Cell* c) {
	return c->tag == APP;
}
int is_abstraction(struct Cell* c) {
	return c->tag == ABSTR;
}
int is_data_object(struct Cell* c) {
	// TODO
	return c->tag == NUMBER;
}
int is_builtin(struct Cell* c) {
	return c->tag == BUILTIN;
}

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
// Builds up the spine stack and returns a pointer to the redex.
struct SelectRedexResult select_redex(struct SpineStack* spine_stack, struct Cell* cell) {
	assert(NULL != cell);
	struct SelectRedexResult result;
	result.redex = NULL;
	result.spine_stack = NULL;

	// If the cell under examination is an application, we must further
	// unwind the spine while building up the spine stack.
	if (is_application(cell)) {
		spine_stack = push_spine_stack(spine_stack, cell);
		return select_redex(spine_stack, cell->f1.ptr);
	}

	// If the cell at the tip of the spine is a data object, no further
	// reduction is needed. The spine stack is destroyed.
	else if (is_data_object(cell)) {
		destroy_spine_stack(spine_stack);
		return result;
	}

	// If the tip is a builtin operator, we go back up the spine 
	// to find the redex, depending on the number of arguments required
	// to reduce the operator.
	else if (is_builtin(cell)) {
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
		result.redex = spine_stack_nth_node(spine_stack, nargs - 1)->cell;

		// Complete the spine stack.
		result.spine_stack = push_spine_stack(spine_stack, cell);

		return result;
	}

	// If the tip is a lambda abstraction...
	else if (is_abstraction(cell)) {
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
	}
	return result;
}

struct Cell* reduce(struct Cell* redex) {
	return NULL;
}
