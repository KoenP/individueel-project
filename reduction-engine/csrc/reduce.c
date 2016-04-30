#include "reduce.h"
#include "cell.h"
#include <stdlib.h>
#include <assert.h>
#include <stdio.h>
#include <string.h>

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
struct Cell* get_nth_argument(struct SpineStack* stack, int n) {
	return get_app_operand(spine_stack_nth_node(stack, n)->cell);
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
	printf("select_redex: Cell = %p, ", cell);

	switch (cell->tag) {
	// If the tip of the spine is a variable, no further reduction is
	// possible.
	case VAR:
		printf("VAR\n");
		return result;

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
		result.spine_stack = push_spine_stack(spine_stack, cell);
		result.redex = spine_stack_nth_node(result.spine_stack, nfields)->cell;
		return result;

	default:
		printf("DID NOT RECOGNIZE\n");
		return result;
	}
}

struct Cell* reduce_arg(struct SpineStack* spine_stack, int n) {
	return reduce(spine_stack_nth_node(spine_stack, n)->cell->f2.ptr);
}

struct Cell* _replace(struct Cell* c, char* sym, struct Cell* arg) {
	switch(c->tag) {
	case VAR:
		// Variable is substituted for argument iff the symbols are
		// the same.
		printf("testje, VAR = %s\n", get_var_symbol(c));
		if (strcmp(get_var_symbol(c), sym) == 0) {
			printf("testje2\n");
			return arg;
		}
		else return c;
	case APP:
		// Recursively apply replace algorithm to children.
		c->f1.ptr = _replace(c->f1.ptr, sym, arg);
		c->f2.ptr = _replace(c->f2.ptr, sym, arg);
		return c;
	case ABSTR:
		// Apply replacement to body iff parameter symbol is not
		// the symbol being replaced. The occurrences of sym in the body
		// of an abstraction with sym as parameter are bound to the parameter
		// of the function.
		if (strcmp(get_abstr_symbol(c), sym) != 0)
			c->f2.ptr = _replace(c->f2.ptr, sym, arg);
		return c;
	default:
		return c;
	}
}
struct Cell* reduce_abstr(struct SpineStack* spine_stack) {
	printf("Reduce abstr with param %s\n", get_abstr_symbol(spine_stack->cell));
	struct Cell* abstr = spine_stack->cell;
	char* sym = get_abstr_symbol(abstr);
	struct Cell* body = get_abstr_body(abstr);
	struct Cell* arg = get_app_operand(spine_stack_nth_node(spine_stack, 1)->cell);
	return _replace(body, sym, arg);
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

	case MINUS: ;
		int diff = reduce_arg(spine_stack, 1)->f1.num
			 - reduce_arg(spine_stack, 2)->f1.num;
		result = make_empty_cell();
		set_cell_number(result, diff);
		break;

	case YCOMB:
		break;

	case SELECT: ;
		printf("SELECT REDUCTION 1\n");
		struct Cell* arg = reduce_arg(spine_stack, 1);
		assert(arg && arg->tag == DATA);
		int index = arg->f1.num - 1;
		printf("SELECT REDUCTION 2\n");
		struct Cell* data_cell = reduce_arg(spine_stack, 2);
		assert(data_cell->tag == DATA);
		result = select_data_field(data_cell, index);
		printf("SELECT RESULT %p\n", result);
		printf("\t= %d\n", get_data_num(result));
		break;

	case EQ: ;
		int equality = reduce_arg(spine_stack, 1)->f1.num
			== reduce_arg(spine_stack, 2)->f1.num;
		result = make_empty_cell(result, equality);
		set_cell_number(result, equality);
		break;

	case IF: ;
		struct Cell* cond_result = reduce_arg(spine_stack, 1);
		assert(cond_result->tag == DATA);
		if (get_data_num(cond_result) == 1)
			result = get_nth_argument(spine_stack, 2);
		else
			result = get_nth_argument(spine_stack, 3);
		break;

	case UNPACK_PRODUCT: ;
		int arity = get_data_num(get_nth_argument(spine_stack, 1));
		struct Cell* f = reduce_arg(spine_stack, 2);
		struct Cell* a = reduce_arg(spine_stack, 3);
		assert(a->tag == DATA);

		struct Cell* lhs = f;
		for (int i = 1; i <= arity; i++) {
			struct Cell* select_op = make_empty_cell();
			struct Cell* n = make_empty_cell();
			struct Cell* inner_app = make_empty_cell();
			struct Cell* middle_app = make_empty_cell();
			struct Cell* outer_app = make_empty_cell();
			set_cell_builtin(select_op, SELECT);
			set_cell_number(n, i);
			set_cell_app(inner_app, select_op, n);
			set_cell_app(middle_app, inner_app, a);
			set_cell_app(outer_app, lhs, middle_app);
			lhs = outer_app;
		}
		result = lhs;
	}

	return result;
}

struct Cell* reduce_constructor(struct SpineStack* spine_stack) {
	printf("reduce_constructor...\n");
	struct Cell* constr = spine_stack->cell;
	StructuredDataTag tag = get_constr_data_tag(constr);
	int nfields = get_constr_nfields(constr);

	// Create an empty data cell.
	struct Cell* result = make_empty_cell();
	set_cell_empty_data(result, tag, nfields);

	// Find arguments in the spine stack, add them to the data cell.
	for (int i = 0; i < nfields; i++) {
		struct Cell* value = reduce_arg(spine_stack, i+1);
		set_data_field(result, i, value);
	}

	return result;
}
static int count = 0;
struct Cell* reduce(struct Cell* cell) {
	int calltag = count++;
	printf("#### BEGIN %d ############\n", calltag);
	printf("CALL TO REDUCE\n");
	struct SelectRedexResult srr = select_redex(NULL, cell);

	// If there is no redex, the expression is already fully reduced.
	if (srr.redex == NULL) {
		printf("RESULT %p = \n\t", cell); print_cell(cell);
		printf("#### END %d ##############\n", calltag);
		return cell;
	}

	struct Cell* operator = srr.spine_stack->cell;
	struct Cell* result = NULL;

	printf("REDUCING\tCELL %p\n\t\tREDEX %p with tag = %i\n\t\tOPERATOR %p with tag = %i\n", cell, srr.redex, srr.redex->tag, operator, operator->tag);

	switch (operator->tag) {
	case ABSTR:
		result = reduce_abstr(srr.spine_stack);
		break;
	case BUILTIN:
		result = reduce_builtin(srr.spine_stack);
		break;
	case DATA:
		result = srr.redex;
		break;
	case CONSTR:
		result = reduce_constructor(srr.spine_stack);
		break;
	default:
		printf("REDUCE: tag not recognized\n");
	}

	printf("RESULT %p = \n\t", result); print_cell(result);
	*srr.redex = *result;
	destroy_spine_stack(srr.spine_stack);

	printf("#### END %d ##############\n", calltag);
	return reduce(cell);
}

// DOES NOT WORK
// let p = PAIR 1 2 in let (PAIR x y) = p in y
// (\p.( \ (PAIR x y).y) p) (PAIR 1 2)
