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

	switch (cell->tag) {
	// If the tip of the spine is a variable, no further reduction is
	// possible.
	case VAR:
		return result;

	// If the cell under examination is an application, we must further
	// unwind the spine while building up the spine stack.
	case APP:
		spine_stack = push_spine_stack(spine_stack, cell);
		return select_redex(spine_stack, cell->f1.ptr);

	// If the tip is a lambda abstraction...
	case ABSTR:
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
		destroy_spine_stack(spine_stack);
		return result;

	// If the tip is a builtin operator, we go back up the spine 
	// to find the redex, depending on the number of arguments required
	// to reduce the operator.
	case BUILTIN: ;
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
		if (nargs > 0)
			result.redex = spine_stack_nth_node(spine_stack, nargs-1)->cell;
		else
			result.redex = cell;

		// Complete the spine stack.
		result.spine_stack = push_spine_stack(spine_stack, cell);

		return result;

	// Constructors are a special kind of builtin operator.
	// The logic is similar to the BUILTIN case, only the number of
	// arguments to the constructor is stored in the CONSTR cell itself.
	case CONSTR: ;
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
		return result;
	}
}

struct Cell* fully_reduce_arg(struct SpineStack* spine_stack, int n) {
	struct Cell* arg = get_nth_argument(spine_stack, n);
	assert(arg);
	reduce(arg);
	assert(arg);
	return arg;
}
struct Cell* reduce_arg(struct SpineStack* spine_stack, int n) {
	struct Cell* arg = get_nth_argument(spine_stack, n);
	assert(arg);
	_reduce(arg);
	assert(arg);
	return arg;
}

struct Cell* _replace(struct Cell* c, char* sym, struct Cell* arg) {
	switch(c->tag) {
	case VAR:
		// Variable is substituted for argument iff the symbols are
		// the same.
		if (strcmp(get_var_symbol(c), sym) == 0) {
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

struct Cell* instantiate(struct Cell* body, Symbol var, struct Cell* value) {
	struct Cell* result = NULL;
	switch (body->tag) {
	case VAR:
		if (strcmp(get_var_symbol(body), var) == 0) result = value;
		else result = body;
		break;
	case APP:
		result = make_empty_cell();
		set_cell_app(result,
			     instantiate(get_app_operator(body), var, value),
			     instantiate(get_app_operand(body), var, value));
		break;
	case ABSTR:
		if (strcmp(get_abstr_symbol(body), var) == 0) result = body;
		else {
			result = make_empty_cell();
			set_cell_abstr(result,
				       get_abstr_symbol(body),
				       instantiate(get_abstr_body(body), var, value));
		}
		break;
	default:
		result = body;
	}
	return result;
}

struct Cell* reduce_abstr(struct SpineStack* spine_stack) {
	struct Cell* abstr = spine_stack->cell;
	assert(abstr->tag == ABSTR);
	char* sym = get_abstr_symbol(abstr);
	struct Cell* body = get_abstr_body(abstr);
	struct Cell* arg = get_app_operand(spine_stack_nth_node(spine_stack, 1)->cell);
	return instantiate(body, sym, arg);
}

struct Cell* reduce_builtin(struct SpineStack* spine_stack) {
	struct Cell* result = NULL;
	struct Cell* operator = spine_stack->cell;

	switch (operator->f1.op) {

	// Add two numbers.
	case PLUS: ;
		struct Cell* term1 = fully_reduce_arg(spine_stack, 1);
		struct Cell* term2 = fully_reduce_arg(spine_stack, 2);
		assert(term1 && term2 && term1->tag == DATA && term2->tag == DATA);
		result = make_empty_cell();
		set_cell_number(result, get_data_num(term1) + get_data_num(term2));
		break;

	case MINUS: ;
		int diff = fully_reduce_arg(spine_stack, 1)->f1.num
			 - fully_reduce_arg(spine_stack, 2)->f1.num;
		result = make_empty_cell();
		set_cell_number(result, diff);
		break;

	case YCOMB:
		break;

	case SELECT: ;
		struct Cell* arg = fully_reduce_arg(spine_stack, 1);
		assert(arg && arg->tag == DATA);
		int index = arg->f1.num - 1;
		struct Cell* data_cell = fully_reduce_arg(spine_stack, 2);
		assert(data_cell->tag == DATA);
		result = select_data_field(data_cell, index);
		break;

	case EQ: ;
		int equality = fully_reduce_arg(spine_stack, 1)->f1.num
			== fully_reduce_arg(spine_stack, 2)->f1.num;
		result = make_empty_cell(result, equality);
		set_cell_number(result, equality);
		break;

	case IF: ;
		struct Cell* cond_result = fully_reduce_arg(spine_stack, 1);
		assert(cond_result->tag == DATA);
		if (get_data_num(cond_result) == 1)
			result = get_nth_argument(spine_stack, 2);
		else
			result = get_nth_argument(spine_stack, 3);
		break;

	case UNPACK_PRODUCT:
		{
			int arity = get_data_num(get_nth_argument(spine_stack, 1));
			struct Cell* f = fully_reduce_arg(spine_stack, 2);
			struct Cell* a = get_nth_argument(spine_stack, 3);
			
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
			break;
		}
	case UNPACK_SUM:
		{
			StructuredDataTag tag =
				get_data_num(get_nth_argument(spine_stack, 1));
			int arity = get_data_num(get_nth_argument(spine_stack, 2));
			struct Cell* f = fully_reduce_arg(spine_stack, 3);
			struct Cell* a = fully_reduce_arg(spine_stack, 4);
			if (get_data_tag(a) != tag) {
				result = make_empty_cell();
				set_cell_builtin(result, FAIL);
				break;
			}
			
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
			break;
		}

	case FATBAR:
		{
			// Keep reducing the first argument until it's either FAIL
			// or fully reduced.
			struct Cell* a = get_nth_argument(spine_stack, 1);
			int done = 0;
			while ((a->tag != BUILTIN || get_builtin_op(a) != FAIL)
			       && !done) {
				done = _reduce(a);
			}

			
			// If a did not fail
			if (a->tag != BUILTIN || get_builtin_op(a) != FAIL) {
				result = a;
				break;
			}

			// If a failed
			result = spine_stack_nth_node(spine_stack, 2)->cell->f2.ptr;
			break;
		}

	case ERROR:
		assert(0 && "ERROR");
		break;
		
	case FAIL:
		result = make_empty_cell();
		set_cell_builtin(result, ERROR);
		break;

	case MULT: ;
		struct Cell* factor1 = fully_reduce_arg(spine_stack, 1);
		struct Cell* factor2 = fully_reduce_arg(spine_stack, 2);
		assert(factor1 && factor2 && factor1->tag == DATA && factor2->tag == DATA);
		result = make_empty_cell();
		set_cell_number(result, get_data_num(factor1) * get_data_num(factor2));
		break;

	}

	return result;
}

struct Cell* reduce_constructor(struct SpineStack* spine_stack) {
	struct Cell* constr = spine_stack->cell;
	StructuredDataTag tag = get_constr_data_tag(constr);
	int nfields = get_constr_nfields(constr);

	// Create an empty data cell.
	struct Cell* result = make_empty_cell();
	set_cell_empty_data(result, tag, nfields);

	// Find arguments in the spine stack, add them to the data cell.
	for (int i = 0; i < nfields; i++) {
		//struct Cell* value = reduce_arg(spine_stack, i+1);
		struct Cell* value = get_nth_argument(spine_stack, i+1);
		set_data_field(result, i, value);
	}

	return result;
}
static int count = 0;
int _reduce(struct Cell* cell) {
	int calltag = count++;
	struct SelectRedexResult srr = select_redex(NULL, cell);

	// If there is no redex, the expression is already fully reduced.
	if (srr.redex == NULL) {
		return 0;
	}

	struct Cell* operator = srr.spine_stack->cell;
	struct Cell* result = NULL;


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
	}

	*srr.redex = *result;
	destroy_spine_stack(srr.spine_stack);

	return 1;
}

struct Cell* reduce(struct Cell* cell) {
	while (_reduce(cell) == 1) {}
	printf("RESULT %p = \n\t", cell); print_cell(cell);
	return cell;
}

void _reduce_print_list(struct Cell* cell) {
	while (_reduce(cell)) {}
	assert(cell->tag == DATA);

	while (get_data_tag(cell) != 0) {
		assert(cell->tag == DATA);
		printf("cell tag = %d\n", cell->tag);
		struct Cell* value = select_data_field(cell, 0);
		printf("cell tag = %d\n", cell->tag);
		struct Cell* cell = select_data_field(cell, 1);
		printf("cell tag = %d\n", cell->tag);
		reduce(value);
		printf("cell tag = %d\n", cell->tag);
		assert(value->tag == DATA);
		printf("################################################################################");
		printf("%d, ", get_data_num(value)); fflush(stdout);
	}
}
void reduce_print_list(struct Cell* cell) {
	printf("["); fflush(stdout);
	_reduce_print_list(cell);
	printf("]\n");
}
