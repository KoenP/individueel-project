#include <stdlib.h>
#include <assert.h>

////////////////////////////////////////////////////////////////////////////////
// TYPES AND GLOBAL CONSTANTS.
////////////////////////////////////////////////////////////////////////////////
typedef enum {APP, ABSTR, NUMBER, BUILTIN} SystemTag;
typedef uint8_t Tag;
typedef enum {PLUS, MINUS, YCOMB, NUM_BUILTINS} Builtin;
typedef char* Symbol;

// Never < 1.
const short BUILTIN_ARGUMENTS = {
	2, //PLUS
	2, //MINUS
	1  //YCOMB
};

union Field {
	Cell* ptr;
	Symbol sym;
	int num;
	Builtin op;
};
struct Cell {
	Tag tag;
	Field f1;
	Field f2;
};
struct SpineStack {
	Cell* cell;
	SpineStack* base;
};
struct Dump {
	int depth;
	Dump* base;
};

////////////////////////////////////////////////////////////////////////////////
// GLOBAL VARIABLES
////////////////////////////////////////////////////////////////////////////////
SpineStack* spine_stack = NULL;
Dump* dump = NULL;
int spine_stack_size = 0;
Cell* graph = NULL;


////////////////////////////////////////////////////////////////////////////////
// CELL CONSTRUCTORS
////////////////////////////////////////////////////////////////////////////////
Cell* make_app(Cell* ptr1, Cell* ptr2) {
	Cell* c		= malloc(sizeof(Cell));
	c->tag		= APP;
	c->f1.ptr	= ptr1;
	c->f2.ptr	= ptr2;
	return c;
}
Cell* make_abstr(Symbol sym, Cell* body) {
	Cell* c		= malloc(sizeof(Cell));
	c->tag		= ABSTR;
	c->f1.sym	= sym;
	c->f2		= ptr2;
	return c;
}
Cell* make_number(int num) {
	Cell* c		= malloc(sizeof(Cell));
	c->tag		= NUMBER;
	c->f1.num	= num;
	return c;
}
Cell* make_builtin(Builtin op) {
	Cell* c		= malloc(sizeof(Cell));
	c->tag		= BUILTIN;
	c->f1.op	= op;
	return c;
}

////////////////////////////////////////////////////////////////////////////////
// CELL PREDICATES
////////////////////////////////////////////////////////////////////////////////
int is_application(Cell* c) {
	return c->tag == APP;
}
int is_abstraction(Cell* c) {
	return c->tag == ABSTR;
}
int is_data_object(Cell* c) {
	assert(0);
}
int is_builtin(Cell* c) {
	return c->tag == BUILTIN;
}

////////////////////////////////////////////////////////////////////////////////
// SPINE STACK MANIPULATION
////////////////////////////////////////////////////////////////////////////////
void push_spine_stack(Cell* c) {
	SpineStack* top = malloc(sizeof(SpineStack));
	top->cell = c;
	top->base = spine_stack;
	spine_stack = top;
	spine_stack_size++;
}
Cell* pop_spine_stack() {
	assert(spine_stack && spine_stack->cell);
	Cell* cell = spine_stack->cell;
	SpineStack* base = spine_stack->base;
	free(spine_stack);
	spine_stack = base;
	spine_stack_size--;
	return cell;
}
SpineStack* spine_stack_nth_node(int n) {
	SpineStack* substack = spine_stack;
	while (n-- > 0) substack = substack->base;
	return substack;
}
void destroy_spine_stack() {
	while (spine_stack ) pop_spine_stack();
	assert(spine_stack_size == 0 && spine_stack == NULL);
}

////////////////////////////////////////////////////////////////////////////////
// SELECT REDEX
////////////////////////////////////////////////////////////////////////////////

// Builds up the spine stack and returns a pointer to the redex.
Cell* select_redex(Cell* cell) {
	assert(NULL != cell);

	// If the cell under examination is an application, we must further
	// unwind the spine while building up the spine stack.
	if (is_application(cell)) {
		push_spine_stack(cell);
		return select_redex(c->f1);
	}

	// If the cell at the tip of the spine is a data object, no further
	// reduction is needed. The spine stack is destroyed.
	else if (is_data_object(cell)) {
		destroy_spine_stack();
		return NULL;
	}

	// If the tip is a builtin operator, we go back up the spine 
	// to find the redex, depending on the number of arguments required
	// to reduce the operator.
	else if (is_builtin(cell)) {
		int nargs = BUILTIN_ARGUMENTS[cell->f1.op];

		// If there are not enough arguments to the builtin function,
		// no further reduction can be done. The spine stack is
		// destroyed.
		if (spine_stack_size < nargs) {
			destroy_spine_stack();
			return NULL;
		}

		// The (-1) because the stack's top is currently the vertebra
		// right above the tip, and not the tip itself.
		SpineStack substack = spine_stack_nth_node(nargs - 1);
		assert(substack  && substack->cell );
		return substack->cell;
	}

	// If the tip is a lambda abstraction...
	else if (is_abstraction(cell)) {
		// If there is at least one argument, the previously pushed
		// vertebra is the redex and the function should be pushed
		// on the stack.
		if (spine_stack_size > 0) {
			assert(spine_stack && spine_stack->cell);
			Cell* redex = spine_stack->cell;
			push_spine_stack(cell);
		}
		
		// If there are no arguments, then there is also no stack,
		// so nothing needs to be done either way.
		return;
	}
}

Cell* reduce(Cell* redex) {
	assert(redex && spine_stack);
	
}
