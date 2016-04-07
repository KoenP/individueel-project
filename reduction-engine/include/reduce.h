#ifndef MAIN_H
#define MAIN_H
#include <stdint.h>

int* test(int* ptr);

////////////////////////////////////////////////////////////////////////////////
// TYPES AND GLOBAL CONSTANTS.
////////////////////////////////////////////////////////////////////////////////
typedef enum {APP, ABSTR, NUMBER, BUILTIN} SystemTag;
typedef uint8_t Tag;
typedef enum {PLUS, MINUS, YCOMB, NUM_BUILTINS} Builtin;
typedef char* Symbol;
struct Cell;
union Field {
	struct Cell* ptr;
	Symbol sym;
	int num;
	Builtin op;
};
struct Cell {
	Tag tag;
	union Field f1;
	union Field f2;
};
// TODO: wrap in container that also stores stack size.
struct SpineStack {
	struct Cell* cell;
	struct SpineStack* base;
};
struct Dump {
	int depth;
	struct Dump* base;
};
const short BUILTIN_ARGUMENTS[NUM_BUILTINS] = {
	2, //PLUS
	2, //MINUS
	1  //YCOMB
};
struct SelectRedexResult {
	struct Cell* redex;
	struct SpineStack* spine_stack;
};

struct Cell* make_app(struct Cell* ptr1, struct Cell* ptr2);
struct Cell* make_abstr(Symbol sym, struct Cell* body);
struct Cell* make_number(int num);
struct Cell* make_builtin(Builtin op);
void print_cell(struct Cell* cell);
int is_application(struct Cell* c);
int is_abstraction(struct Cell* c);
int is_data_object(struct Cell* c);
int is_builtin(struct Cell* c);
struct SpineStack* push_spine_stack(struct SpineStack* stack, struct Cell* cell);
struct SpineStack* pop_spine_stack(struct SpineStack* stack);
struct SpineStack* spine_stack_nth_node(struct SpineStack* stack, int n);
void destroy_spine_stack(struct SpineStack* stack);
int spine_stack_size(struct SpineStack* stack);
struct SelectRedexResult select_redex(struct SpineStack* spine_stack, struct Cell* cell);
struct Cell* reduce(struct Cell* redex);


#endif
