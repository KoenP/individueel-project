#include "cell.h"
#include <stdlib.h>
#include <assert.h>
#include <stdio.h>

////////////////////////////////////////////////////////////////////////////////
// CELL INITIALIZATION
////////////////////////////////////////////////////////////////////////////////
struct Cell* make_empty_cell() {
	return malloc(sizeof(struct Cell));
}
void set_cell_var(struct Cell* c, Symbol sym) {
	c->tag = VAR;
	c->f1.sym = sym;
}
void set_cell_app(struct Cell* c, struct Cell* ptr1, struct Cell* ptr2) {
	c->tag		= APP;
	c->f1.ptr	= ptr1;
	c->f2.ptr	= ptr2;
}
void set_cell_abstr(struct Cell* c, Symbol sym, struct Cell* body) {
	c->tag		= ABSTR;
	c->f1.sym	= sym;
	c->f2.ptr	= body;
}
void set_cell_number(struct Cell* c, int num) {
	c->tag		= DATA;
	c->f1.num	= num;
}
void set_cell_builtin(struct Cell* c, Builtin op) {
	c->tag		= BUILTIN;
	c->f1.op	= op;
}
void set_cell_empty_data(struct Cell* c, StructuredDataTag tag, int size) {
	c->tag		= DATA;
	c->f1.data_tag	= tag;
	if (size > 0)
		c->f2.data_ptr = calloc(size, sizeof(struct Cell*));
	else
		c->f2.data_ptr = NULL;
}
void set_cell_constructor(struct Cell* c, StructuredDataTag data_tag, int nargs) {
	c->tag		= CONSTR;
	c->f1.data_tag	= data_tag;
	c->f2.num	= nargs;
}
void set_data_field(struct Cell* data_cell, int index, struct Cell* value) {
	data_cell->f2.data_ptr[index] = value;
}

////////////////////////////////////////////////////////////////////////////////
// CELL DATA QUERIES
////////////////////////////////////////////////////////////////////////////////
StructuredDataTag get_constr_data_tag(struct Cell* constr) {
	return constr->f1.data_tag;
}
int get_constr_nfields(struct Cell* constr) {
	return constr->f2.num;
}
struct Cell* select_data_field(struct Cell* data_cell, size_t index) {
	return data_cell->f2.data_ptr[index];
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

	// XXX: For now, we assume it's a number...
	case DATA:
		printf("%i", cell->f1.num);
		break;
	case BUILTIN:
		printf("+");
		break;
	default:
		printf("Unrecognized tag: %i\n", cell->tag);
	}
}
void print_cell(struct Cell* cell) {
	_print_cell(cell);
	printf("\n");
}
