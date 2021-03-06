#ifndef CELL_H
#define CELL_H
#include <stdint.h>
#include <stddef.h>

// Tag identifying the meaning of a cell's data.
typedef enum {VAR, APP, ABSTR, DATA, BUILTIN, CONSTR} Tag;
/* Cell layouts
 *  General:
 *    Tag | F1 | F2
 *  Application:
 *    APP | ptr | ptr
 *    Holds two pointers, each pointing to another cell.
 *  Abstraction:
 *    ABSTR | sym | ptr
 *    Holds a pointer to a symbol (param name) and a pointer to
 *    another cell, the function body.
 *  Structured data:
 *    DATA | data_tag | data_ptr
 *    Holds a tag identifying the constructor in a sum type, and
 *    a pointer to an array of cells, the fields of the data type.
 *  Builtin operator:
 *    BUILTIN | op | _
 *    Holds a symbolic value associated with the operation.
 *  Constructor:
 *    CONSTR | data_tag | num
 *    Holds a data tag to be inserted in the resulting DATA object,
 *    and an integer number indicating the number of fields in the
 *    resulting DATA object.
 */

// Constant symbols representing buitlin functions.
typedef enum {PLUS		= 0,
	      MINUS		= 1,
	      YCOMB		= 2,
	      SELECT		= 3,
	      EQ		= 4,
	      IF		= 5,
	      UNPACK_PRODUCT	= 6,
	      UNPACK_SUM	= 7,
	      FATBAR		= 8,
	      ERROR		= 9,
	      FAIL  		= 10,
	      MULT  		= 11,
	      NUM_BUILTINS
} Builtin;
const static short BUILTIN_ARGUMENTS[NUM_BUILTINS] = {
	2, //PLUS
	2, //MINUS
	1, //YCOMB
	2, //SELECT
	2, //EQ
	3, //IF
	3, //UNPACK-PRODUCT arity f a
	4, //UNPACK-SUM tag arity f a
	2, //FATBAR e f
	0, //ERROR
	0, //FAIL
	2  //MULT x y
};

// Symbols for variable names.
typedef char* Symbol;

// Tag for identifying the different constructors in a sum type.
typedef int StructuredDataTag;

// Pointer to an array of cell pointers (occurs in a structured data type).
typedef struct Cell** StructuredDataPtr;	

struct Cell;

// Different kinds of data that can be carried by a cell.
union Field {
	struct Cell* ptr;
	Symbol sym;
	int num;
	Builtin op;
	StructuredDataTag data_tag;
	StructuredDataPtr data_ptr;
};

// Nodes in the graph that represents the program.
struct Cell {
	Tag tag;
	union Field f1;
	union Field f2;
};

// Cell initialization.
struct Cell* make_empty_cell();
void set_cell_var(struct Cell* c, Symbol sym);
void set_cell_app(struct Cell* c, struct Cell* ptr1, struct Cell* ptr2);
void set_cell_abstr(struct Cell* c, Symbol sym, struct Cell* body);
void set_cell_number(struct Cell* c, int num);
void set_cell_builtin(struct Cell* c, Builtin op);
void set_cell_empty_data(struct Cell* c, StructuredDataTag tag, int size);
void set_cell_constructor(struct Cell* c, StructuredDataTag data_tag, int nargs);
void set_data_field(struct Cell* data_cell, int index, struct Cell* value);

// Cell data queries.
char* get_var_symbol(struct Cell* var);
struct Cell* get_app_operator(struct Cell* app);
struct Cell* get_app_operand(struct Cell* app);
char* get_abstr_symbol(struct Cell* abstr);
struct Cell* get_abstr_body(struct Cell* abstr);
StructuredDataTag get_data_tag(struct Cell* data_cell);
int get_data_num(struct Cell* data_cell);
Builtin get_builtin_op(struct Cell* builtin_cell);
StructuredDataTag get_constr_data_tag(struct Cell* constr);
int get_constr_nfields(struct Cell* constr);
struct Cell* select_data_field(struct Cell* data_cell, size_t index);

void print_cell(struct Cell* cell);

#endif
