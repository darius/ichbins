#include <assert.h>
#include <errno.h>
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static char input[64*1024];
static char *snarfing = input;
static const char *in = input;

static char output[1024*1024];
static char *out = output;

typedef struct {
  unsigned row;
  unsigned column;
} Position;

static Position
make_position (unsigned row, unsigned column)
{
  Position p = { row, column };
  return p;
}

static Position
where (const char *at)
{
  unsigned r = 1;
  unsigned c = 1;
  const char *s = input;
  for (; s < at; ++s)
    if ('\n' == *s)
      ++r, c = 1;
    else
      ++c;
  return make_position (r, c);
}

static void
stdlib_error (const char *plaint)
{
  fprintf (stderr, "%s: %s\n", plaint, strerror (errno));
  exit (1);
}

static void
panic (const char *complaint)
{
  Position p = where (in);
  printf ("! %u:%u: %s\n", p.row, p.column, complaint);
  exit (1);
}

static void traceback (void);

static void
traceback_error (const char *plaint)
{
  strcpy (output, plaint);
  out = output + strlen (plaint);
  *out++ = '\n';
  traceback ();
  *out++ = '\0';
  panic (output);
}

static void
error (const char *plaint)
{
  traceback_error (plaint);
}

static void
insist (int ok, const char *violation)
{
  if (!ok)
    traceback_error (violation);
}

static int
get_input (FILE *infile)
{
  if (NULL == fgets (snarfing, input + sizeof input - snarfing, infile))
    return 0;
  snarfing += strlen (snarfing);
  return 1;
}

static void
snarf (FILE *infile)
{
  while (get_input (infile))
    ;
}

typedef unsigned obj;

enum { a_pair = 0, nil, a_char };

static unsigned
get_tag (obj x)
{
  return 3 & x;
}

static unsigned
untag (unsigned tag, obj x)
{
#if 1
  /* assert (tag == get_tag (x)); */
  insist (tag == get_tag (x), "Bad type");
#else
  if (tag != get_tag (x))
    {
      *out = '\0';
      fprintf (stderr, "output: %s\n", output);
      fprintf (stderr, "tag %d; expected %d\n", get_tag (x), tag);
      error ("Bad tag");
    }
#endif
  return x >> 2;
}

static obj
entag (unsigned tag, unsigned value)
{
  return tag | (value << 2);
}

static obj
make_char (char c)
{
  return entag (a_char, c);
}

static char
char_value (obj x)
{
  return untag (a_char, x);
}

enum { heap_size = 50*1024*1024 };
static obj heap[heap_size][2];
static unsigned heap_ptr = 0;

static obj
cons (obj car, obj cdr)
{
  assert (heap_ptr < heap_size);
  heap[heap_ptr][0] = car;
  heap[heap_ptr][1] = cdr;
  return entag (a_pair, heap_ptr++);
}

static obj
car (obj x)
{
  unsigned p = untag (a_pair, x);
  assert (p < heap_ptr);  /* XXX heap_size, once we have gc */
  return heap[p][0];
}

static obj
cdr (obj x)
{
  unsigned p = untag (a_pair, x);
  assert (p < heap_ptr);  /* XXX heap_size, once we have gc */
  return heap[p][1];
}

static void
set_car (obj x, obj y)
{
  unsigned p = untag (a_pair, x);
  assert (p < heap_ptr);  /* XXX heap_size, once we have gc */
  heap[p][0] = y;
}

static int
is_string (obj x)
{
  /* XXX should we consider nil a string? */
  for (; a_pair == get_tag (x); x = cdr (x))
    if (a_char != get_tag (car (x)))
      return 0;
  return nil == x;
}

/* Pre: is_string (x) && is_string (y) */
static int
string_equal (obj x, obj y)
{
  for (; a_pair == get_tag (x) && a_pair == get_tag (y); x = cdr (x), y = cdr (y))
    if (car (x) != car (y))
      return 0;
  return x == y;
}

static obj symbols = nil;	/* a list of nonempty strings */

static obj
symbol_lookup (obj x)
{
  if (nil != x && is_string (x))
    {
      obj s = symbols;
      for (; a_pair == get_tag (s); s = cdr (s))
	if (string_equal (car (s), x))
	  return car (s);
    }
  return nil;
}

static int
is_symbol (obj x)
{
  if (a_pair == get_tag (x))
    {
      obj s = symbols;
      for (; a_pair == get_tag (s); s = cdr (s))
	if (car (s) == x)
	  return 1;
    }
  return 0;
}

static obj
intern (obj x)
{
  obj y = symbol_lookup (x);
  if (nil != y)
    return y;
  assert (nil != x && is_string (x));
  symbols = cons (x, symbols);
  return x;
}

static void
write_symbol (obj x)
{
  for (; nil != get_tag (x); x = cdr (x))
    *out++ = char_value (car (x));
}

static void
write_string (obj x)
{
  *out++ = '"';
  for (; nil != get_tag (x); x = cdr (x))
    *out++ = char_value (car (x));
  *out++ = '"';
}

static void
write (obj x)
{
  switch (get_tag (x))
    {
    case nil:
      *out++ = '(';
      *out++ = ')';
      return;
    case a_char:
      *out++ = '\\';
      *out++ = char_value (x);
      return;
    case a_pair:
      if (is_string (x))
	{
	  if (is_symbol (x))
	    write_symbol (x);
	  else
	    write_string (x);
	}
      else
	{
	  *out++ = '(';
	  write (car (x));
	  for (x = cdr (x); a_pair == get_tag (x); x = cdr (x))
	    {
	      *out++ = ' ';
	      write (car (x));
	    }
	  if (nil == x)
	    *out++ = ')';
	  else
	    error ("Fucked-up list in write");
	}
      return;
    default:
      error ("Unknown tag");
    }
}

static void
print (obj x)
{
  write (x);
  *out++ = '\n';
}

static void
error_with_sexpr (obj x)
{
  out = output;
  write (x);
  *out++ = '\0';
  error (output);
}

static obj
c_string (const char *s)
{
  return '\0' == *s ? nil : cons (make_char (*s), c_string (s + 1));
}

static obj eof;
static obj sym_car;
static obj sym_cdr;
static obj sym_charP;
static obj sym_cond;
static obj sym_cons;
static obj sym_define;
static obj sym_eof_object;
static obj sym_eqP;
static obj sym_error;
static obj sym_f;
static obj sym_nullP;
static obj sym_pairP;
static obj sym_quote;
static obj sym_peek_char;
static obj sym_read_char;
static obj sym_set_carB;
static obj sym_t;
static obj sym_write_char;

static obj definitions = nil;
static obj global_vars = nil;
static obj global_vals = nil;

static void
set_up (void)
{
  sym_car        = intern (c_string ("car"));
  sym_cdr        = intern (c_string ("cdr"));
  sym_charP      = intern (c_string ("char?"));
  sym_cond       = intern (c_string ("cond"));
  sym_cons       = intern (c_string ("cons"));
  sym_define     = intern (c_string ("define"));
  sym_eof_object = intern (c_string ("eof-object"));
  sym_eqP        = intern (c_string ("eq?"));
  sym_error      = intern (c_string ("error"));
  sym_f          = intern (c_string ("f"));
  sym_nullP      = intern (c_string ("null?"));
  sym_pairP      = intern (c_string ("pair?"));
  sym_quote      = intern (c_string ("quote"));
  sym_peek_char  = intern (c_string ("peek-char"));
  sym_read_char  = intern (c_string ("read-char"));
  sym_set_carB   = intern (c_string ("set-car!"));
  sym_t          = intern (c_string ("t"));
  sym_write_char = intern (c_string ("write-char"));

  eof = cons (c_string ("*eof*"), nil);
  global_vars = cons (sym_eof_object, global_vars);
  global_vals = cons (eof, global_vals);
}

static obj
read_list (void);

static obj
read_string (void)
{
  switch (*in)
    {
    case '"':
      ++in;
      return nil;
    case '\0':
      error ("Unfinished string");
      return nil;
    default:
      {
	char c = *in++;
	return cons (make_char (c), read_string ());
      }
    }
}

static int
is_symbol_char (char c)
{
  return isprint (c) && !isspace (c) && !strchr ("\"\\()';", c);
}

static obj
read_symbol (void)
{
  if (!is_symbol_char (*in))
    return nil;
  {
    char c = *in++;
    return cons (make_char (c), read_symbol ());
  }
}

static obj
read (void)
{
  while (isspace (*in))
    ++in;
  switch (*in)
    {
    case '\\':
      ++in;
      return make_char (*in++);
    case '(':
      ++in;
      return read_list ();
    case '"':
      ++in;
      return read_string ();
    case '\'':
      ++in;
      return cons (sym_quote, cons (read (), nil));
    default:
      if (isprint (*in))
	return intern (read_symbol ());
      /* fall through */
    case ')':
      error ("rotten lexical syntax");
      return nil;
    }
}

static obj
read_top_level (void)
{
  while (isspace (*in))		/* XXX skip comments too */
    ++in;
  switch (*in)
    {
    case '\0':
      return eof;
    default:
      return read ();
    }
}

static obj
read_list (void)
{
  while (isspace (*in))
    ++in;
  if (')' == *in)
    {
      ++in;
      return nil;
    }
  if (strchr ("(\\\"'", *in) || is_symbol_char (*in))
    return cons (read (), read_list ());
  error ("lexical error in list");
  return nil;
}

static int
is_true (obj x)
{
  return x != sym_f;
}

static obj
make_flag (int flag)
{
  return flag ? sym_t : sym_f;
}

static obj
eval (obj x, obj vars, obj vals);

static obj
evseq (obj exprs, obj vars, obj vals)
{
  if (nil == exprs)
    return sym_f;
  if (nil == cdr (exprs))
    return eval (car (exprs), vars, vals);
  eval (car (exprs), vars, vals);
  return evseq (cdr (exprs), vars, vals);
}

static obj
evcond (obj clauses, obj vars, obj vals)
{
  if (a_pair != get_tag (clauses))
    {
      error ("No matching cond clause");
      return sym_f;
    }
  {
    obj c = car (clauses);
    if (a_pair != get_tag (c))
      {
	error ("Bad cond syntax");
	return sym_f;
      }
    if (is_true (eval (car (c), vars, vals)))
      return evseq (cdr (c), vars, vals);
    return evcond (cdr (clauses), vars, vals);
  }
}

enum { stack_size = 1000 };

static int sp = -1;
static obj stack_rator[stack_size];
static obj stack_args[stack_size];

static void
traceback (void)
{
  int i;
  for (i = sp; 0 <= i; --i)
    {
      write (stack_rator[i]);
      *out++ = ' ';
      print (stack_args[i]);
    }
}

static void
entrace (obj rator, obj args)
{
  if (stack_size <= sp + 1)
    traceback_error ("Stack overflow");
  ++sp;
  stack_rator[sp] = rator;
  stack_args[sp] = args;
}

static void
untrace (void)
{
  assert (0 <= sp);
  --sp;
}

static obj
apply (obj rator, obj args)
{
  if (sym_car == rator)
    return car (car (args));
  if (sym_cdr == rator)
    return cdr (car (args));
  if (sym_charP == rator)
    return make_flag (a_char == get_tag (car (args)));
  if (sym_cons == rator)
    return cons (car (args), car (cdr (args)));
  if (sym_eqP == rator)
    return make_flag (car (args) == car (cdr (args)));
  if (sym_error == rator)
    {
      error_with_sexpr (car (args));
      return sym_f;
    }
  if (sym_nullP == rator)
    return make_flag (nil == car (args));
  if (sym_pairP == rator)
    return make_flag (a_pair == get_tag (car (args)));
  if (sym_peek_char == rator)
    return '\0' == *in ? sym_f : make_char (*in);
  if (sym_read_char == rator)
    return '\0' == *in ? sym_f : make_char (*in++);
  if (sym_set_carB == rator)
    {
      set_car (car (args), car (cdr (args)));
      return sym_f;
    }
  if (sym_write_char == rator)
    {
      *out++ = untag (a_char, car (args));
      return sym_f;
    }
  {
    obj defs = definitions;
    for (; nil != defs; defs = cdr (defs))
      {
	obj def = car (defs);
	if (car (car (def)) == rator)
          {
	    obj result;
            entrace (rator, args);
	    result = evseq (cdr (def), cdr (car (def)), args);
            untrace ();
            return result;
          }
      }
  }
  error ("Unknown procedure");
  return nil;
}

static obj
evlis (obj rands, obj vars, obj vals)
{
  if (nil == rands)
    return nil;
  if (a_pair == get_tag (rands))
    return cons (eval (car (rands), vars, vals), 
		 evlis (cdr (rands), vars, vals));
  error ("Non-list operand list");
  return nil;
}

static obj
lookup (obj var, obj vars, obj vals)
{
  for (; a_pair == get_tag (vars); vars = cdr (vars), vals = cdr (vals))
    if (var == car (vars))
      return car (vals);
  vars = global_vars, vals = global_vals;
  for (; a_pair == get_tag (vars); vars = cdr (vars), vals = cdr (vals))
    if (var == car (vars))
      return car (vals);
  error ("Unbound variable");
  return nil;
}

static obj
eval (obj x, obj vars, obj vals)
{
  switch (get_tag (x))
    {
    case nil:
    case a_char:
      return x;
    case a_pair:
      if (is_symbol (x))
	return lookup (x, vars, vals);
      else
	{
	  obj rator = car (x);
	  obj rands = cdr (x);
	  if (sym_quote == rator)
	    return car (rands);
	  if (sym_cond == rator)
	    return evcond (cdr (x), vars, vals);
	  return apply (rator, evlis (rands, vars, vals));
	}
    default:
      error ("Bad tag");
      return nil;
    }
}

static void
define (obj definition)
{
  /* TODO: more syntax checking */
  if (!is_symbol (car (definition)))
    definitions = cons (definition, definitions);
  else
    {
      obj var  = car (definition);
      obj expr = car (cdr (definition));
      obj val  = eval (expr, nil, nil);
      global_vars = cons (var, global_vars);
      global_vals = cons (val, global_vals);
    }
}

static void
eval_form (obj x)
{
  if (a_pair == get_tag (x) && sym_define == car (x))
    define (cdr (x));
  else
    print (eval (x, nil, nil));
}

static void
run (void)
{
  in = input;
  for (;;) 
    {
      obj x = read_top_level ();
      if (eof == x)
	break;
      eval_form (x);
    }
}

int
main (int argc, char **argv)
{
  out = output;
  set_up ();
  if (argc < 2)
    snarf (stdin);
  else
    {
      int i;
      for (i = 1; i < argc; ++i)
        {
          FILE *f = fopen (argv[i], "r");
          if (!f)
            stdlib_error (argv[i]);
          snarf (f);
          fclose (f);
        }
    }
  run ();
  *out++ = '\0';
  printf ("= %s", output);
  return 0;
}
