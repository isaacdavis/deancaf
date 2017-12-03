
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "runtime.h"

/******************************************************************************/
/* Object																	  */
/******************************************************************************/

Object
Object$$init$(Object xthis)
{
	return xthis;
}

/******************************************************************************/
/* String																	  */
/******************************************************************************/

String
String$$init$(String xthis)
{
	Object$$init$((Object) xthis);

	return xthis;
}


String
_$CreateString(char *t)
{
	extern void *_V$String;
	String s = (String) malloc(sizeof(struct __String) + strlen(t));
	s->v_table = &_V$String;
	strcpy(s->contents, t);
	return s;
}

/******************************************************************************/
/* IO																		  */
/******************************************************************************/

void
IO$putChar (char c)
{
	putchar(c);
}

void
IO$putInt (int v)
{
	printf("%x", v);
}

void
IO$putString (String s)
{
	printf("%s", s->contents);
}

int
IO$peek(void)
{
	int ch = getc(stdin);
	ungetc(ch, stdin);
	return ch;
}

int
IO$getChar()
{
	int ch = fgetc(stdin);
	return ch;
}

int
IO$getInt()
{
	int v;
	scanf("%d",&v);
	return v;
}

String
IO$getLine()
{
	char buf[10240];
	fgets(buf,10240,stdin);

	return _$CreateString(buf);
}

/******************************************************************************/
/* ARRAY																	  */
/******************************************************************************/

static ARRAY doAlloc(int,int,int,va_list);

ARRAY
_$ArrayAllocate(int typ, int dims, ...)
{
	va_list ap;
	va_start(ap,dims);

	return doAlloc(typ,dims,0,ap);
}

static ARRAY
doAlloc(int typ, int ndim, int dim, va_list ap)
{
	int n = va_arg(ap,int);
	int ntyp = typ;
	int sz,dsz,i;
	ARRAY a;

	if (dim+1 != ndim) ntyp = 3;

	switch (ntyp) {
	case 0:						/* BYTE 	*/
		dsz = sizeof(char);
		break;
	case 1:						/* CHAR 	*/
		dsz = sizeof(short);
		break;
	case 2:						/* INT		*/
		dsz = sizeof(int);
		break;
	case 3:						/* REF		*/
		dsz = sizeof(void *);
		break;
	}

	sz = sizeof(int) + dsz * n;
	a = (ARRAY) malloc(sz);
	memset(a, 0, sz);
	a->length = n;

	if (dim + 1 != ndim) {
		ARRAY *ptr = (ARRAY *) (&a->data);
		for (i = 0; i < n; ++i) {
			*ptr++ = doAlloc(typ,ndim,dim+1,ap);
		}
	}

	return a;
}

/******************************************************************************/
/* main																		  */
/******************************************************************************/

extern void _$DecafMain(ARRAY);

int
main(int argc,char ** argv)
{
	int i;
	ARRAY a = _$ArrayAllocate(3,1,argc-1);
	String *sp = (String *) (&a->data);

	for (i = 1; i < argc; ++i) {
		sp[i-1] = _$CreateString(argv[i]);
	}
	_$DecafMain(a);
	return 0;
}