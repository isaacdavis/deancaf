#ifndef __RUNTIME_H__
#define __RUNTIME_H__

struct __Object {
	void *v_table;
};
typedef struct __Object *Object;

struct __String {
	Object $s1;
	void *v_table;
	char contents[];
};
typedef struct __String *String;

struct __Io {
	Object $s1;
	void *v_table;
};
typedef struct __Io *IO;

struct __Array {
	int length;
	int data[];
};
typedef struct __Array *ARRAY;


#endif