#pragma src "/sys/src/libdraw"

typedef struct	Cursor Cursor;

struct	Cursor
{
    Point	offset;

    byte	clr[2*16];
    byte	set[2*16];
};
