#pragma src "/sys/src/libdraw"

typedef struct	Channel Channel;
typedef struct	Cursor Cursor;
typedef struct	Menu Menu;
typedef struct 	Mousectl Mousectl;

enum Click {
    CLICK_LEFT = 1,
    CLICK_MIDDLE = 2,
    CLICK_RIGHT = 4,
};

struct	Mouse
{
    Point	xy;
    // bitset<enum<Click>>
    int	buttons;	/* bit array: LMR=124 */

    ulong	msec;
};

struct Mousectl
{
    Mouse;

    // /dev/mouse
    fdt		mfd;		/* to mouse file */ 
    // ref_own<filename>
    char	*file; // "/dev/mouse" usually

    // ref<Image>
    Image*	image;	/* of associated window/display */

    // chan<Mouse> (listener = user program, sender = _ioproc(mouse.c))
    Channel	*c;			/* chan(Mouse) */
    Channel	*resizec;	/* chan(int)[2] */
    /* buffered in case client is waiting for a mouse action before handling resize */
    int		pid;	/* of slave proc */
    // /dev/cursor
    fdt		cfd;		/* to cursor file */
};

//TODO: mv in menu.h?
struct Menu
{
    char	**item;
    char	*(*gen)(int);
    int	lasthit;
};

/*
 * Mouse
 */
extern Mousectl*	initmouse(char*, Image*);
extern void			closemouse(Mousectl*);

extern int			readmouse(Mousectl*);
extern void			moveto(Mousectl*, Point);
extern void			setcursor(Mousectl*, Cursor*);

//TODO? mv in menu.h?
extern void			drawgetrect(Rectangle, int);
extern Rectangle	getrect(int, Mousectl*);

extern int	 		menuhit(int, Mousectl*, Menu*, Screen*);
