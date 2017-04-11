
extern int		drawlsetrefresh(ulong, int, void*, void*);

// dead?
extern	int		_cursorfd;

extern	bool	_drawdebug;	/* set to true to see errors from flushimage */

// used also by window.c
extern Image*	_allocimage(Image*, Display*, Rectangle, ulong, int, ulong, int, int);
extern int	    _freeimage1(Image*);

extern	void	_setdrawop(Display*, Drawop);

// used also by libmemdraw/
void _twiddlecompressed(uchar *buf, int n);
int _compblocksize(Rectangle r, int depth);

/*
 * Compressed image file parameters and helper routines
 */
#define	NMATCH	3		/* shortest match possible */
#define	NRUN	(NMATCH+31)	/* longest match possible */
#define	NMEM	1024		/* window size */
#define	NDUMP	128		/* maximum length of dump */
#define	NCBLOCK	6000		/* size of compressed blocks */

/* XXX backwards helps; should go */
extern	ulong	drawld2chan[];
extern	void	drawsetdebug(bool);

#include <marshal.h>

