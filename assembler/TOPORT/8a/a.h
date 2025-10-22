typedef	struct	Ref	Ref;
typedef	struct	Gen2	Gen2;

struct	Sym
{
	Ref*	ref;
};

struct	Ref
{
	int	class;
};

struct	Gen
{
	int32	offset;
	int32	offset2;
	short	index;
	short	scale;
};
struct	Gen2
{
	Gen	from;
	Gen	to;
};
