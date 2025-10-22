typedef	struct	Ref	Ref;
typedef	struct	Gen2	Gen2;

struct	Sym
{
	Ref*	ref;
	vlong	value;
};

struct	Ref
{
	int	class;
};

struct	Gen
{
	vlong	offset;
	short	index;
	short	scale;
};
struct	Gen2
{
	Gen	from;
	Gen	to;
};
