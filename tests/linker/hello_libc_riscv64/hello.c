/* Real, non-trivial (i.e. actually calls into lib_core/libc, not a
 * hand-rolled per-test stub) C program used as a whole-program
 * integration test for o5a/o5l: scripts/diff-c-program.sh finds this
 * file's real, minimal lib_core/libc dependency closure (via
 * scripts/find-c-closure.py's BFS over a real goken 5c -S), assembles
 * and links the whole closure with xix's own o5a/o5l, and runs the
 * result under qemu-arm.
 *
 * Deliberately checked in here (a copy, not a reference into a
 * sibling goken checkout) so this fixture doesn't depend on goken's
 * own test suite layout staying stable -- lib_core/libc itself,
 * needed for the dependency closure, is still read live from
 * $GOKEN_ROOT, same as every other scripts/diff-*.sh fixture.
 * Content matches goken's own tests/c/hello_libc/hello.c exactly. See
 * docs/claude_notes/plan_hello_libc_linking.md for the full story of
 * what this uncovered.
 */
#include <u.h>
#include <libc.h>

void
main(void)
{
	print("hello from libc.a: %d + %d = %d\n", 2, 2, 2+2);
	exit(0);
}
