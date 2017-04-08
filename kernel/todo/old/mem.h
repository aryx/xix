#define KiB     1024u           /* Kibi 0x0000000000000400 */
#define MiB     1048576u        /* Mebi 0x0000000000100000 */
#define GiB     1073741824u     /* Gibi 000000000040000000 */

#define KZERO 0x80000000

#define BY2PG       (4*KiB)

#define CPUADDR     (KZERO+0x2000)
#define CPUSIZE BY2PG
