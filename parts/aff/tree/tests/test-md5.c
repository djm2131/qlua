/* The canonical test from RFC 1321 */

/* MDDRIVER.C - test driver for MD2, MD4 and MD5
 */

/* Copyright (C) 1990-2, RSA Data Security, Inc. Created 1990. All
   rights reserved.

   RSA Data Security, Inc. makes no representations concerning either
   the merchantability of this software or the suitability of this
   software for any particular purpose. It is provided "as is"
   without express or implied warranty of any kind.
   
   These notices must be retained in any copies of any part of this
   documentation and/or software.
 */

/* The following makes MD default to MD5 if it has not already been
   defined with C compiler flags.
 */

#include <stdio.h>
#include <sys/time.h>
#include <string.h>
#include <stdint.h>
#include "md5.h"

/* Length of test block, number of test blocks.
 */
#define TEST_BLOCK_LEN 100000
#define TEST_BLOCK_COUNT 1000

static void MDString(uint8_t *);
static void MDTimeTrial(void);
static void MDTestSuite(void);
static void MDFile(char *);
static void MDFilter(void);
static void MDPrint(uint8_t [16]);

/* Main driver.

   Arguments (may be any combination):
   -sstring - digests string
   -t       - runs time trial
   -x       - runs test script
   filename - digests file
   (none)   - digests standard input
*/
int
main(int argc, char *argv[])
{
    int i;

    if (argc > 1)
	for (i = 1; i < argc; i++)
	    if (argv[i][0] == '-' && argv[i][1] == 's')
		MDString((uint8_t *)(argv[i] + 2));
	    else if (strcmp(argv[i], "-t") == 0)
		MDTimeTrial();
	    else if (strcmp(argv[i], "-x") == 0)
		MDTestSuite();
	    else
		MDFile(argv[i]);
    else
	MDFilter();
    return 0;
}

/* Digests a string and prints the result.
 */
static void
MDString(uint8_t *string)
{
    struct AffMD5_s context;
    uint8_t digest[16];
    uint32_t len = strlen((char *)string);

    aff_md5_init(&context);
    aff_md5_update(&context, string, len);
    aff_md5_final(digest, &context);

    printf("MD5 (\"%s\") = ", string);
    MDPrint(digest);
    printf ("\n");
}

/* Measures the time to digest TEST_BLOCK_COUNT TEST_BLOCK_LEN-byte
  blocks.
 */
static void
MDTimeTrial(void)
{
    struct AffMD5_s context;
    double elapsed_time;
    struct timeval endTime, startTime;
    uint8_t block[TEST_BLOCK_LEN], digest[16];
    uint32_t i;

    printf("MD5 time trial. Digesting %d %d-byte blocks ...",
	   TEST_BLOCK_LEN, TEST_BLOCK_COUNT);

    /* Initialize block */
    for (i = 0; i < TEST_BLOCK_LEN; i++)
	block[i] = (uint8_t)(i & 0xff);

    /* Start timer */
    gettimeofday(&startTime, NULL);

    /* Digest blocks */
    aff_md5_init(&context);
    for (i = 0; i < TEST_BLOCK_COUNT; i++)
	aff_md5_update(&context, block, TEST_BLOCK_LEN);
    aff_md5_final(digest, &context);

    /* Stop timer */
    gettimeofday(&endTime, NULL);
    elapsed_time = endTime.tv_sec - startTime.tv_sec
	+ 1e-6  * (endTime.tv_usec - startTime.tv_usec);

    printf(" done\n");
    printf("Digest = ");
    MDPrint(digest);
    printf("\nTime = %.2f seconds\n", elapsed_time);
    printf("Speed = %.0f bytes/second\n",
	   (double)TEST_BLOCK_LEN * (double)TEST_BLOCK_COUNT/elapsed_time);
}

/* Digests a reference suite of strings and prints the results.
 */
static void
MDTestSuite(void)
{
    printf("MD5 test suite:\n");

    MDString((uint8_t *)"");
    MDString((uint8_t *)"a");
    MDString((uint8_t *)"abc");
    MDString((uint8_t *)"message digest");
    MDString((uint8_t *)"abcdefghijklmnopqrstuvwxyz");
    MDString((uint8_t *)
	     ("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefgh\
ijklmnopqrstuvwxyz0123456789"));
    MDString((uint8_t *)
	("1234567890123456789012345678901234567890\
1234567890123456789012345678901234567890"));
}

/* Digests a file and prints the result.
 */
static void
MDFile (char *filename)
{
    FILE *file;
    struct AffMD5_s context;
    int len;
    uint8_t buffer[1024], digest[16];

    if ((file = fopen(filename, "rb")) == NULL) {
	printf ("%s can't be opened\n", filename);
    } else {
	aff_md5_init(&context);
	while ((len = fread(buffer, 1, 1024, file)))
	    aff_md5_update(&context, buffer, len);
	aff_md5_final(digest, &context);

	fclose(file);

	printf("MD5 (%s) = ", filename);
	MDPrint(digest);
	printf ("\n");
    }
}

/* Digests the standard input and prints the result.
 */
static void
MDFilter(void)
{
    struct AffMD5_s context;
    int len;
    uint8_t buffer[16], digest[16];

    aff_md5_init (&context);
    while ((len = fread(buffer, 1, 16, stdin)))
	aff_md5_update(&context, buffer, len);
    aff_md5_final(digest, &context);

    MDPrint(digest);
    printf ("\n");
}

/* Prints a message digest in hexadecimal.
 */
static void
MDPrint(uint8_t digest[16])
{
    int i;

    for (i = 0; i < 16; i++)
	printf("%02x", digest[i]);
}
