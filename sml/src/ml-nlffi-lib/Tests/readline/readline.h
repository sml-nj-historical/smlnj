/* readline.h
 *
 *   Prototypes for functions from readline library that
 *   are used in this example.
 *   (This file is read by ml-nlffigen to generate correctly typed
 *   FFI bindings.)
 *
 * Copyright (c) 2004 by Toyota Technological Institute at Chicago
 *
 * Author: Matthias Blume (blume@tti-c.org)
 */
extern char *readline (const char *);
extern void using_history (void);
extern void stifle_history (int);
extern void add_history (const char *);
