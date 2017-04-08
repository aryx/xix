/*s: byterun/gc.h */
/*s: copyright header C damien */
/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*             Damien Doligez, projet Para, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  Automatique.  Distributed only by permission.                      */
/*                                                                     */
/***********************************************************************/
/*e: copyright header C damien */

#ifndef _gc_
#define _gc_


#include "mlvalues.h"

/*s: constant White */
#define White (0 << 8)
/*e: constant White */
/*s: constant Gray */
#define Gray  (1 << 8)
/*e: constant Gray */
/*s: constant Blue */
#define Blue  (2 << 8)
/*e: constant Blue */
/*s: constant Black */
#define Black (3 << 8)
/*e: constant Black */

/*s: function Color_hd */
#define Color_hd(hd) ((color_t) ((hd) & Black))
/*e: function Color_hd */
/*s: function Color_hp */
#define Color_hp(hp) Color_hd (Hd_hp (hp))
/*e: function Color_hp */

/*s: function Is_white_hd */
#define Is_white_hd(hd) (Color_hd (hd) == White)
/*e: function Is_white_hd */
/*s: function Is_gray_hd */
#define Is_gray_hd(hd) (Color_hd (hd) == Gray)
/*e: function Is_gray_hd */
/*s: function Is_blue_hd */
#define Is_blue_hd(hd) (Color_hd (hd) == Blue)
/*e: function Is_blue_hd */
/*s: function Is_black_hd */
#define Is_black_hd(hd) (Color_hd (hd) == Black)
/*e: function Is_black_hd */

/*s: function Whitehd_hd */
#define Whitehd_hd(hd) (((hd)  & ~Black)/*| White*/)
/*e: function Whitehd_hd */
/*s: function Grayhd_hd */
#define Grayhd_hd(hd)  (((hd)  & ~Black)  | Gray)
/*e: function Grayhd_hd */
/*s: function Blackhd_hd */
#define Blackhd_hd(hd) (((hd)/*& ~Black*/)| Black)
/*e: function Blackhd_hd */
/*s: function Bluehd_hd */
#define Bluehd_hd(hd)  (((hd)  & ~Black)  | Blue)
/*e: function Bluehd_hd */

/*s: function Make_header */
/* This depends on the layout of the header.  See [mlvalues.h]. */
#define Make_header(wosize, tag, color)                                       \
       ((header_t) (((header_t) (wosize) << 10)                               \
                    + (color)                                                 \
                    + (tag_t) (tag)))
/*e: function Make_header */

/*s: function Color_val */
#define Color_val(val) (Color_hd (Hd_val (val)))
/*e: function Color_val */

/*s: function Is_white_val */
#define Is_white_val(val) (Color_val(val) == White)
/*e: function Is_white_val */
/*s: function Is_gray_val */
#define Is_gray_val(val) (Color_val(val) == Gray)
/*e: function Is_gray_val */
/*s: function Is_blue_val */
#define Is_blue_val(val) (Color_val(val) == Blue)
/*e: function Is_blue_val */
/*s: function Is_black_val */
#define Is_black_val(val) (Color_val(val) == Black)
/*e: function Is_black_val */


#endif /* _gc_ */
/*e: byterun/gc.h */
