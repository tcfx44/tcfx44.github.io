
      var $parcel$global = globalThis;
    
var $parcel$modules = {};
var $parcel$inits = {};

var parcelRequire = $parcel$global["parcelRequire477f"];

if (parcelRequire == null) {
  parcelRequire = function(id) {
    if (id in $parcel$modules) {
      return $parcel$modules[id].exports;
    }
    if (id in $parcel$inits) {
      var init = $parcel$inits[id];
      delete $parcel$inits[id];
      var module = {id: id, exports: {}};
      $parcel$modules[id] = module;
      init.call(module.exports, module, module.exports);
      return module.exports;
    }
    var err = new Error("Cannot find module '" + id + "'");
    err.code = 'MODULE_NOT_FOUND';
    throw err;
  };

  parcelRequire.register = function register(id, init) {
    $parcel$inits[id] = init;
  };

  $parcel$global["parcelRequire477f"] = parcelRequire;
}

var parcelRegister = parcelRequire.register;
parcelRegister("5GoMm", function(module, exports) {

var $92x9i = parcelRequire("92x9i");
module.exports = $92x9i("7BRz0").then(()=>parcelRequire("gBMsy"));

});
parcelRegister("92x9i", function(module, exports) {
"use strict";

function $694e03a97467efc7$var$load(id) {
    // eslint-disable-next-line no-undef
    return import((parcelRequire("aKzDW")).resolve(id));
}
module.exports = $694e03a97467efc7$var$load;

});


parcelRegister("eV5qQ", function(module, exports) {

var $92x9i = parcelRequire("92x9i");
module.exports = $92x9i("c6HcA").then(()=>parcelRequire("bCV3h"));

});

parcelRegister("3vVHm", function(module, exports) {

var $92x9i = parcelRequire("92x9i");
module.exports = $92x9i("gPTBT").then(()=>parcelRequire("6Auj5"));

});

parcelRegister("2HErZ", function(module, exports) {

var $92x9i = parcelRequire("92x9i");
module.exports = $92x9i("cPMkm").then(()=>parcelRequire("hi4OS"));

});

parcelRegister("erTWI", function(module, exports) {

var $92x9i = parcelRequire("92x9i");
module.exports = $92x9i("5RSoz").then(()=>parcelRequire("lX7el"));

});

parcelRegister("3Cwwi", function(module, exports) {

var $92x9i = parcelRequire("92x9i");
module.exports = $92x9i("g3dGR").then(()=>parcelRequire("52Hyn"));

});

parcelRegister("cPWZz", function(module, exports) {

var $92x9i = parcelRequire("92x9i");
module.exports = $92x9i("5nseh").then(()=>parcelRequire("dWzk5"));

});

class $07442ebb96f65399$export$5b519f82636185ec extends Error {
}
class $07442ebb96f65399$export$66d311bf29d5c89c extends $07442ebb96f65399$export$5b519f82636185ec {
}


/*! pako 2.1.0 https://github.com/nodeca/pako @license (MIT AND Zlib) */ // (C) 1995-2013 Jean-loup Gailly and Mark Adler
// (C) 2014-2017 Vitaly Puzrin and Andrey Tupitsin
//
// This software is provided 'as-is', without any express or implied
// warranty. In no event will the authors be held liable for any damages
// arising from the use of this software.
//
// Permission is granted to anyone to use this software for any purpose,
// including commercial applications, and to alter it and redistribute it
// freely, subject to the following restrictions:
//
// 1. The origin of this software must not be misrepresented; you must not
//   claim that you wrote the original software. If you use this software
//   in a product, an acknowledgment in the product documentation would be
//   appreciated but is not required.
// 2. Altered source versions must be plainly marked as such, and must not be
//   misrepresented as being the original software.
// 3. This notice may not be removed or altered from any source distribution.
/* eslint-disable space-unary-ops */ /* Public constants ==========================================================*/ /* ===========================================================================*/ //const Z_FILTERED          = 1;
//const Z_HUFFMAN_ONLY      = 2;
//const Z_RLE               = 3;
const $1d314fc79f930156$var$Z_FIXED$1 = 4;
//const Z_DEFAULT_STRATEGY  = 0;
/* Possible values of the data_type field (though see inflate()) */ const $1d314fc79f930156$var$Z_BINARY = 0;
const $1d314fc79f930156$var$Z_TEXT = 1;
//const Z_ASCII             = 1; // = Z_TEXT
const $1d314fc79f930156$var$Z_UNKNOWN$1 = 2;
/*============================================================================*/ function $1d314fc79f930156$var$zero$1(buf) {
    let len = buf.length;
    while(--len >= 0)buf[len] = 0;
}
// From zutil.h
const $1d314fc79f930156$var$STORED_BLOCK = 0;
const $1d314fc79f930156$var$STATIC_TREES = 1;
const $1d314fc79f930156$var$DYN_TREES = 2;
/* The three kinds of block type */ const $1d314fc79f930156$var$MIN_MATCH$1 = 3;
const $1d314fc79f930156$var$MAX_MATCH$1 = 258;
/* The minimum and maximum match lengths */ // From deflate.h
/* ===========================================================================
 * Internal compression state.
 */ const $1d314fc79f930156$var$LENGTH_CODES$1 = 29;
/* number of length codes, not counting the special END_BLOCK code */ const $1d314fc79f930156$var$LITERALS$1 = 256;
/* number of literal bytes 0..255 */ const $1d314fc79f930156$var$L_CODES$1 = $1d314fc79f930156$var$LITERALS$1 + 1 + $1d314fc79f930156$var$LENGTH_CODES$1;
/* number of Literal or Length codes, including the END_BLOCK code */ const $1d314fc79f930156$var$D_CODES$1 = 30;
/* number of distance codes */ const $1d314fc79f930156$var$BL_CODES$1 = 19;
/* number of codes used to transfer the bit lengths */ const $1d314fc79f930156$var$HEAP_SIZE$1 = 2 * $1d314fc79f930156$var$L_CODES$1 + 1;
/* maximum heap size */ const $1d314fc79f930156$var$MAX_BITS$1 = 15;
/* All codes must not exceed MAX_BITS bits */ const $1d314fc79f930156$var$Buf_size = 16;
/* size of bit buffer in bi_buf */ /* ===========================================================================
 * Constants
 */ const $1d314fc79f930156$var$MAX_BL_BITS = 7;
/* Bit length codes must not exceed MAX_BL_BITS bits */ const $1d314fc79f930156$var$END_BLOCK = 256;
/* end of block literal code */ const $1d314fc79f930156$var$REP_3_6 = 16;
/* repeat previous bit length 3-6 times (2 bits of repeat count) */ const $1d314fc79f930156$var$REPZ_3_10 = 17;
/* repeat a zero length 3-10 times  (3 bits of repeat count) */ const $1d314fc79f930156$var$REPZ_11_138 = 18;
/* repeat a zero length 11-138 times  (7 bits of repeat count) */ /* eslint-disable comma-spacing,array-bracket-spacing */ const $1d314fc79f930156$var$extra_lbits = /* extra bits for each length code */ new Uint8Array([
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    1,
    1,
    1,
    1,
    2,
    2,
    2,
    2,
    3,
    3,
    3,
    3,
    4,
    4,
    4,
    4,
    5,
    5,
    5,
    5,
    0
]);
const $1d314fc79f930156$var$extra_dbits = /* extra bits for each distance code */ new Uint8Array([
    0,
    0,
    0,
    0,
    1,
    1,
    2,
    2,
    3,
    3,
    4,
    4,
    5,
    5,
    6,
    6,
    7,
    7,
    8,
    8,
    9,
    9,
    10,
    10,
    11,
    11,
    12,
    12,
    13,
    13
]);
const $1d314fc79f930156$var$extra_blbits = /* extra bits for each bit length code */ new Uint8Array([
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    2,
    3,
    7
]);
const $1d314fc79f930156$var$bl_order = new Uint8Array([
    16,
    17,
    18,
    0,
    8,
    7,
    9,
    6,
    10,
    5,
    11,
    4,
    12,
    3,
    13,
    2,
    14,
    1,
    15
]);
/* eslint-enable comma-spacing,array-bracket-spacing */ /* The lengths of the bit length codes are sent in order of decreasing
 * probability, to avoid transmitting the lengths for unused bit length codes.
 */ /* ===========================================================================
 * Local data. These are initialized only once.
 */ // We pre-fill arrays with 0 to avoid uninitialized gaps
const $1d314fc79f930156$var$DIST_CODE_LEN = 512; /* see definition of array dist_code below */ 
// !!!! Use flat array instead of structure, Freq = i*2, Len = i*2+1
const $1d314fc79f930156$var$static_ltree = new Array(($1d314fc79f930156$var$L_CODES$1 + 2) * 2);
$1d314fc79f930156$var$zero$1($1d314fc79f930156$var$static_ltree);
/* The static literal tree. Since the bit lengths are imposed, there is no
 * need for the L_CODES extra codes used during heap construction. However
 * The codes 286 and 287 are needed to build a canonical tree (see _tr_init
 * below).
 */ const $1d314fc79f930156$var$static_dtree = new Array($1d314fc79f930156$var$D_CODES$1 * 2);
$1d314fc79f930156$var$zero$1($1d314fc79f930156$var$static_dtree);
/* The static distance tree. (Actually a trivial tree since all codes use
 * 5 bits.)
 */ const $1d314fc79f930156$var$_dist_code = new Array($1d314fc79f930156$var$DIST_CODE_LEN);
$1d314fc79f930156$var$zero$1($1d314fc79f930156$var$_dist_code);
/* Distance codes. The first 256 values correspond to the distances
 * 3 .. 258, the last 256 values correspond to the top 8 bits of
 * the 15 bit distances.
 */ const $1d314fc79f930156$var$_length_code = new Array($1d314fc79f930156$var$MAX_MATCH$1 - $1d314fc79f930156$var$MIN_MATCH$1 + 1);
$1d314fc79f930156$var$zero$1($1d314fc79f930156$var$_length_code);
/* length code for each normalized match length (0 == MIN_MATCH) */ const $1d314fc79f930156$var$base_length = new Array($1d314fc79f930156$var$LENGTH_CODES$1);
$1d314fc79f930156$var$zero$1($1d314fc79f930156$var$base_length);
/* First normalized length for each code (0 = MIN_MATCH) */ const $1d314fc79f930156$var$base_dist = new Array($1d314fc79f930156$var$D_CODES$1);
$1d314fc79f930156$var$zero$1($1d314fc79f930156$var$base_dist);
/* First normalized distance for each code (0 = distance of 1) */ function $1d314fc79f930156$var$StaticTreeDesc(static_tree, extra_bits, extra_base, elems, max_length) {
    this.static_tree = static_tree; /* static tree or NULL */ 
    this.extra_bits = extra_bits; /* extra bits for each code or NULL */ 
    this.extra_base = extra_base; /* base index for extra_bits */ 
    this.elems = elems; /* max number of elements in the tree */ 
    this.max_length = max_length; /* max bit length for the codes */ 
    // show if `static_tree` has data or dummy - needed for monomorphic objects
    this.has_stree = static_tree && static_tree.length;
}
let $1d314fc79f930156$var$static_l_desc;
let $1d314fc79f930156$var$static_d_desc;
let $1d314fc79f930156$var$static_bl_desc;
function $1d314fc79f930156$var$TreeDesc(dyn_tree, stat_desc) {
    this.dyn_tree = dyn_tree; /* the dynamic tree */ 
    this.max_code = 0; /* largest code with non zero frequency */ 
    this.stat_desc = stat_desc; /* the corresponding static tree */ 
}
const $1d314fc79f930156$var$d_code = (dist)=>{
    return dist < 256 ? $1d314fc79f930156$var$_dist_code[dist] : $1d314fc79f930156$var$_dist_code[256 + (dist >>> 7)];
};
/* ===========================================================================
 * Output a short LSB first on the stream.
 * IN assertion: there is enough room in pendingBuf.
 */ const $1d314fc79f930156$var$put_short = (s, w)=>{
    //    put_byte(s, (uch)((w) & 0xff));
    //    put_byte(s, (uch)((ush)(w) >> 8));
    s.pending_buf[s.pending++] = w & 0xff;
    s.pending_buf[s.pending++] = w >>> 8 & 0xff;
};
/* ===========================================================================
 * Send a value on a given number of bits.
 * IN assertion: length <= 16 and value fits in length bits.
 */ const $1d314fc79f930156$var$send_bits = (s, value, length)=>{
    if (s.bi_valid > $1d314fc79f930156$var$Buf_size - length) {
        s.bi_buf |= value << s.bi_valid & 0xffff;
        $1d314fc79f930156$var$put_short(s, s.bi_buf);
        s.bi_buf = value >> $1d314fc79f930156$var$Buf_size - s.bi_valid;
        s.bi_valid += length - $1d314fc79f930156$var$Buf_size;
    } else {
        s.bi_buf |= value << s.bi_valid & 0xffff;
        s.bi_valid += length;
    }
};
const $1d314fc79f930156$var$send_code = (s, c, tree)=>{
    $1d314fc79f930156$var$send_bits(s, tree[c * 2], tree[c * 2 + 1]);
};
/* ===========================================================================
 * Reverse the first len bits of a code, using straightforward code (a faster
 * method would use a table)
 * IN assertion: 1 <= len <= 15
 */ const $1d314fc79f930156$var$bi_reverse = (code, len)=>{
    let res = 0;
    do {
        res |= code & 1;
        code >>>= 1;
        res <<= 1;
    }while (--len > 0);
    return res >>> 1;
};
/* ===========================================================================
 * Flush the bit buffer, keeping at most 7 bits in it.
 */ const $1d314fc79f930156$var$bi_flush = (s)=>{
    if (s.bi_valid === 16) {
        $1d314fc79f930156$var$put_short(s, s.bi_buf);
        s.bi_buf = 0;
        s.bi_valid = 0;
    } else if (s.bi_valid >= 8) {
        s.pending_buf[s.pending++] = s.bi_buf & 0xff;
        s.bi_buf >>= 8;
        s.bi_valid -= 8;
    }
};
/* ===========================================================================
 * Compute the optimal bit lengths for a tree and update the total bit length
 * for the current block.
 * IN assertion: the fields freq and dad are set, heap[heap_max] and
 *    above are the tree nodes sorted by increasing frequency.
 * OUT assertions: the field len is set to the optimal bit length, the
 *     array bl_count contains the frequencies for each bit length.
 *     The length opt_len is updated; static_len is also updated if stree is
 *     not null.
 */ const $1d314fc79f930156$var$gen_bitlen = (s, desc)=>{
    //    deflate_state *s;
    //    tree_desc *desc;    /* the tree descriptor */
    const tree = desc.dyn_tree;
    const max_code = desc.max_code;
    const stree = desc.stat_desc.static_tree;
    const has_stree = desc.stat_desc.has_stree;
    const extra = desc.stat_desc.extra_bits;
    const base = desc.stat_desc.extra_base;
    const max_length = desc.stat_desc.max_length;
    let h; /* heap index */ 
    let n, m; /* iterate over the tree elements */ 
    let bits; /* bit length */ 
    let xbits; /* extra bits */ 
    let f; /* frequency */ 
    let overflow = 0; /* number of elements with bit length too large */ 
    for(bits = 0; bits <= $1d314fc79f930156$var$MAX_BITS$1; bits++)s.bl_count[bits] = 0;
    /* In a first pass, compute the optimal bit lengths (which may
   * overflow in the case of the bit length tree).
   */ tree[s.heap[s.heap_max] * 2 + 1] = 0; /* root of the heap */ 
    for(h = s.heap_max + 1; h < $1d314fc79f930156$var$HEAP_SIZE$1; h++){
        n = s.heap[h];
        bits = tree[tree[n * 2 + 1] * 2 + 1] + 1;
        if (bits > max_length) {
            bits = max_length;
            overflow++;
        }
        tree[n * 2 + 1] = bits;
        /* We overwrite tree[n].Dad which is no longer needed */ if (n > max_code) continue;
         /* not a leaf node */ 
        s.bl_count[bits]++;
        xbits = 0;
        if (n >= base) xbits = extra[n - base];
        f = tree[n * 2] /*.Freq*/ ;
        s.opt_len += f * (bits + xbits);
        if (has_stree) s.static_len += f * (stree[n * 2 + 1] + xbits);
    }
    if (overflow === 0) return;
    // Tracev((stderr,"\nbit length overflow\n"));
    /* This happens for example on obj2 and pic of the Calgary corpus */ /* Find the first bit length which could increase: */ do {
        bits = max_length - 1;
        while(s.bl_count[bits] === 0)bits--;
        s.bl_count[bits]--; /* move one leaf down the tree */ 
        s.bl_count[bits + 1] += 2; /* move one overflow item as its brother */ 
        s.bl_count[max_length]--;
        /* The brother of the overflow item also moves one step up,
     * but this does not affect bl_count[max_length]
     */ overflow -= 2;
    }while (overflow > 0);
    /* Now recompute all bit lengths, scanning in increasing frequency.
   * h is still equal to HEAP_SIZE. (It is simpler to reconstruct all
   * lengths instead of fixing only the wrong ones. This idea is taken
   * from 'ar' written by Haruhiko Okumura.)
   */ for(bits = max_length; bits !== 0; bits--){
        n = s.bl_count[bits];
        while(n !== 0){
            m = s.heap[--h];
            if (m > max_code) continue;
            if (tree[m * 2 + 1] !== bits) {
                // Tracev((stderr,"code %d bits %d->%d\n", m, tree[m].Len, bits));
                s.opt_len += (bits - tree[m * 2 + 1]) * tree[m * 2] /*.Freq*/ ;
                tree[m * 2 + 1] = bits;
            }
            n--;
        }
    }
};
/* ===========================================================================
 * Generate the codes for a given tree and bit counts (which need not be
 * optimal).
 * IN assertion: the array bl_count contains the bit length statistics for
 * the given tree and the field len is set for all tree elements.
 * OUT assertion: the field code is set for all tree elements of non
 *     zero code length.
 */ const $1d314fc79f930156$var$gen_codes = (tree, max_code, bl_count)=>{
    //    ct_data *tree;             /* the tree to decorate */
    //    int max_code;              /* largest code with non zero frequency */
    //    ushf *bl_count;            /* number of codes at each bit length */
    const next_code = new Array($1d314fc79f930156$var$MAX_BITS$1 + 1); /* next code value for each bit length */ 
    let code = 0; /* running code value */ 
    let bits; /* bit index */ 
    let n; /* code index */ 
    /* The distribution counts are first used to generate the code values
   * without bit reversal.
   */ for(bits = 1; bits <= $1d314fc79f930156$var$MAX_BITS$1; bits++){
        code = code + bl_count[bits - 1] << 1;
        next_code[bits] = code;
    }
    /* Check that the bit counts in bl_count are consistent. The last code
   * must be all ones.
   */ //Assert (code + bl_count[MAX_BITS]-1 == (1<<MAX_BITS)-1,
    //        "inconsistent bit counts");
    //Tracev((stderr,"\ngen_codes: max_code %d ", max_code));
    for(n = 0; n <= max_code; n++){
        let len = tree[n * 2 + 1] /*.Len*/ ;
        if (len === 0) continue;
        /* Now reverse the bits */ tree[n * 2] = $1d314fc79f930156$var$bi_reverse(next_code[len]++, len);
    //Tracecv(tree != static_ltree, (stderr,"\nn %3d %c l %2d c %4x (%x) ",
    //     n, (isgraph(n) ? n : ' '), len, tree[n].Code, next_code[len]-1));
    }
};
/* ===========================================================================
 * Initialize the various 'constant' tables.
 */ const $1d314fc79f930156$var$tr_static_init = ()=>{
    let n; /* iterates over tree elements */ 
    let bits; /* bit counter */ 
    let length; /* length value */ 
    let code; /* code value */ 
    let dist; /* distance index */ 
    const bl_count = new Array($1d314fc79f930156$var$MAX_BITS$1 + 1);
    /* number of codes at each bit length for an optimal tree */ // do check in _tr_init()
    //if (static_init_done) return;
    /* For some embedded targets, global variables are not initialized: */ /*#ifdef NO_INIT_GLOBAL_POINTERS
  static_l_desc.static_tree = static_ltree;
  static_l_desc.extra_bits = extra_lbits;
  static_d_desc.static_tree = static_dtree;
  static_d_desc.extra_bits = extra_dbits;
  static_bl_desc.extra_bits = extra_blbits;
#endif*/ /* Initialize the mapping length (0..255) -> length code (0..28) */ length = 0;
    for(code = 0; code < $1d314fc79f930156$var$LENGTH_CODES$1 - 1; code++){
        $1d314fc79f930156$var$base_length[code] = length;
        for(n = 0; n < 1 << $1d314fc79f930156$var$extra_lbits[code]; n++)$1d314fc79f930156$var$_length_code[length++] = code;
    }
    //Assert (length == 256, "tr_static_init: length != 256");
    /* Note that the length 255 (match length 258) can be represented
   * in two different ways: code 284 + 5 bits or code 285, so we
   * overwrite length_code[255] to use the best encoding:
   */ $1d314fc79f930156$var$_length_code[length - 1] = code;
    /* Initialize the mapping dist (0..32K) -> dist code (0..29) */ dist = 0;
    for(code = 0; code < 16; code++){
        $1d314fc79f930156$var$base_dist[code] = dist;
        for(n = 0; n < 1 << $1d314fc79f930156$var$extra_dbits[code]; n++)$1d314fc79f930156$var$_dist_code[dist++] = code;
    }
    //Assert (dist == 256, "tr_static_init: dist != 256");
    dist >>= 7; /* from now on, all distances are divided by 128 */ 
    for(; code < $1d314fc79f930156$var$D_CODES$1; code++){
        $1d314fc79f930156$var$base_dist[code] = dist << 7;
        for(n = 0; n < 1 << $1d314fc79f930156$var$extra_dbits[code] - 7; n++)$1d314fc79f930156$var$_dist_code[256 + dist++] = code;
    }
    //Assert (dist == 256, "tr_static_init: 256+dist != 512");
    /* Construct the codes of the static literal tree */ for(bits = 0; bits <= $1d314fc79f930156$var$MAX_BITS$1; bits++)bl_count[bits] = 0;
    n = 0;
    while(n <= 143){
        $1d314fc79f930156$var$static_ltree[n * 2 + 1] = 8;
        n++;
        bl_count[8]++;
    }
    while(n <= 255){
        $1d314fc79f930156$var$static_ltree[n * 2 + 1] = 9;
        n++;
        bl_count[9]++;
    }
    while(n <= 279){
        $1d314fc79f930156$var$static_ltree[n * 2 + 1] = 7;
        n++;
        bl_count[7]++;
    }
    while(n <= 287){
        $1d314fc79f930156$var$static_ltree[n * 2 + 1] = 8;
        n++;
        bl_count[8]++;
    }
    /* Codes 286 and 287 do not exist, but we must include them in the
   * tree construction to get a canonical Huffman tree (longest code
   * all ones)
   */ $1d314fc79f930156$var$gen_codes($1d314fc79f930156$var$static_ltree, $1d314fc79f930156$var$L_CODES$1 + 1, bl_count);
    /* The static distance tree is trivial: */ for(n = 0; n < $1d314fc79f930156$var$D_CODES$1; n++){
        $1d314fc79f930156$var$static_dtree[n * 2 + 1] = 5;
        $1d314fc79f930156$var$static_dtree[n * 2] = $1d314fc79f930156$var$bi_reverse(n, 5);
    }
    // Now data ready and we can init static trees
    $1d314fc79f930156$var$static_l_desc = new $1d314fc79f930156$var$StaticTreeDesc($1d314fc79f930156$var$static_ltree, $1d314fc79f930156$var$extra_lbits, $1d314fc79f930156$var$LITERALS$1 + 1, $1d314fc79f930156$var$L_CODES$1, $1d314fc79f930156$var$MAX_BITS$1);
    $1d314fc79f930156$var$static_d_desc = new $1d314fc79f930156$var$StaticTreeDesc($1d314fc79f930156$var$static_dtree, $1d314fc79f930156$var$extra_dbits, 0, $1d314fc79f930156$var$D_CODES$1, $1d314fc79f930156$var$MAX_BITS$1);
    $1d314fc79f930156$var$static_bl_desc = new $1d314fc79f930156$var$StaticTreeDesc(new Array(0), $1d314fc79f930156$var$extra_blbits, 0, $1d314fc79f930156$var$BL_CODES$1, $1d314fc79f930156$var$MAX_BL_BITS);
//static_init_done = true;
};
/* ===========================================================================
 * Initialize a new block.
 */ const $1d314fc79f930156$var$init_block = (s)=>{
    let n; /* iterates over tree elements */ 
    /* Initialize the trees. */ for(n = 0; n < $1d314fc79f930156$var$L_CODES$1; n++)s.dyn_ltree[n * 2] = 0;
    for(n = 0; n < $1d314fc79f930156$var$D_CODES$1; n++)s.dyn_dtree[n * 2] = 0;
    for(n = 0; n < $1d314fc79f930156$var$BL_CODES$1; n++)s.bl_tree[n * 2] = 0;
    s.dyn_ltree[$1d314fc79f930156$var$END_BLOCK * 2] = 1;
    s.opt_len = s.static_len = 0;
    s.sym_next = s.matches = 0;
};
/* ===========================================================================
 * Flush the bit buffer and align the output on a byte boundary
 */ const $1d314fc79f930156$var$bi_windup = (s)=>{
    if (s.bi_valid > 8) $1d314fc79f930156$var$put_short(s, s.bi_buf);
    else if (s.bi_valid > 0) //put_byte(s, (Byte)s->bi_buf);
    s.pending_buf[s.pending++] = s.bi_buf;
    s.bi_buf = 0;
    s.bi_valid = 0;
};
/* ===========================================================================
 * Compares to subtrees, using the tree depth as tie breaker when
 * the subtrees have equal frequency. This minimizes the worst case length.
 */ const $1d314fc79f930156$var$smaller = (tree, n, m, depth)=>{
    const _n2 = n * 2;
    const _m2 = m * 2;
    return tree[_n2] < tree[_m2] || tree[_n2] === tree[_m2] && depth[n] <= depth[m];
};
/* ===========================================================================
 * Restore the heap property by moving down the tree starting at node k,
 * exchanging a node with the smallest of its two sons if necessary, stopping
 * when the heap property is re-established (each father smaller than its
 * two sons).
 */ const $1d314fc79f930156$var$pqdownheap = (s, tree, k)=>{
    //    deflate_state *s;
    //    ct_data *tree;  /* the tree to restore */
    //    int k;               /* node to move down */
    const v = s.heap[k];
    let j = k << 1; /* left son of k */ 
    while(j <= s.heap_len){
        /* Set j to the smallest of the two sons: */ if (j < s.heap_len && $1d314fc79f930156$var$smaller(tree, s.heap[j + 1], s.heap[j], s.depth)) j++;
        /* Exit if v is smaller than both sons */ if ($1d314fc79f930156$var$smaller(tree, v, s.heap[j], s.depth)) break;
        /* Exchange v with the smallest son */ s.heap[k] = s.heap[j];
        k = j;
        /* And continue down the tree, setting j to the left son of k */ j <<= 1;
    }
    s.heap[k] = v;
};
// inlined manually
// const SMALLEST = 1;
/* ===========================================================================
 * Send the block data compressed using the given Huffman trees
 */ const $1d314fc79f930156$var$compress_block = (s, ltree, dtree)=>{
    //    deflate_state *s;
    //    const ct_data *ltree; /* literal tree */
    //    const ct_data *dtree; /* distance tree */
    let dist; /* distance of matched string */ 
    let lc; /* match length or unmatched char (if dist == 0) */ 
    let sx = 0; /* running index in sym_buf */ 
    let code; /* the code to send */ 
    let extra; /* number of extra bits to send */ 
    if (s.sym_next !== 0) do {
        dist = s.pending_buf[s.sym_buf + sx++] & 0xff;
        dist += (s.pending_buf[s.sym_buf + sx++] & 0xff) << 8;
        lc = s.pending_buf[s.sym_buf + sx++];
        if (dist === 0) $1d314fc79f930156$var$send_code(s, lc, ltree); /* send a literal byte */ 
        else {
            /* Here, lc is the match length - MIN_MATCH */ code = $1d314fc79f930156$var$_length_code[lc];
            $1d314fc79f930156$var$send_code(s, code + $1d314fc79f930156$var$LITERALS$1 + 1, ltree); /* send the length code */ 
            extra = $1d314fc79f930156$var$extra_lbits[code];
            if (extra !== 0) {
                lc -= $1d314fc79f930156$var$base_length[code];
                $1d314fc79f930156$var$send_bits(s, lc, extra); /* send the extra length bits */ 
            }
            dist--; /* dist is now the match distance - 1 */ 
            code = $1d314fc79f930156$var$d_code(dist);
            //Assert (code < D_CODES, "bad d_code");
            $1d314fc79f930156$var$send_code(s, code, dtree); /* send the distance code */ 
            extra = $1d314fc79f930156$var$extra_dbits[code];
            if (extra !== 0) {
                dist -= $1d314fc79f930156$var$base_dist[code];
                $1d314fc79f930156$var$send_bits(s, dist, extra); /* send the extra distance bits */ 
            }
        } /* literal or match pair ? */ 
    /* Check that the overlay between pending_buf and sym_buf is ok: */ //Assert(s->pending < s->lit_bufsize + sx, "pendingBuf overflow");
    }while (sx < s.sym_next);
    $1d314fc79f930156$var$send_code(s, $1d314fc79f930156$var$END_BLOCK, ltree);
};
/* ===========================================================================
 * Construct one Huffman tree and assigns the code bit strings and lengths.
 * Update the total bit length for the current block.
 * IN assertion: the field freq is set for all tree elements.
 * OUT assertions: the fields len and code are set to the optimal bit length
 *     and corresponding code. The length opt_len is updated; static_len is
 *     also updated if stree is not null. The field max_code is set.
 */ const $1d314fc79f930156$var$build_tree = (s, desc)=>{
    //    deflate_state *s;
    //    tree_desc *desc; /* the tree descriptor */
    const tree = desc.dyn_tree;
    const stree = desc.stat_desc.static_tree;
    const has_stree = desc.stat_desc.has_stree;
    const elems = desc.stat_desc.elems;
    let n, m; /* iterate over heap elements */ 
    let max_code = -1; /* largest code with non zero frequency */ 
    let node; /* new node being created */ 
    /* Construct the initial heap, with least frequent element in
   * heap[SMALLEST]. The sons of heap[n] are heap[2*n] and heap[2*n+1].
   * heap[0] is not used.
   */ s.heap_len = 0;
    s.heap_max = $1d314fc79f930156$var$HEAP_SIZE$1;
    for(n = 0; n < elems; n++)if (tree[n * 2] !== 0) {
        s.heap[++s.heap_len] = max_code = n;
        s.depth[n] = 0;
    } else tree[n * 2 + 1] = 0;
    /* The pkzip format requires that at least one distance code exists,
   * and that at least one bit should be sent even if there is only one
   * possible code. So to avoid special checks later on we force at least
   * two codes of non zero frequency.
   */ while(s.heap_len < 2){
        node = s.heap[++s.heap_len] = max_code < 2 ? ++max_code : 0;
        tree[node * 2] = 1;
        s.depth[node] = 0;
        s.opt_len--;
        if (has_stree) s.static_len -= stree[node * 2 + 1] /*.Len*/ ;
    /* node is 0 or 1 so it does not have extra bits */ }
    desc.max_code = max_code;
    /* The elements heap[heap_len/2+1 .. heap_len] are leaves of the tree,
   * establish sub-heaps of increasing lengths:
   */ for(n = s.heap_len >> 1 /*int /2*/ ; n >= 1; n--)$1d314fc79f930156$var$pqdownheap(s, tree, n);
    /* Construct the Huffman tree by repeatedly combining the least two
   * frequent nodes.
   */ node = elems; /* next internal node of the tree */ 
    do {
        //pqremove(s, tree, n);  /* n = node of least frequency */
        /*** pqremove ***/ n = s.heap[1 /*SMALLEST*/ ];
        s.heap[1 /*SMALLEST*/ ] = s.heap[s.heap_len--];
        $1d314fc79f930156$var$pqdownheap(s, tree, 1 /*SMALLEST*/ );
        /***/ m = s.heap[1 /*SMALLEST*/ ]; /* m = node of next least frequency */ 
        s.heap[--s.heap_max] = n; /* keep the nodes sorted by frequency */ 
        s.heap[--s.heap_max] = m;
        /* Create a new node father of n and m */ tree[node * 2] = tree[n * 2] + tree[m * 2] /*.Freq*/ ;
        s.depth[node] = (s.depth[n] >= s.depth[m] ? s.depth[n] : s.depth[m]) + 1;
        tree[n * 2 + 1] = tree[m * 2 + 1] = node;
        /* and insert the new node in the heap */ s.heap[1 /*SMALLEST*/ ] = node++;
        $1d314fc79f930156$var$pqdownheap(s, tree, 1 /*SMALLEST*/ );
    }while (s.heap_len >= 2);
    s.heap[--s.heap_max] = s.heap[1 /*SMALLEST*/ ];
    /* At this point, the fields freq and dad are set. We can now
   * generate the bit lengths.
   */ $1d314fc79f930156$var$gen_bitlen(s, desc);
    /* The field len is now set, we can generate the bit codes */ $1d314fc79f930156$var$gen_codes(tree, max_code, s.bl_count);
};
/* ===========================================================================
 * Scan a literal or distance tree to determine the frequencies of the codes
 * in the bit length tree.
 */ const $1d314fc79f930156$var$scan_tree = (s, tree, max_code)=>{
    //    deflate_state *s;
    //    ct_data *tree;   /* the tree to be scanned */
    //    int max_code;    /* and its largest code of non zero frequency */
    let n; /* iterates over all tree elements */ 
    let prevlen = -1; /* last emitted length */ 
    let curlen; /* length of current code */ 
    let nextlen = tree[1] /*.Len*/ ; /* length of next code */ 
    let count = 0; /* repeat count of the current code */ 
    let max_count = 7; /* max repeat count */ 
    let min_count = 4; /* min repeat count */ 
    if (nextlen === 0) {
        max_count = 138;
        min_count = 3;
    }
    tree[(max_code + 1) * 2 + 1] = 0xffff; /* guard */ 
    for(n = 0; n <= max_code; n++){
        curlen = nextlen;
        nextlen = tree[(n + 1) * 2 + 1] /*.Len*/ ;
        if (++count < max_count && curlen === nextlen) continue;
        else if (count < min_count) s.bl_tree[curlen * 2] += count;
        else if (curlen !== 0) {
            if (curlen !== prevlen) s.bl_tree[curlen * 2]++;
            s.bl_tree[$1d314fc79f930156$var$REP_3_6 * 2]++;
        } else if (count <= 10) s.bl_tree[$1d314fc79f930156$var$REPZ_3_10 * 2]++;
        else s.bl_tree[$1d314fc79f930156$var$REPZ_11_138 * 2]++;
        count = 0;
        prevlen = curlen;
        if (nextlen === 0) {
            max_count = 138;
            min_count = 3;
        } else if (curlen === nextlen) {
            max_count = 6;
            min_count = 3;
        } else {
            max_count = 7;
            min_count = 4;
        }
    }
};
/* ===========================================================================
 * Send a literal or distance tree in compressed form, using the codes in
 * bl_tree.
 */ const $1d314fc79f930156$var$send_tree = (s, tree, max_code)=>{
    //    deflate_state *s;
    //    ct_data *tree; /* the tree to be scanned */
    //    int max_code;       /* and its largest code of non zero frequency */
    let n; /* iterates over all tree elements */ 
    let prevlen = -1; /* last emitted length */ 
    let curlen; /* length of current code */ 
    let nextlen = tree[1] /*.Len*/ ; /* length of next code */ 
    let count = 0; /* repeat count of the current code */ 
    let max_count = 7; /* max repeat count */ 
    let min_count = 4; /* min repeat count */ 
    /* tree[max_code+1].Len = -1; */ /* guard already set */ if (nextlen === 0) {
        max_count = 138;
        min_count = 3;
    }
    for(n = 0; n <= max_code; n++){
        curlen = nextlen;
        nextlen = tree[(n + 1) * 2 + 1] /*.Len*/ ;
        if (++count < max_count && curlen === nextlen) continue;
        else if (count < min_count) do $1d314fc79f930156$var$send_code(s, curlen, s.bl_tree);
        while (--count !== 0);
        else if (curlen !== 0) {
            if (curlen !== prevlen) {
                $1d314fc79f930156$var$send_code(s, curlen, s.bl_tree);
                count--;
            }
            //Assert(count >= 3 && count <= 6, " 3_6?");
            $1d314fc79f930156$var$send_code(s, $1d314fc79f930156$var$REP_3_6, s.bl_tree);
            $1d314fc79f930156$var$send_bits(s, count - 3, 2);
        } else if (count <= 10) {
            $1d314fc79f930156$var$send_code(s, $1d314fc79f930156$var$REPZ_3_10, s.bl_tree);
            $1d314fc79f930156$var$send_bits(s, count - 3, 3);
        } else {
            $1d314fc79f930156$var$send_code(s, $1d314fc79f930156$var$REPZ_11_138, s.bl_tree);
            $1d314fc79f930156$var$send_bits(s, count - 11, 7);
        }
        count = 0;
        prevlen = curlen;
        if (nextlen === 0) {
            max_count = 138;
            min_count = 3;
        } else if (curlen === nextlen) {
            max_count = 6;
            min_count = 3;
        } else {
            max_count = 7;
            min_count = 4;
        }
    }
};
/* ===========================================================================
 * Construct the Huffman tree for the bit lengths and return the index in
 * bl_order of the last bit length code to send.
 */ const $1d314fc79f930156$var$build_bl_tree = (s)=>{
    let max_blindex; /* index of last bit length code of non zero freq */ 
    /* Determine the bit length frequencies for literal and distance trees */ $1d314fc79f930156$var$scan_tree(s, s.dyn_ltree, s.l_desc.max_code);
    $1d314fc79f930156$var$scan_tree(s, s.dyn_dtree, s.d_desc.max_code);
    /* Build the bit length tree: */ $1d314fc79f930156$var$build_tree(s, s.bl_desc);
    /* opt_len now includes the length of the tree representations, except
   * the lengths of the bit lengths codes and the 5+5+4 bits for the counts.
   */ /* Determine the number of bit length codes to send. The pkzip format
   * requires that at least 4 bit length codes be sent. (appnote.txt says
   * 3 but the actual value used is 4.)
   */ for(max_blindex = $1d314fc79f930156$var$BL_CODES$1 - 1; max_blindex >= 3; max_blindex--){
        if (s.bl_tree[$1d314fc79f930156$var$bl_order[max_blindex] * 2 + 1] !== 0) break;
    }
    /* Update opt_len to include the bit length tree and counts */ s.opt_len += 3 * (max_blindex + 1) + 5 + 5 + 4;
    //Tracev((stderr, "\ndyn trees: dyn %ld, stat %ld",
    //        s->opt_len, s->static_len));
    return max_blindex;
};
/* ===========================================================================
 * Send the header for a block using dynamic Huffman trees: the counts, the
 * lengths of the bit length codes, the literal tree and the distance tree.
 * IN assertion: lcodes >= 257, dcodes >= 1, blcodes >= 4.
 */ const $1d314fc79f930156$var$send_all_trees = (s, lcodes, dcodes, blcodes)=>{
    //    deflate_state *s;
    //    int lcodes, dcodes, blcodes; /* number of codes for each tree */
    let rank; /* index in bl_order */ 
    //Assert (lcodes >= 257 && dcodes >= 1 && blcodes >= 4, "not enough codes");
    //Assert (lcodes <= L_CODES && dcodes <= D_CODES && blcodes <= BL_CODES,
    //        "too many codes");
    //Tracev((stderr, "\nbl counts: "));
    $1d314fc79f930156$var$send_bits(s, lcodes - 257, 5); /* not +255 as stated in appnote.txt */ 
    $1d314fc79f930156$var$send_bits(s, dcodes - 1, 5);
    $1d314fc79f930156$var$send_bits(s, blcodes - 4, 4); /* not -3 as stated in appnote.txt */ 
    for(rank = 0; rank < blcodes; rank++)//Tracev((stderr, "\nbl code %2d ", bl_order[rank]));
    $1d314fc79f930156$var$send_bits(s, s.bl_tree[$1d314fc79f930156$var$bl_order[rank] * 2 + 1], 3);
    //Tracev((stderr, "\nbl tree: sent %ld", s->bits_sent));
    $1d314fc79f930156$var$send_tree(s, s.dyn_ltree, lcodes - 1); /* literal tree */ 
    //Tracev((stderr, "\nlit tree: sent %ld", s->bits_sent));
    $1d314fc79f930156$var$send_tree(s, s.dyn_dtree, dcodes - 1); /* distance tree */ 
//Tracev((stderr, "\ndist tree: sent %ld", s->bits_sent));
};
/* ===========================================================================
 * Check if the data type is TEXT or BINARY, using the following algorithm:
 * - TEXT if the two conditions below are satisfied:
 *    a) There are no non-portable control characters belonging to the
 *       "block list" (0..6, 14..25, 28..31).
 *    b) There is at least one printable character belonging to the
 *       "allow list" (9 {TAB}, 10 {LF}, 13 {CR}, 32..255).
 * - BINARY otherwise.
 * - The following partially-portable control characters form a
 *   "gray list" that is ignored in this detection algorithm:
 *   (7 {BEL}, 8 {BS}, 11 {VT}, 12 {FF}, 26 {SUB}, 27 {ESC}).
 * IN assertion: the fields Freq of dyn_ltree are set.
 */ const $1d314fc79f930156$var$detect_data_type = (s)=>{
    /* block_mask is the bit mask of block-listed bytes
   * set bits 0..6, 14..25, and 28..31
   * 0xf3ffc07f = binary 11110011111111111100000001111111
   */ let block_mask = 0xf3ffc07f;
    let n;
    /* Check for non-textual ("block-listed") bytes. */ for(n = 0; n <= 31; n++, block_mask >>>= 1){
        if (block_mask & 1 && s.dyn_ltree[n * 2] !== 0) return $1d314fc79f930156$var$Z_BINARY;
    }
    /* Check for textual ("allow-listed") bytes. */ if (s.dyn_ltree[18] !== 0 || s.dyn_ltree[20] !== 0 || s.dyn_ltree[26] !== 0) return $1d314fc79f930156$var$Z_TEXT;
    for(n = 32; n < $1d314fc79f930156$var$LITERALS$1; n++){
        if (s.dyn_ltree[n * 2] !== 0) return $1d314fc79f930156$var$Z_TEXT;
    }
    /* There are no "block-listed" or "allow-listed" bytes:
   * this stream either is empty or has tolerated ("gray-listed") bytes only.
   */ return $1d314fc79f930156$var$Z_BINARY;
};
let $1d314fc79f930156$var$static_init_done = false;
/* ===========================================================================
 * Initialize the tree data structures for a new zlib stream.
 */ const $1d314fc79f930156$var$_tr_init$1 = (s)=>{
    if (!$1d314fc79f930156$var$static_init_done) {
        $1d314fc79f930156$var$tr_static_init();
        $1d314fc79f930156$var$static_init_done = true;
    }
    s.l_desc = new $1d314fc79f930156$var$TreeDesc(s.dyn_ltree, $1d314fc79f930156$var$static_l_desc);
    s.d_desc = new $1d314fc79f930156$var$TreeDesc(s.dyn_dtree, $1d314fc79f930156$var$static_d_desc);
    s.bl_desc = new $1d314fc79f930156$var$TreeDesc(s.bl_tree, $1d314fc79f930156$var$static_bl_desc);
    s.bi_buf = 0;
    s.bi_valid = 0;
    /* Initialize the first block of the first file: */ $1d314fc79f930156$var$init_block(s);
};
/* ===========================================================================
 * Send a stored block
 */ const $1d314fc79f930156$var$_tr_stored_block$1 = (s, buf, stored_len, last)=>{
    //DeflateState *s;
    //charf *buf;       /* input block */
    //ulg stored_len;   /* length of input block */
    //int last;         /* one if this is the last block for a file */
    $1d314fc79f930156$var$send_bits(s, ($1d314fc79f930156$var$STORED_BLOCK << 1) + (last ? 1 : 0), 3); /* send block type */ 
    $1d314fc79f930156$var$bi_windup(s); /* align on byte boundary */ 
    $1d314fc79f930156$var$put_short(s, stored_len);
    $1d314fc79f930156$var$put_short(s, ~stored_len);
    if (stored_len) s.pending_buf.set(s.window.subarray(buf, buf + stored_len), s.pending);
    s.pending += stored_len;
};
/* ===========================================================================
 * Send one empty static block to give enough lookahead for inflate.
 * This takes 10 bits, of which 7 may remain in the bit buffer.
 */ const $1d314fc79f930156$var$_tr_align$1 = (s)=>{
    $1d314fc79f930156$var$send_bits(s, $1d314fc79f930156$var$STATIC_TREES << 1, 3);
    $1d314fc79f930156$var$send_code(s, $1d314fc79f930156$var$END_BLOCK, $1d314fc79f930156$var$static_ltree);
    $1d314fc79f930156$var$bi_flush(s);
};
/* ===========================================================================
 * Determine the best encoding for the current block: dynamic trees, static
 * trees or store, and write out the encoded block.
 */ const $1d314fc79f930156$var$_tr_flush_block$1 = (s, buf, stored_len, last)=>{
    //DeflateState *s;
    //charf *buf;       /* input block, or NULL if too old */
    //ulg stored_len;   /* length of input block */
    //int last;         /* one if this is the last block for a file */
    let opt_lenb, static_lenb; /* opt_len and static_len in bytes */ 
    let max_blindex = 0; /* index of last bit length code of non zero freq */ 
    /* Build the Huffman trees unless a stored block is forced */ if (s.level > 0) {
        /* Check if the file is binary or text */ if (s.strm.data_type === $1d314fc79f930156$var$Z_UNKNOWN$1) s.strm.data_type = $1d314fc79f930156$var$detect_data_type(s);
        /* Construct the literal and distance trees */ $1d314fc79f930156$var$build_tree(s, s.l_desc);
        // Tracev((stderr, "\nlit data: dyn %ld, stat %ld", s->opt_len,
        //        s->static_len));
        $1d314fc79f930156$var$build_tree(s, s.d_desc);
        // Tracev((stderr, "\ndist data: dyn %ld, stat %ld", s->opt_len,
        //        s->static_len));
        /* At this point, opt_len and static_len are the total bit lengths of
     * the compressed block data, excluding the tree representations.
     */ /* Build the bit length tree for the above two trees, and get the index
     * in bl_order of the last bit length code to send.
     */ max_blindex = $1d314fc79f930156$var$build_bl_tree(s);
        /* Determine the best encoding. Compute the block lengths in bytes. */ opt_lenb = s.opt_len + 3 + 7 >>> 3;
        static_lenb = s.static_len + 3 + 7 >>> 3;
        // Tracev((stderr, "\nopt %lu(%lu) stat %lu(%lu) stored %lu lit %u ",
        //        opt_lenb, s->opt_len, static_lenb, s->static_len, stored_len,
        //        s->sym_next / 3));
        if (static_lenb <= opt_lenb) opt_lenb = static_lenb;
    } else // Assert(buf != (char*)0, "lost buf");
    opt_lenb = static_lenb = stored_len + 5; /* force a stored block */ 
    if (stored_len + 4 <= opt_lenb && buf !== -1) /* 4: two words for the lengths */ /* The test buf != NULL is only necessary if LIT_BUFSIZE > WSIZE.
     * Otherwise we can't have processed more than WSIZE input bytes since
     * the last block flush, because compression would have been
     * successful. If LIT_BUFSIZE <= WSIZE, it is never too late to
     * transform a block into a stored block.
     */ $1d314fc79f930156$var$_tr_stored_block$1(s, buf, stored_len, last);
    else if (s.strategy === $1d314fc79f930156$var$Z_FIXED$1 || static_lenb === opt_lenb) {
        $1d314fc79f930156$var$send_bits(s, ($1d314fc79f930156$var$STATIC_TREES << 1) + (last ? 1 : 0), 3);
        $1d314fc79f930156$var$compress_block(s, $1d314fc79f930156$var$static_ltree, $1d314fc79f930156$var$static_dtree);
    } else {
        $1d314fc79f930156$var$send_bits(s, ($1d314fc79f930156$var$DYN_TREES << 1) + (last ? 1 : 0), 3);
        $1d314fc79f930156$var$send_all_trees(s, s.l_desc.max_code + 1, s.d_desc.max_code + 1, max_blindex + 1);
        $1d314fc79f930156$var$compress_block(s, s.dyn_ltree, s.dyn_dtree);
    }
    // Assert (s->compressed_len == s->bits_sent, "bad compressed size");
    /* The above check is made mod 2^32, for files larger than 512 MB
   * and uLong implemented on 32 bits.
   */ $1d314fc79f930156$var$init_block(s);
    if (last) $1d314fc79f930156$var$bi_windup(s);
// Tracev((stderr,"\ncomprlen %lu(%lu) ", s->compressed_len>>3,
//       s->compressed_len-7*last));
};
/* ===========================================================================
 * Save the match info and tally the frequency counts. Return true if
 * the current block must be flushed.
 */ const $1d314fc79f930156$var$_tr_tally$1 = (s, dist, lc)=>{
    //    deflate_state *s;
    //    unsigned dist;  /* distance of matched string */
    //    unsigned lc;    /* match length-MIN_MATCH or unmatched char (if dist==0) */
    s.pending_buf[s.sym_buf + s.sym_next++] = dist;
    s.pending_buf[s.sym_buf + s.sym_next++] = dist >> 8;
    s.pending_buf[s.sym_buf + s.sym_next++] = lc;
    if (dist === 0) /* lc is the unmatched char */ s.dyn_ltree[lc * 2]++;
    else {
        s.matches++;
        /* Here, lc is the match length - MIN_MATCH */ dist--; /* dist = match distance - 1 */ 
        //Assert((ush)dist < (ush)MAX_DIST(s) &&
        //       (ush)lc <= (ush)(MAX_MATCH-MIN_MATCH) &&
        //       (ush)d_code(dist) < (ush)D_CODES,  "_tr_tally: bad match");
        s.dyn_ltree[($1d314fc79f930156$var$_length_code[lc] + $1d314fc79f930156$var$LITERALS$1 + 1) * 2]++;
        s.dyn_dtree[$1d314fc79f930156$var$d_code(dist) * 2]++;
    }
    return s.sym_next === s.sym_end;
};
var $1d314fc79f930156$var$_tr_init_1 = $1d314fc79f930156$var$_tr_init$1;
var $1d314fc79f930156$var$_tr_stored_block_1 = $1d314fc79f930156$var$_tr_stored_block$1;
var $1d314fc79f930156$var$_tr_flush_block_1 = $1d314fc79f930156$var$_tr_flush_block$1;
var $1d314fc79f930156$var$_tr_tally_1 = $1d314fc79f930156$var$_tr_tally$1;
var $1d314fc79f930156$var$_tr_align_1 = $1d314fc79f930156$var$_tr_align$1;
var $1d314fc79f930156$var$trees = {
    _tr_init: $1d314fc79f930156$var$_tr_init_1,
    _tr_stored_block: $1d314fc79f930156$var$_tr_stored_block_1,
    _tr_flush_block: $1d314fc79f930156$var$_tr_flush_block_1,
    _tr_tally: $1d314fc79f930156$var$_tr_tally_1,
    _tr_align: $1d314fc79f930156$var$_tr_align_1
};
// Note: adler32 takes 12% for level 0 and 2% for level 6.
// It isn't worth it to make additional optimizations as in original.
// Small size is preferable.
// (C) 1995-2013 Jean-loup Gailly and Mark Adler
// (C) 2014-2017 Vitaly Puzrin and Andrey Tupitsin
//
// This software is provided 'as-is', without any express or implied
// warranty. In no event will the authors be held liable for any damages
// arising from the use of this software.
//
// Permission is granted to anyone to use this software for any purpose,
// including commercial applications, and to alter it and redistribute it
// freely, subject to the following restrictions:
//
// 1. The origin of this software must not be misrepresented; you must not
//   claim that you wrote the original software. If you use this software
//   in a product, an acknowledgment in the product documentation would be
//   appreciated but is not required.
// 2. Altered source versions must be plainly marked as such, and must not be
//   misrepresented as being the original software.
// 3. This notice may not be removed or altered from any source distribution.
const $1d314fc79f930156$var$adler32 = (adler, buf, len, pos)=>{
    let s1 = adler & 0xffff | 0, s2 = adler >>> 16 & 0xffff | 0, n = 0;
    while(len !== 0){
        // Set limit ~ twice less than 5552, to keep
        // s2 in 31-bits, because we force signed ints.
        // in other case %= will fail.
        n = len > 2000 ? 2000 : len;
        len -= n;
        do {
            s1 = s1 + buf[pos++] | 0;
            s2 = s2 + s1 | 0;
        }while (--n);
        s1 %= 65521;
        s2 %= 65521;
    }
    return s1 | s2 << 16 | 0;
};
var $1d314fc79f930156$var$adler32_1 = $1d314fc79f930156$var$adler32;
// Note: we can't get significant speed boost here.
// So write code to minimize size - no pregenerated tables
// and array tools dependencies.
// (C) 1995-2013 Jean-loup Gailly and Mark Adler
// (C) 2014-2017 Vitaly Puzrin and Andrey Tupitsin
//
// This software is provided 'as-is', without any express or implied
// warranty. In no event will the authors be held liable for any damages
// arising from the use of this software.
//
// Permission is granted to anyone to use this software for any purpose,
// including commercial applications, and to alter it and redistribute it
// freely, subject to the following restrictions:
//
// 1. The origin of this software must not be misrepresented; you must not
//   claim that you wrote the original software. If you use this software
//   in a product, an acknowledgment in the product documentation would be
//   appreciated but is not required.
// 2. Altered source versions must be plainly marked as such, and must not be
//   misrepresented as being the original software.
// 3. This notice may not be removed or altered from any source distribution.
// Use ordinary array, since untyped makes no boost here
const $1d314fc79f930156$var$makeTable = ()=>{
    let c, table = [];
    for(var n = 0; n < 256; n++){
        c = n;
        for(var k = 0; k < 8; k++)c = c & 1 ? 0xEDB88320 ^ c >>> 1 : c >>> 1;
        table[n] = c;
    }
    return table;
};
// Create table on load. Just 255 signed longs. Not a problem.
const $1d314fc79f930156$var$crcTable = new Uint32Array($1d314fc79f930156$var$makeTable());
const $1d314fc79f930156$var$crc32 = (crc, buf, len, pos)=>{
    const t = $1d314fc79f930156$var$crcTable;
    const end = pos + len;
    crc ^= -1;
    for(let i = pos; i < end; i++)crc = crc >>> 8 ^ t[(crc ^ buf[i]) & 0xFF];
    return crc ^ -1; // >>> 0;
};
var $1d314fc79f930156$var$crc32_1 = $1d314fc79f930156$var$crc32;
// (C) 1995-2013 Jean-loup Gailly and Mark Adler
// (C) 2014-2017 Vitaly Puzrin and Andrey Tupitsin
//
// This software is provided 'as-is', without any express or implied
// warranty. In no event will the authors be held liable for any damages
// arising from the use of this software.
//
// Permission is granted to anyone to use this software for any purpose,
// including commercial applications, and to alter it and redistribute it
// freely, subject to the following restrictions:
//
// 1. The origin of this software must not be misrepresented; you must not
//   claim that you wrote the original software. If you use this software
//   in a product, an acknowledgment in the product documentation would be
//   appreciated but is not required.
// 2. Altered source versions must be plainly marked as such, and must not be
//   misrepresented as being the original software.
// 3. This notice may not be removed or altered from any source distribution.
var $1d314fc79f930156$var$messages = {
    2: "need dictionary",
    /* Z_NEED_DICT       2  */ 1: "stream end",
    /* Z_STREAM_END      1  */ 0: "",
    /* Z_OK              0  */ "-1": "file error",
    /* Z_ERRNO         (-1) */ "-2": "stream error",
    /* Z_STREAM_ERROR  (-2) */ "-3": "data error",
    /* Z_DATA_ERROR    (-3) */ "-4": "insufficient memory",
    /* Z_MEM_ERROR     (-4) */ "-5": "buffer error",
    /* Z_BUF_ERROR     (-5) */ "-6": "incompatible version" /* Z_VERSION_ERROR (-6) */ 
};
// (C) 1995-2013 Jean-loup Gailly and Mark Adler
// (C) 2014-2017 Vitaly Puzrin and Andrey Tupitsin
//
// This software is provided 'as-is', without any express or implied
// warranty. In no event will the authors be held liable for any damages
// arising from the use of this software.
//
// Permission is granted to anyone to use this software for any purpose,
// including commercial applications, and to alter it and redistribute it
// freely, subject to the following restrictions:
//
// 1. The origin of this software must not be misrepresented; you must not
//   claim that you wrote the original software. If you use this software
//   in a product, an acknowledgment in the product documentation would be
//   appreciated but is not required.
// 2. Altered source versions must be plainly marked as such, and must not be
//   misrepresented as being the original software.
// 3. This notice may not be removed or altered from any source distribution.
var $1d314fc79f930156$var$constants$2 = {
    /* Allowed flush values; see deflate() and inflate() below for details */ Z_NO_FLUSH: 0,
    Z_PARTIAL_FLUSH: 1,
    Z_SYNC_FLUSH: 2,
    Z_FULL_FLUSH: 3,
    Z_FINISH: 4,
    Z_BLOCK: 5,
    Z_TREES: 6,
    /* Return codes for the compression/decompression functions. Negative values
  * are errors, positive values are used for special but normal events.
  */ Z_OK: 0,
    Z_STREAM_END: 1,
    Z_NEED_DICT: 2,
    Z_ERRNO: -1,
    Z_STREAM_ERROR: -2,
    Z_DATA_ERROR: -3,
    Z_MEM_ERROR: -4,
    Z_BUF_ERROR: -5,
    //Z_VERSION_ERROR: -6,
    /* compression levels */ Z_NO_COMPRESSION: 0,
    Z_BEST_SPEED: 1,
    Z_BEST_COMPRESSION: 9,
    Z_DEFAULT_COMPRESSION: -1,
    Z_FILTERED: 1,
    Z_HUFFMAN_ONLY: 2,
    Z_RLE: 3,
    Z_FIXED: 4,
    Z_DEFAULT_STRATEGY: 0,
    /* Possible values of the data_type field (though see inflate()) */ Z_BINARY: 0,
    Z_TEXT: 1,
    //Z_ASCII:                1, // = Z_TEXT (deprecated)
    Z_UNKNOWN: 2,
    /* The deflate compression method */ Z_DEFLATED: 8
};
// (C) 1995-2013 Jean-loup Gailly and Mark Adler
// (C) 2014-2017 Vitaly Puzrin and Andrey Tupitsin
//
// This software is provided 'as-is', without any express or implied
// warranty. In no event will the authors be held liable for any damages
// arising from the use of this software.
//
// Permission is granted to anyone to use this software for any purpose,
// including commercial applications, and to alter it and redistribute it
// freely, subject to the following restrictions:
//
// 1. The origin of this software must not be misrepresented; you must not
//   claim that you wrote the original software. If you use this software
//   in a product, an acknowledgment in the product documentation would be
//   appreciated but is not required.
// 2. Altered source versions must be plainly marked as such, and must not be
//   misrepresented as being the original software.
// 3. This notice may not be removed or altered from any source distribution.
const { _tr_init: $1d314fc79f930156$var$_tr_init, _tr_stored_block: $1d314fc79f930156$var$_tr_stored_block, _tr_flush_block: $1d314fc79f930156$var$_tr_flush_block, _tr_tally: $1d314fc79f930156$var$_tr_tally, _tr_align: $1d314fc79f930156$var$_tr_align } = $1d314fc79f930156$var$trees;
/* Public constants ==========================================================*/ /* ===========================================================================*/ const { Z_NO_FLUSH: $1d314fc79f930156$var$Z_NO_FLUSH$2, Z_PARTIAL_FLUSH: $1d314fc79f930156$var$Z_PARTIAL_FLUSH, Z_FULL_FLUSH: $1d314fc79f930156$var$Z_FULL_FLUSH$1, Z_FINISH: $1d314fc79f930156$var$Z_FINISH$3, Z_BLOCK: $1d314fc79f930156$var$Z_BLOCK$1, Z_OK: $1d314fc79f930156$var$Z_OK$3, Z_STREAM_END: $1d314fc79f930156$var$Z_STREAM_END$3, Z_STREAM_ERROR: $1d314fc79f930156$var$Z_STREAM_ERROR$2, Z_DATA_ERROR: $1d314fc79f930156$var$Z_DATA_ERROR$2, Z_BUF_ERROR: $1d314fc79f930156$var$Z_BUF_ERROR$1, Z_DEFAULT_COMPRESSION: $1d314fc79f930156$var$Z_DEFAULT_COMPRESSION$1, Z_FILTERED: $1d314fc79f930156$var$Z_FILTERED, Z_HUFFMAN_ONLY: $1d314fc79f930156$var$Z_HUFFMAN_ONLY, Z_RLE: $1d314fc79f930156$var$Z_RLE, Z_FIXED: $1d314fc79f930156$var$Z_FIXED, Z_DEFAULT_STRATEGY: $1d314fc79f930156$var$Z_DEFAULT_STRATEGY$1, Z_UNKNOWN: $1d314fc79f930156$var$Z_UNKNOWN, Z_DEFLATED: $1d314fc79f930156$var$Z_DEFLATED$2 } = $1d314fc79f930156$var$constants$2;
/*============================================================================*/ const $1d314fc79f930156$var$MAX_MEM_LEVEL = 9;
/* Maximum value for memLevel in deflateInit2 */ const $1d314fc79f930156$var$MAX_WBITS$1 = 15;
/* 32K LZ77 window */ const $1d314fc79f930156$var$DEF_MEM_LEVEL = 8;
const $1d314fc79f930156$var$LENGTH_CODES = 29;
/* number of length codes, not counting the special END_BLOCK code */ const $1d314fc79f930156$var$LITERALS = 256;
/* number of literal bytes 0..255 */ const $1d314fc79f930156$var$L_CODES = $1d314fc79f930156$var$LITERALS + 1 + $1d314fc79f930156$var$LENGTH_CODES;
/* number of Literal or Length codes, including the END_BLOCK code */ const $1d314fc79f930156$var$D_CODES = 30;
/* number of distance codes */ const $1d314fc79f930156$var$BL_CODES = 19;
/* number of codes used to transfer the bit lengths */ const $1d314fc79f930156$var$HEAP_SIZE = 2 * $1d314fc79f930156$var$L_CODES + 1;
/* maximum heap size */ const $1d314fc79f930156$var$MAX_BITS = 15;
/* All codes must not exceed MAX_BITS bits */ const $1d314fc79f930156$var$MIN_MATCH = 3;
const $1d314fc79f930156$var$MAX_MATCH = 258;
const $1d314fc79f930156$var$MIN_LOOKAHEAD = $1d314fc79f930156$var$MAX_MATCH + $1d314fc79f930156$var$MIN_MATCH + 1;
const $1d314fc79f930156$var$PRESET_DICT = 0x20;
const $1d314fc79f930156$var$INIT_STATE = 42; /* zlib header -> BUSY_STATE */ 
//#ifdef GZIP
const $1d314fc79f930156$var$GZIP_STATE = 57; /* gzip header -> BUSY_STATE | EXTRA_STATE */ 
//#endif
const $1d314fc79f930156$var$EXTRA_STATE = 69; /* gzip extra block -> NAME_STATE */ 
const $1d314fc79f930156$var$NAME_STATE = 73; /* gzip file name -> COMMENT_STATE */ 
const $1d314fc79f930156$var$COMMENT_STATE = 91; /* gzip comment -> HCRC_STATE */ 
const $1d314fc79f930156$var$HCRC_STATE = 103; /* gzip header CRC -> BUSY_STATE */ 
const $1d314fc79f930156$var$BUSY_STATE = 113; /* deflate -> FINISH_STATE */ 
const $1d314fc79f930156$var$FINISH_STATE = 666; /* stream complete */ 
const $1d314fc79f930156$var$BS_NEED_MORE = 1; /* block not completed, need more input or more output */ 
const $1d314fc79f930156$var$BS_BLOCK_DONE = 2; /* block flush performed */ 
const $1d314fc79f930156$var$BS_FINISH_STARTED = 3; /* finish started, need only more output at next deflate */ 
const $1d314fc79f930156$var$BS_FINISH_DONE = 4; /* finish done, accept no more input or output */ 
const $1d314fc79f930156$var$OS_CODE = 0x03; // Unix :) . Don't detect, use this default.
const $1d314fc79f930156$var$err = (strm, errorCode)=>{
    strm.msg = $1d314fc79f930156$var$messages[errorCode];
    return errorCode;
};
const $1d314fc79f930156$var$rank = (f)=>{
    return f * 2 - (f > 4 ? 9 : 0);
};
const $1d314fc79f930156$var$zero = (buf)=>{
    let len = buf.length;
    while(--len >= 0)buf[len] = 0;
};
/* ===========================================================================
 * Slide the hash table when sliding the window down (could be avoided with 32
 * bit values at the expense of memory usage). We slide even when level == 0 to
 * keep the hash table consistent if we switch back to level > 0 later.
 */ const $1d314fc79f930156$var$slide_hash = (s)=>{
    let n, m;
    let p;
    let wsize = s.w_size;
    n = s.hash_size;
    p = n;
    do {
        m = s.head[--p];
        s.head[p] = m >= wsize ? m - wsize : 0;
    }while (--n);
    n = wsize;
    //#ifndef FASTEST
    p = n;
    do {
        m = s.prev[--p];
        s.prev[p] = m >= wsize ? m - wsize : 0;
    /* If n is not on any hash chain, prev[n] is garbage but
     * its value will never be used.
     */ }while (--n);
//#endif
};
/* eslint-disable new-cap */ let $1d314fc79f930156$var$HASH_ZLIB = (s, prev, data)=>(prev << s.hash_shift ^ data) & s.hash_mask;
// This hash causes less collisions, https://github.com/nodeca/pako/issues/135
// But breaks binary compatibility
//let HASH_FAST = (s, prev, data) => ((prev << 8) + (prev >> 8) + (data << 4)) & s.hash_mask;
let $1d314fc79f930156$var$HASH = $1d314fc79f930156$var$HASH_ZLIB;
/* =========================================================================
 * Flush as much pending output as possible. All deflate() output, except for
 * some deflate_stored() output, goes through this function so some
 * applications may wish to modify it to avoid allocating a large
 * strm->next_out buffer and copying into it. (See also read_buf()).
 */ const $1d314fc79f930156$var$flush_pending = (strm)=>{
    const s = strm.state;
    //_tr_flush_bits(s);
    let len = s.pending;
    if (len > strm.avail_out) len = strm.avail_out;
    if (len === 0) return;
    strm.output.set(s.pending_buf.subarray(s.pending_out, s.pending_out + len), strm.next_out);
    strm.next_out += len;
    s.pending_out += len;
    strm.total_out += len;
    strm.avail_out -= len;
    s.pending -= len;
    if (s.pending === 0) s.pending_out = 0;
};
const $1d314fc79f930156$var$flush_block_only = (s, last)=>{
    $1d314fc79f930156$var$_tr_flush_block(s, s.block_start >= 0 ? s.block_start : -1, s.strstart - s.block_start, last);
    s.block_start = s.strstart;
    $1d314fc79f930156$var$flush_pending(s.strm);
};
const $1d314fc79f930156$var$put_byte = (s, b)=>{
    s.pending_buf[s.pending++] = b;
};
/* =========================================================================
 * Put a short in the pending buffer. The 16-bit value is put in MSB order.
 * IN assertion: the stream state is correct and there is enough room in
 * pending_buf.
 */ const $1d314fc79f930156$var$putShortMSB = (s, b)=>{
    //  put_byte(s, (Byte)(b >> 8));
    //  put_byte(s, (Byte)(b & 0xff));
    s.pending_buf[s.pending++] = b >>> 8 & 0xff;
    s.pending_buf[s.pending++] = b & 0xff;
};
/* ===========================================================================
 * Read a new buffer from the current input stream, update the adler32
 * and total number of bytes read.  All deflate() input goes through
 * this function so some applications may wish to modify it to avoid
 * allocating a large strm->input buffer and copying from it.
 * (See also flush_pending()).
 */ const $1d314fc79f930156$var$read_buf = (strm, buf, start, size)=>{
    let len = strm.avail_in;
    if (len > size) len = size;
    if (len === 0) return 0;
    strm.avail_in -= len;
    // zmemcpy(buf, strm->next_in, len);
    buf.set(strm.input.subarray(strm.next_in, strm.next_in + len), start);
    if (strm.state.wrap === 1) strm.adler = $1d314fc79f930156$var$adler32_1(strm.adler, buf, len, start);
    else if (strm.state.wrap === 2) strm.adler = $1d314fc79f930156$var$crc32_1(strm.adler, buf, len, start);
    strm.next_in += len;
    strm.total_in += len;
    return len;
};
/* ===========================================================================
 * Set match_start to the longest match starting at the given string and
 * return its length. Matches shorter or equal to prev_length are discarded,
 * in which case the result is equal to prev_length and match_start is
 * garbage.
 * IN assertions: cur_match is the head of the hash chain for the current
 *   string (strstart) and its distance is <= MAX_DIST, and prev_length >= 1
 * OUT assertion: the match length is not greater than s->lookahead.
 */ const $1d314fc79f930156$var$longest_match = (s, cur_match)=>{
    let chain_length = s.max_chain_length; /* max hash chain length */ 
    let scan = s.strstart; /* current string */ 
    let match; /* matched string */ 
    let len; /* length of current match */ 
    let best_len = s.prev_length; /* best match length so far */ 
    let nice_match = s.nice_match; /* stop if match long enough */ 
    const limit = s.strstart > s.w_size - $1d314fc79f930156$var$MIN_LOOKAHEAD ? s.strstart - (s.w_size - $1d314fc79f930156$var$MIN_LOOKAHEAD) : 0 /*NIL*/ ;
    const _win = s.window; // shortcut
    const wmask = s.w_mask;
    const prev = s.prev;
    /* Stop when cur_match becomes <= limit. To simplify the code,
   * we prevent matches with the string of window index 0.
   */ const strend = s.strstart + $1d314fc79f930156$var$MAX_MATCH;
    let scan_end1 = _win[scan + best_len - 1];
    let scan_end = _win[scan + best_len];
    /* The code is optimized for HASH_BITS >= 8 and MAX_MATCH-2 multiple of 16.
   * It is easy to get rid of this optimization if necessary.
   */ // Assert(s->hash_bits >= 8 && MAX_MATCH == 258, "Code too clever");
    /* Do not waste too much time if we already have a good match: */ if (s.prev_length >= s.good_match) chain_length >>= 2;
    /* Do not look for matches beyond the end of the input. This is necessary
   * to make deflate deterministic.
   */ if (nice_match > s.lookahead) nice_match = s.lookahead;
    // Assert((ulg)s->strstart <= s->window_size-MIN_LOOKAHEAD, "need lookahead");
    do {
        // Assert(cur_match < s->strstart, "no future");
        match = cur_match;
        /* Skip to next match if the match length cannot increase
     * or if the match length is less than 2.  Note that the checks below
     * for insufficient lookahead only occur occasionally for performance
     * reasons.  Therefore uninitialized memory will be accessed, and
     * conditional jumps will be made that depend on those values.
     * However the length of the match is limited to the lookahead, so
     * the output of deflate is not affected by the uninitialized values.
     */ if (_win[match + best_len] !== scan_end || _win[match + best_len - 1] !== scan_end1 || _win[match] !== _win[scan] || _win[++match] !== _win[scan + 1]) continue;
        /* The check at best_len-1 can be removed because it will be made
     * again later. (This heuristic is not always a win.)
     * It is not necessary to compare scan[2] and match[2] since they
     * are always equal when the other bytes match, given that
     * the hash keys are equal and that HASH_BITS >= 8.
     */ scan += 2;
        match++;
        // Assert(*scan == *match, "match[2]?");
        /* We check for insufficient lookahead only every 8th comparison;
     * the 256th check will be made at strstart+258.
     */ do ;
        while (_win[++scan] === _win[++match] && _win[++scan] === _win[++match] && _win[++scan] === _win[++match] && _win[++scan] === _win[++match] && _win[++scan] === _win[++match] && _win[++scan] === _win[++match] && _win[++scan] === _win[++match] && _win[++scan] === _win[++match] && scan < strend);
        // Assert(scan <= s->window+(unsigned)(s->window_size-1), "wild scan");
        len = $1d314fc79f930156$var$MAX_MATCH - (strend - scan);
        scan = strend - $1d314fc79f930156$var$MAX_MATCH;
        if (len > best_len) {
            s.match_start = cur_match;
            best_len = len;
            if (len >= nice_match) break;
            scan_end1 = _win[scan + best_len - 1];
            scan_end = _win[scan + best_len];
        }
    }while ((cur_match = prev[cur_match & wmask]) > limit && --chain_length !== 0);
    if (best_len <= s.lookahead) return best_len;
    return s.lookahead;
};
/* ===========================================================================
 * Fill the window when the lookahead becomes insufficient.
 * Updates strstart and lookahead.
 *
 * IN assertion: lookahead < MIN_LOOKAHEAD
 * OUT assertions: strstart <= window_size-MIN_LOOKAHEAD
 *    At least one byte has been read, or avail_in == 0; reads are
 *    performed for at least two bytes (required for the zip translate_eol
 *    option -- not supported here).
 */ const $1d314fc79f930156$var$fill_window = (s)=>{
    const _w_size = s.w_size;
    let n, more, str;
    //Assert(s->lookahead < MIN_LOOKAHEAD, "already enough lookahead");
    do {
        more = s.window_size - s.lookahead - s.strstart;
        // JS ints have 32 bit, block below not needed
        /* Deal with !@#$% 64K limit: */ //if (sizeof(int) <= 2) {
        //    if (more == 0 && s->strstart == 0 && s->lookahead == 0) {
        //        more = wsize;
        //
        //  } else if (more == (unsigned)(-1)) {
        //        /* Very unlikely, but possible on 16 bit machine if
        //         * strstart == 0 && lookahead == 1 (input done a byte at time)
        //         */
        //        more--;
        //    }
        //}
        /* If the window is almost full and there is insufficient lookahead,
     * move the upper half to the lower one to make room in the upper half.
     */ if (s.strstart >= _w_size + (_w_size - $1d314fc79f930156$var$MIN_LOOKAHEAD)) {
            s.window.set(s.window.subarray(_w_size, _w_size + _w_size - more), 0);
            s.match_start -= _w_size;
            s.strstart -= _w_size;
            /* we now have strstart >= MAX_DIST */ s.block_start -= _w_size;
            if (s.insert > s.strstart) s.insert = s.strstart;
            $1d314fc79f930156$var$slide_hash(s);
            more += _w_size;
        }
        if (s.strm.avail_in === 0) break;
        /* If there was no sliding:
     *    strstart <= WSIZE+MAX_DIST-1 && lookahead <= MIN_LOOKAHEAD - 1 &&
     *    more == window_size - lookahead - strstart
     * => more >= window_size - (MIN_LOOKAHEAD-1 + WSIZE + MAX_DIST-1)
     * => more >= window_size - 2*WSIZE + 2
     * In the BIG_MEM or MMAP case (not yet supported),
     *   window_size == input_size + MIN_LOOKAHEAD  &&
     *   strstart + s->lookahead <= input_size => more >= MIN_LOOKAHEAD.
     * Otherwise, window_size == 2*WSIZE so more >= 2.
     * If there was sliding, more >= WSIZE. So in all cases, more >= 2.
     */ //Assert(more >= 2, "more < 2");
        n = $1d314fc79f930156$var$read_buf(s.strm, s.window, s.strstart + s.lookahead, more);
        s.lookahead += n;
        /* Initialize the hash value now that we have some input: */ if (s.lookahead + s.insert >= $1d314fc79f930156$var$MIN_MATCH) {
            str = s.strstart - s.insert;
            s.ins_h = s.window[str];
            /* UPDATE_HASH(s, s->ins_h, s->window[str + 1]); */ s.ins_h = $1d314fc79f930156$var$HASH(s, s.ins_h, s.window[str + 1]);
            //#if MIN_MATCH != 3
            //        Call update_hash() MIN_MATCH-3 more times
            //#endif
            while(s.insert){
                /* UPDATE_HASH(s, s->ins_h, s->window[str + MIN_MATCH-1]); */ s.ins_h = $1d314fc79f930156$var$HASH(s, s.ins_h, s.window[str + $1d314fc79f930156$var$MIN_MATCH - 1]);
                s.prev[str & s.w_mask] = s.head[s.ins_h];
                s.head[s.ins_h] = str;
                str++;
                s.insert--;
                if (s.lookahead + s.insert < $1d314fc79f930156$var$MIN_MATCH) break;
            }
        }
    /* If the whole input has less than MIN_MATCH bytes, ins_h is garbage,
     * but this is not important since only literal bytes will be emitted.
     */ }while (s.lookahead < $1d314fc79f930156$var$MIN_LOOKAHEAD && s.strm.avail_in !== 0);
/* If the WIN_INIT bytes after the end of the current data have never been
   * written, then zero those bytes in order to avoid memory check reports of
   * the use of uninitialized (or uninitialised as Julian writes) bytes by
   * the longest match routines.  Update the high water mark for the next
   * time through here.  WIN_INIT is set to MAX_MATCH since the longest match
   * routines allow scanning to strstart + MAX_MATCH, ignoring lookahead.
   */ //  if (s.high_water < s.window_size) {
//    const curr = s.strstart + s.lookahead;
//    let init = 0;
//
//    if (s.high_water < curr) {
//      /* Previous high water mark below current data -- zero WIN_INIT
//       * bytes or up to end of window, whichever is less.
//       */
//      init = s.window_size - curr;
//      if (init > WIN_INIT)
//        init = WIN_INIT;
//      zmemzero(s->window + curr, (unsigned)init);
//      s->high_water = curr + init;
//    }
//    else if (s->high_water < (ulg)curr + WIN_INIT) {
//      /* High water mark at or above current data, but below current data
//       * plus WIN_INIT -- zero out to current data plus WIN_INIT, or up
//       * to end of window, whichever is less.
//       */
//      init = (ulg)curr + WIN_INIT - s->high_water;
//      if (init > s->window_size - s->high_water)
//        init = s->window_size - s->high_water;
//      zmemzero(s->window + s->high_water, (unsigned)init);
//      s->high_water += init;
//    }
//  }
//
//  Assert((ulg)s->strstart <= s->window_size - MIN_LOOKAHEAD,
//    "not enough room for search");
};
/* ===========================================================================
 * Copy without compression as much as possible from the input stream, return
 * the current block state.
 *
 * In case deflateParams() is used to later switch to a non-zero compression
 * level, s->matches (otherwise unused when storing) keeps track of the number
 * of hash table slides to perform. If s->matches is 1, then one hash table
 * slide will be done when switching. If s->matches is 2, the maximum value
 * allowed here, then the hash table will be cleared, since two or more slides
 * is the same as a clear.
 *
 * deflate_stored() is written to minimize the number of times an input byte is
 * copied. It is most efficient with large input and output buffers, which
 * maximizes the opportunites to have a single copy from next_in to next_out.
 */ const $1d314fc79f930156$var$deflate_stored = (s, flush)=>{
    /* Smallest worthy block size when not flushing or finishing. By default
   * this is 32K. This can be as small as 507 bytes for memLevel == 1. For
   * large input and output buffers, the stored block size will be larger.
   */ let min_block = s.pending_buf_size - 5 > s.w_size ? s.w_size : s.pending_buf_size - 5;
    /* Copy as many min_block or larger stored blocks directly to next_out as
   * possible. If flushing, copy the remaining available input to next_out as
   * stored blocks, if there is enough space.
   */ let len, left, have, last = 0;
    let used = s.strm.avail_in;
    do {
        /* Set len to the maximum size block that we can copy directly with the
     * available input data and output space. Set left to how much of that
     * would be copied from what's left in the window.
     */ len = 65535 /* MAX_STORED */ ; /* maximum deflate stored block length */ 
        have = s.bi_valid + 42 >> 3; /* number of header bytes */ 
        if (s.strm.avail_out < have) break;
        /* maximum stored block length that will fit in avail_out: */ have = s.strm.avail_out - have;
        left = s.strstart - s.block_start; /* bytes left in window */ 
        if (len > left + s.strm.avail_in) len = left + s.strm.avail_in; /* limit len to the input */ 
        if (len > have) len = have; /* limit len to the output */ 
        /* If the stored block would be less than min_block in length, or if
     * unable to copy all of the available input when flushing, then try
     * copying to the window and the pending buffer instead. Also don't
     * write an empty block when flushing -- deflate() does that.
     */ if (len < min_block && (len === 0 && flush !== $1d314fc79f930156$var$Z_FINISH$3 || flush === $1d314fc79f930156$var$Z_NO_FLUSH$2 || len !== left + s.strm.avail_in)) break;
        /* Make a dummy stored block in pending to get the header bytes,
     * including any pending bits. This also updates the debugging counts.
     */ last = flush === $1d314fc79f930156$var$Z_FINISH$3 && len === left + s.strm.avail_in ? 1 : 0;
        $1d314fc79f930156$var$_tr_stored_block(s, 0, 0, last);
        /* Replace the lengths in the dummy stored block with len. */ s.pending_buf[s.pending - 4] = len;
        s.pending_buf[s.pending - 3] = len >> 8;
        s.pending_buf[s.pending - 2] = ~len;
        s.pending_buf[s.pending - 1] = ~len >> 8;
        /* Write the stored block header bytes. */ $1d314fc79f930156$var$flush_pending(s.strm);
        //#ifdef ZLIB_DEBUG
        //    /* Update debugging counts for the data about to be copied. */
        //    s->compressed_len += len << 3;
        //    s->bits_sent += len << 3;
        //#endif
        /* Copy uncompressed bytes from the window to next_out. */ if (left) {
            if (left > len) left = len;
            //zmemcpy(s->strm->next_out, s->window + s->block_start, left);
            s.strm.output.set(s.window.subarray(s.block_start, s.block_start + left), s.strm.next_out);
            s.strm.next_out += left;
            s.strm.avail_out -= left;
            s.strm.total_out += left;
            s.block_start += left;
            len -= left;
        }
        /* Copy uncompressed bytes directly from next_in to next_out, updating
     * the check value.
     */ if (len) {
            $1d314fc79f930156$var$read_buf(s.strm, s.strm.output, s.strm.next_out, len);
            s.strm.next_out += len;
            s.strm.avail_out -= len;
            s.strm.total_out += len;
        }
    }while (last === 0);
    /* Update the sliding window with the last s->w_size bytes of the copied
   * data, or append all of the copied data to the existing window if less
   * than s->w_size bytes were copied. Also update the number of bytes to
   * insert in the hash tables, in the event that deflateParams() switches to
   * a non-zero compression level.
   */ used -= s.strm.avail_in; /* number of input bytes directly copied */ 
    if (used) {
        /* If any input was used, then no unused input remains in the window,
     * therefore s->block_start == s->strstart.
     */ if (used >= s.w_size) {
            s.matches = 2; /* clear hash */ 
            //zmemcpy(s->window, s->strm->next_in - s->w_size, s->w_size);
            s.window.set(s.strm.input.subarray(s.strm.next_in - s.w_size, s.strm.next_in), 0);
            s.strstart = s.w_size;
            s.insert = s.strstart;
        } else {
            if (s.window_size - s.strstart <= used) {
                /* Slide the window down. */ s.strstart -= s.w_size;
                //zmemcpy(s->window, s->window + s->w_size, s->strstart);
                s.window.set(s.window.subarray(s.w_size, s.w_size + s.strstart), 0);
                if (s.matches < 2) s.matches++; /* add a pending slide_hash() */ 
                if (s.insert > s.strstart) s.insert = s.strstart;
            }
            //zmemcpy(s->window + s->strstart, s->strm->next_in - used, used);
            s.window.set(s.strm.input.subarray(s.strm.next_in - used, s.strm.next_in), s.strstart);
            s.strstart += used;
            s.insert += used > s.w_size - s.insert ? s.w_size - s.insert : used;
        }
        s.block_start = s.strstart;
    }
    if (s.high_water < s.strstart) s.high_water = s.strstart;
    /* If the last block was written to next_out, then done. */ if (last) return $1d314fc79f930156$var$BS_FINISH_DONE;
    /* If flushing and all input has been consumed, then done. */ if (flush !== $1d314fc79f930156$var$Z_NO_FLUSH$2 && flush !== $1d314fc79f930156$var$Z_FINISH$3 && s.strm.avail_in === 0 && s.strstart === s.block_start) return $1d314fc79f930156$var$BS_BLOCK_DONE;
    /* Fill the window with any remaining input. */ have = s.window_size - s.strstart;
    if (s.strm.avail_in > have && s.block_start >= s.w_size) {
        /* Slide the window down. */ s.block_start -= s.w_size;
        s.strstart -= s.w_size;
        //zmemcpy(s->window, s->window + s->w_size, s->strstart);
        s.window.set(s.window.subarray(s.w_size, s.w_size + s.strstart), 0);
        if (s.matches < 2) s.matches++; /* add a pending slide_hash() */ 
        have += s.w_size; /* more space now */ 
        if (s.insert > s.strstart) s.insert = s.strstart;
    }
    if (have > s.strm.avail_in) have = s.strm.avail_in;
    if (have) {
        $1d314fc79f930156$var$read_buf(s.strm, s.window, s.strstart, have);
        s.strstart += have;
        s.insert += have > s.w_size - s.insert ? s.w_size - s.insert : have;
    }
    if (s.high_water < s.strstart) s.high_water = s.strstart;
    /* There was not enough avail_out to write a complete worthy or flushed
   * stored block to next_out. Write a stored block to pending instead, if we
   * have enough input for a worthy block, or if flushing and there is enough
   * room for the remaining input as a stored block in the pending buffer.
   */ have = s.bi_valid + 42 >> 3; /* number of header bytes */ 
    /* maximum stored block length that will fit in pending: */ have = s.pending_buf_size - have > 65535 /* MAX_STORED */  ? 65535 /* MAX_STORED */  : s.pending_buf_size - have;
    min_block = have > s.w_size ? s.w_size : have;
    left = s.strstart - s.block_start;
    if (left >= min_block || (left || flush === $1d314fc79f930156$var$Z_FINISH$3) && flush !== $1d314fc79f930156$var$Z_NO_FLUSH$2 && s.strm.avail_in === 0 && left <= have) {
        len = left > have ? have : left;
        last = flush === $1d314fc79f930156$var$Z_FINISH$3 && s.strm.avail_in === 0 && len === left ? 1 : 0;
        $1d314fc79f930156$var$_tr_stored_block(s, s.block_start, len, last);
        s.block_start += len;
        $1d314fc79f930156$var$flush_pending(s.strm);
    }
    /* We've done all we can with the available input and output. */ return last ? $1d314fc79f930156$var$BS_FINISH_STARTED : $1d314fc79f930156$var$BS_NEED_MORE;
};
/* ===========================================================================
 * Compress as much as possible from the input stream, return the current
 * block state.
 * This function does not perform lazy evaluation of matches and inserts
 * new strings in the dictionary only for unmatched strings or for short
 * matches. It is used only for the fast compression options.
 */ const $1d314fc79f930156$var$deflate_fast = (s, flush)=>{
    let hash_head; /* head of the hash chain */ 
    let bflush; /* set if current block must be flushed */ 
    for(;;){
        /* Make sure that we always have enough lookahead, except
     * at the end of the input file. We need MAX_MATCH bytes
     * for the next match, plus MIN_MATCH bytes to insert the
     * string following the next match.
     */ if (s.lookahead < $1d314fc79f930156$var$MIN_LOOKAHEAD) {
            $1d314fc79f930156$var$fill_window(s);
            if (s.lookahead < $1d314fc79f930156$var$MIN_LOOKAHEAD && flush === $1d314fc79f930156$var$Z_NO_FLUSH$2) return $1d314fc79f930156$var$BS_NEED_MORE;
            if (s.lookahead === 0) break; /* flush the current block */ 
        }
        /* Insert the string window[strstart .. strstart+2] in the
     * dictionary, and set hash_head to the head of the hash chain:
     */ hash_head = 0 /*NIL*/ ;
        if (s.lookahead >= $1d314fc79f930156$var$MIN_MATCH) {
            /*** INSERT_STRING(s, s.strstart, hash_head); ***/ s.ins_h = $1d314fc79f930156$var$HASH(s, s.ins_h, s.window[s.strstart + $1d314fc79f930156$var$MIN_MATCH - 1]);
            hash_head = s.prev[s.strstart & s.w_mask] = s.head[s.ins_h];
            s.head[s.ins_h] = s.strstart;
        /***/ }
        /* Find the longest match, discarding those <= prev_length.
     * At this point we have always match_length < MIN_MATCH
     */ if (hash_head !== 0 /*NIL*/  && s.strstart - hash_head <= s.w_size - $1d314fc79f930156$var$MIN_LOOKAHEAD) /* To simplify the code, we prevent matches with the string
       * of window index 0 (in particular we have to avoid a match
       * of the string with itself at the start of the input file).
       */ s.match_length = $1d314fc79f930156$var$longest_match(s, hash_head);
        if (s.match_length >= $1d314fc79f930156$var$MIN_MATCH) {
            // check_match(s, s.strstart, s.match_start, s.match_length); // for debug only
            /*** _tr_tally_dist(s, s.strstart - s.match_start,
                     s.match_length - MIN_MATCH, bflush); ***/ bflush = $1d314fc79f930156$var$_tr_tally(s, s.strstart - s.match_start, s.match_length - $1d314fc79f930156$var$MIN_MATCH);
            s.lookahead -= s.match_length;
            /* Insert new strings in the hash table only if the match length
       * is not too large. This saves time but degrades compression.
       */ if (s.match_length <= s.max_lazy_match /*max_insert_length*/  && s.lookahead >= $1d314fc79f930156$var$MIN_MATCH) {
                s.match_length--; /* string at strstart already in table */ 
                do {
                    s.strstart++;
                    /*** INSERT_STRING(s, s.strstart, hash_head); ***/ s.ins_h = $1d314fc79f930156$var$HASH(s, s.ins_h, s.window[s.strstart + $1d314fc79f930156$var$MIN_MATCH - 1]);
                    hash_head = s.prev[s.strstart & s.w_mask] = s.head[s.ins_h];
                    s.head[s.ins_h] = s.strstart;
                /***/ /* strstart never exceeds WSIZE-MAX_MATCH, so there are
           * always MIN_MATCH bytes ahead.
           */ }while (--s.match_length !== 0);
                s.strstart++;
            } else {
                s.strstart += s.match_length;
                s.match_length = 0;
                s.ins_h = s.window[s.strstart];
                /* UPDATE_HASH(s, s.ins_h, s.window[s.strstart+1]); */ s.ins_h = $1d314fc79f930156$var$HASH(s, s.ins_h, s.window[s.strstart + 1]);
            //#if MIN_MATCH != 3
            //                Call UPDATE_HASH() MIN_MATCH-3 more times
            //#endif
            /* If lookahead < MIN_MATCH, ins_h is garbage, but it does not
         * matter since it will be recomputed at next deflate call.
         */ }
        } else {
            /* No match, output a literal byte */ //Tracevv((stderr,"%c", s.window[s.strstart]));
            /*** _tr_tally_lit(s, s.window[s.strstart], bflush); ***/ bflush = $1d314fc79f930156$var$_tr_tally(s, 0, s.window[s.strstart]);
            s.lookahead--;
            s.strstart++;
        }
        if (bflush) {
            /*** FLUSH_BLOCK(s, 0); ***/ $1d314fc79f930156$var$flush_block_only(s, false);
            if (s.strm.avail_out === 0) return $1d314fc79f930156$var$BS_NEED_MORE;
        /***/ }
    }
    s.insert = s.strstart < $1d314fc79f930156$var$MIN_MATCH - 1 ? s.strstart : $1d314fc79f930156$var$MIN_MATCH - 1;
    if (flush === $1d314fc79f930156$var$Z_FINISH$3) {
        /*** FLUSH_BLOCK(s, 1); ***/ $1d314fc79f930156$var$flush_block_only(s, true);
        if (s.strm.avail_out === 0) return $1d314fc79f930156$var$BS_FINISH_STARTED;
        /***/ return $1d314fc79f930156$var$BS_FINISH_DONE;
    }
    if (s.sym_next) {
        /*** FLUSH_BLOCK(s, 0); ***/ $1d314fc79f930156$var$flush_block_only(s, false);
        if (s.strm.avail_out === 0) return $1d314fc79f930156$var$BS_NEED_MORE;
    /***/ }
    return $1d314fc79f930156$var$BS_BLOCK_DONE;
};
/* ===========================================================================
 * Same as above, but achieves better compression. We use a lazy
 * evaluation for matches: a match is finally adopted only if there is
 * no better match at the next window position.
 */ const $1d314fc79f930156$var$deflate_slow = (s, flush)=>{
    let hash_head; /* head of hash chain */ 
    let bflush; /* set if current block must be flushed */ 
    let max_insert;
    /* Process the input block. */ for(;;){
        /* Make sure that we always have enough lookahead, except
     * at the end of the input file. We need MAX_MATCH bytes
     * for the next match, plus MIN_MATCH bytes to insert the
     * string following the next match.
     */ if (s.lookahead < $1d314fc79f930156$var$MIN_LOOKAHEAD) {
            $1d314fc79f930156$var$fill_window(s);
            if (s.lookahead < $1d314fc79f930156$var$MIN_LOOKAHEAD && flush === $1d314fc79f930156$var$Z_NO_FLUSH$2) return $1d314fc79f930156$var$BS_NEED_MORE;
            if (s.lookahead === 0) break;
             /* flush the current block */ 
        }
        /* Insert the string window[strstart .. strstart+2] in the
     * dictionary, and set hash_head to the head of the hash chain:
     */ hash_head = 0 /*NIL*/ ;
        if (s.lookahead >= $1d314fc79f930156$var$MIN_MATCH) {
            /*** INSERT_STRING(s, s.strstart, hash_head); ***/ s.ins_h = $1d314fc79f930156$var$HASH(s, s.ins_h, s.window[s.strstart + $1d314fc79f930156$var$MIN_MATCH - 1]);
            hash_head = s.prev[s.strstart & s.w_mask] = s.head[s.ins_h];
            s.head[s.ins_h] = s.strstart;
        /***/ }
        /* Find the longest match, discarding those <= prev_length.
     */ s.prev_length = s.match_length;
        s.prev_match = s.match_start;
        s.match_length = $1d314fc79f930156$var$MIN_MATCH - 1;
        if (hash_head !== 0 /*NIL*/  && s.prev_length < s.max_lazy_match && s.strstart - hash_head <= s.w_size - $1d314fc79f930156$var$MIN_LOOKAHEAD) {
            /* To simplify the code, we prevent matches with the string
       * of window index 0 (in particular we have to avoid a match
       * of the string with itself at the start of the input file).
       */ s.match_length = $1d314fc79f930156$var$longest_match(s, hash_head);
            /* longest_match() sets match_start */ if (s.match_length <= 5 && (s.strategy === $1d314fc79f930156$var$Z_FILTERED || s.match_length === $1d314fc79f930156$var$MIN_MATCH && s.strstart - s.match_start > 4096 /*TOO_FAR*/ )) /* If prev_match is also MIN_MATCH, match_start is garbage
         * but we will ignore the current match anyway.
         */ s.match_length = $1d314fc79f930156$var$MIN_MATCH - 1;
        }
        /* If there was a match at the previous step and the current
     * match is not better, output the previous match:
     */ if (s.prev_length >= $1d314fc79f930156$var$MIN_MATCH && s.match_length <= s.prev_length) {
            max_insert = s.strstart + s.lookahead - $1d314fc79f930156$var$MIN_MATCH;
            /* Do not insert strings in hash table beyond this. */ //check_match(s, s.strstart-1, s.prev_match, s.prev_length);
            /***_tr_tally_dist(s, s.strstart - 1 - s.prev_match,
                     s.prev_length - MIN_MATCH, bflush);***/ bflush = $1d314fc79f930156$var$_tr_tally(s, s.strstart - 1 - s.prev_match, s.prev_length - $1d314fc79f930156$var$MIN_MATCH);
            /* Insert in hash table all strings up to the end of the match.
       * strstart-1 and strstart are already inserted. If there is not
       * enough lookahead, the last two strings are not inserted in
       * the hash table.
       */ s.lookahead -= s.prev_length - 1;
            s.prev_length -= 2;
            do if (++s.strstart <= max_insert) {
                /*** INSERT_STRING(s, s.strstart, hash_head); ***/ s.ins_h = $1d314fc79f930156$var$HASH(s, s.ins_h, s.window[s.strstart + $1d314fc79f930156$var$MIN_MATCH - 1]);
                hash_head = s.prev[s.strstart & s.w_mask] = s.head[s.ins_h];
                s.head[s.ins_h] = s.strstart;
            /***/ }
            while (--s.prev_length !== 0);
            s.match_available = 0;
            s.match_length = $1d314fc79f930156$var$MIN_MATCH - 1;
            s.strstart++;
            if (bflush) {
                /*** FLUSH_BLOCK(s, 0); ***/ $1d314fc79f930156$var$flush_block_only(s, false);
                if (s.strm.avail_out === 0) return $1d314fc79f930156$var$BS_NEED_MORE;
            /***/ }
        } else if (s.match_available) {
            /* If there was no match at the previous position, output a
       * single literal. If there was a match but the current match
       * is longer, truncate the previous match to a single literal.
       */ //Tracevv((stderr,"%c", s->window[s->strstart-1]));
            /*** _tr_tally_lit(s, s.window[s.strstart-1], bflush); ***/ bflush = $1d314fc79f930156$var$_tr_tally(s, 0, s.window[s.strstart - 1]);
            if (bflush) /*** FLUSH_BLOCK_ONLY(s, 0) ***/ $1d314fc79f930156$var$flush_block_only(s, false);
            s.strstart++;
            s.lookahead--;
            if (s.strm.avail_out === 0) return $1d314fc79f930156$var$BS_NEED_MORE;
        } else {
            /* There is no previous match to compare with, wait for
       * the next step to decide.
       */ s.match_available = 1;
            s.strstart++;
            s.lookahead--;
        }
    }
    //Assert (flush != Z_NO_FLUSH, "no flush?");
    if (s.match_available) {
        //Tracevv((stderr,"%c", s->window[s->strstart-1]));
        /*** _tr_tally_lit(s, s.window[s.strstart-1], bflush); ***/ bflush = $1d314fc79f930156$var$_tr_tally(s, 0, s.window[s.strstart - 1]);
        s.match_available = 0;
    }
    s.insert = s.strstart < $1d314fc79f930156$var$MIN_MATCH - 1 ? s.strstart : $1d314fc79f930156$var$MIN_MATCH - 1;
    if (flush === $1d314fc79f930156$var$Z_FINISH$3) {
        /*** FLUSH_BLOCK(s, 1); ***/ $1d314fc79f930156$var$flush_block_only(s, true);
        if (s.strm.avail_out === 0) return $1d314fc79f930156$var$BS_FINISH_STARTED;
        /***/ return $1d314fc79f930156$var$BS_FINISH_DONE;
    }
    if (s.sym_next) {
        /*** FLUSH_BLOCK(s, 0); ***/ $1d314fc79f930156$var$flush_block_only(s, false);
        if (s.strm.avail_out === 0) return $1d314fc79f930156$var$BS_NEED_MORE;
    /***/ }
    return $1d314fc79f930156$var$BS_BLOCK_DONE;
};
/* ===========================================================================
 * For Z_RLE, simply look for runs of bytes, generate matches only of distance
 * one.  Do not maintain a hash table.  (It will be regenerated if this run of
 * deflate switches away from Z_RLE.)
 */ const $1d314fc79f930156$var$deflate_rle = (s, flush)=>{
    let bflush; /* set if current block must be flushed */ 
    let prev; /* byte at distance one to match */ 
    let scan, strend; /* scan goes up to strend for length of run */ 
    const _win = s.window;
    for(;;){
        /* Make sure that we always have enough lookahead, except
     * at the end of the input file. We need MAX_MATCH bytes
     * for the longest run, plus one for the unrolled loop.
     */ if (s.lookahead <= $1d314fc79f930156$var$MAX_MATCH) {
            $1d314fc79f930156$var$fill_window(s);
            if (s.lookahead <= $1d314fc79f930156$var$MAX_MATCH && flush === $1d314fc79f930156$var$Z_NO_FLUSH$2) return $1d314fc79f930156$var$BS_NEED_MORE;
            if (s.lookahead === 0) break;
             /* flush the current block */ 
        }
        /* See how many times the previous byte repeats */ s.match_length = 0;
        if (s.lookahead >= $1d314fc79f930156$var$MIN_MATCH && s.strstart > 0) {
            scan = s.strstart - 1;
            prev = _win[scan];
            if (prev === _win[++scan] && prev === _win[++scan] && prev === _win[++scan]) {
                strend = s.strstart + $1d314fc79f930156$var$MAX_MATCH;
                do ;
                while (prev === _win[++scan] && prev === _win[++scan] && prev === _win[++scan] && prev === _win[++scan] && prev === _win[++scan] && prev === _win[++scan] && prev === _win[++scan] && prev === _win[++scan] && scan < strend);
                s.match_length = $1d314fc79f930156$var$MAX_MATCH - (strend - scan);
                if (s.match_length > s.lookahead) s.match_length = s.lookahead;
            }
        //Assert(scan <= s->window+(uInt)(s->window_size-1), "wild scan");
        }
        /* Emit match if have run of MIN_MATCH or longer, else emit literal */ if (s.match_length >= $1d314fc79f930156$var$MIN_MATCH) {
            //check_match(s, s.strstart, s.strstart - 1, s.match_length);
            /*** _tr_tally_dist(s, 1, s.match_length - MIN_MATCH, bflush); ***/ bflush = $1d314fc79f930156$var$_tr_tally(s, 1, s.match_length - $1d314fc79f930156$var$MIN_MATCH);
            s.lookahead -= s.match_length;
            s.strstart += s.match_length;
            s.match_length = 0;
        } else {
            /* No match, output a literal byte */ //Tracevv((stderr,"%c", s->window[s->strstart]));
            /*** _tr_tally_lit(s, s.window[s.strstart], bflush); ***/ bflush = $1d314fc79f930156$var$_tr_tally(s, 0, s.window[s.strstart]);
            s.lookahead--;
            s.strstart++;
        }
        if (bflush) {
            /*** FLUSH_BLOCK(s, 0); ***/ $1d314fc79f930156$var$flush_block_only(s, false);
            if (s.strm.avail_out === 0) return $1d314fc79f930156$var$BS_NEED_MORE;
        /***/ }
    }
    s.insert = 0;
    if (flush === $1d314fc79f930156$var$Z_FINISH$3) {
        /*** FLUSH_BLOCK(s, 1); ***/ $1d314fc79f930156$var$flush_block_only(s, true);
        if (s.strm.avail_out === 0) return $1d314fc79f930156$var$BS_FINISH_STARTED;
        /***/ return $1d314fc79f930156$var$BS_FINISH_DONE;
    }
    if (s.sym_next) {
        /*** FLUSH_BLOCK(s, 0); ***/ $1d314fc79f930156$var$flush_block_only(s, false);
        if (s.strm.avail_out === 0) return $1d314fc79f930156$var$BS_NEED_MORE;
    /***/ }
    return $1d314fc79f930156$var$BS_BLOCK_DONE;
};
/* ===========================================================================
 * For Z_HUFFMAN_ONLY, do not look for matches.  Do not maintain a hash table.
 * (It will be regenerated if this run of deflate switches away from Huffman.)
 */ const $1d314fc79f930156$var$deflate_huff = (s, flush)=>{
    let bflush; /* set if current block must be flushed */ 
    for(;;){
        /* Make sure that we have a literal to write. */ if (s.lookahead === 0) {
            $1d314fc79f930156$var$fill_window(s);
            if (s.lookahead === 0) {
                if (flush === $1d314fc79f930156$var$Z_NO_FLUSH$2) return $1d314fc79f930156$var$BS_NEED_MORE;
                break; /* flush the current block */ 
            }
        }
        /* Output a literal byte */ s.match_length = 0;
        //Tracevv((stderr,"%c", s->window[s->strstart]));
        /*** _tr_tally_lit(s, s.window[s.strstart], bflush); ***/ bflush = $1d314fc79f930156$var$_tr_tally(s, 0, s.window[s.strstart]);
        s.lookahead--;
        s.strstart++;
        if (bflush) {
            /*** FLUSH_BLOCK(s, 0); ***/ $1d314fc79f930156$var$flush_block_only(s, false);
            if (s.strm.avail_out === 0) return $1d314fc79f930156$var$BS_NEED_MORE;
        /***/ }
    }
    s.insert = 0;
    if (flush === $1d314fc79f930156$var$Z_FINISH$3) {
        /*** FLUSH_BLOCK(s, 1); ***/ $1d314fc79f930156$var$flush_block_only(s, true);
        if (s.strm.avail_out === 0) return $1d314fc79f930156$var$BS_FINISH_STARTED;
        /***/ return $1d314fc79f930156$var$BS_FINISH_DONE;
    }
    if (s.sym_next) {
        /*** FLUSH_BLOCK(s, 0); ***/ $1d314fc79f930156$var$flush_block_only(s, false);
        if (s.strm.avail_out === 0) return $1d314fc79f930156$var$BS_NEED_MORE;
    /***/ }
    return $1d314fc79f930156$var$BS_BLOCK_DONE;
};
/* Values for max_lazy_match, good_match and max_chain_length, depending on
 * the desired pack level (0..9). The values given below have been tuned to
 * exclude worst case performance for pathological files. Better values may be
 * found for specific files.
 */ function $1d314fc79f930156$var$Config(good_length, max_lazy, nice_length, max_chain, func) {
    this.good_length = good_length;
    this.max_lazy = max_lazy;
    this.nice_length = nice_length;
    this.max_chain = max_chain;
    this.func = func;
}
const $1d314fc79f930156$var$configuration_table = [
    /*      good lazy nice chain */ new $1d314fc79f930156$var$Config(0, 0, 0, 0, $1d314fc79f930156$var$deflate_stored),
    /* 0 store only */ new $1d314fc79f930156$var$Config(4, 4, 8, 4, $1d314fc79f930156$var$deflate_fast),
    /* 1 max speed, no lazy matches */ new $1d314fc79f930156$var$Config(4, 5, 16, 8, $1d314fc79f930156$var$deflate_fast),
    /* 2 */ new $1d314fc79f930156$var$Config(4, 6, 32, 32, $1d314fc79f930156$var$deflate_fast),
    /* 3 */ new $1d314fc79f930156$var$Config(4, 4, 16, 16, $1d314fc79f930156$var$deflate_slow),
    /* 4 lazy matches */ new $1d314fc79f930156$var$Config(8, 16, 32, 32, $1d314fc79f930156$var$deflate_slow),
    /* 5 */ new $1d314fc79f930156$var$Config(8, 16, 128, 128, $1d314fc79f930156$var$deflate_slow),
    /* 6 */ new $1d314fc79f930156$var$Config(8, 32, 128, 256, $1d314fc79f930156$var$deflate_slow),
    /* 7 */ new $1d314fc79f930156$var$Config(32, 128, 258, 1024, $1d314fc79f930156$var$deflate_slow),
    /* 8 */ new $1d314fc79f930156$var$Config(32, 258, 258, 4096, $1d314fc79f930156$var$deflate_slow)
];
/* ===========================================================================
 * Initialize the "longest match" routines for a new zlib stream
 */ const $1d314fc79f930156$var$lm_init = (s)=>{
    s.window_size = 2 * s.w_size;
    /*** CLEAR_HASH(s); ***/ $1d314fc79f930156$var$zero(s.head); // Fill with NIL (= 0);
    /* Set the default configuration parameters:
   */ s.max_lazy_match = $1d314fc79f930156$var$configuration_table[s.level].max_lazy;
    s.good_match = $1d314fc79f930156$var$configuration_table[s.level].good_length;
    s.nice_match = $1d314fc79f930156$var$configuration_table[s.level].nice_length;
    s.max_chain_length = $1d314fc79f930156$var$configuration_table[s.level].max_chain;
    s.strstart = 0;
    s.block_start = 0;
    s.lookahead = 0;
    s.insert = 0;
    s.match_length = s.prev_length = $1d314fc79f930156$var$MIN_MATCH - 1;
    s.match_available = 0;
    s.ins_h = 0;
};
function $1d314fc79f930156$var$DeflateState() {
    this.strm = null; /* pointer back to this zlib stream */ 
    this.status = 0; /* as the name implies */ 
    this.pending_buf = null; /* output still pending */ 
    this.pending_buf_size = 0; /* size of pending_buf */ 
    this.pending_out = 0; /* next pending byte to output to the stream */ 
    this.pending = 0; /* nb of bytes in the pending buffer */ 
    this.wrap = 0; /* bit 0 true for zlib, bit 1 true for gzip */ 
    this.gzhead = null; /* gzip header information to write */ 
    this.gzindex = 0; /* where in extra, name, or comment */ 
    this.method = $1d314fc79f930156$var$Z_DEFLATED$2; /* can only be DEFLATED */ 
    this.last_flush = -1; /* value of flush param for previous deflate call */ 
    this.w_size = 0; /* LZ77 window size (32K by default) */ 
    this.w_bits = 0; /* log2(w_size)  (8..16) */ 
    this.w_mask = 0; /* w_size - 1 */ 
    this.window = null;
    /* Sliding window. Input bytes are read into the second half of the window,
   * and move to the first half later to keep a dictionary of at least wSize
   * bytes. With this organization, matches are limited to a distance of
   * wSize-MAX_MATCH bytes, but this ensures that IO is always
   * performed with a length multiple of the block size.
   */ this.window_size = 0;
    /* Actual size of window: 2*wSize, except when the user input buffer
   * is directly used as sliding window.
   */ this.prev = null;
    /* Link to older string with same hash index. To limit the size of this
   * array to 64K, this link is maintained only for the last 32K strings.
   * An index in this array is thus a window index modulo 32K.
   */ this.head = null; /* Heads of the hash chains or NIL. */ 
    this.ins_h = 0; /* hash index of string to be inserted */ 
    this.hash_size = 0; /* number of elements in hash table */ 
    this.hash_bits = 0; /* log2(hash_size) */ 
    this.hash_mask = 0; /* hash_size-1 */ 
    this.hash_shift = 0;
    /* Number of bits by which ins_h must be shifted at each input
   * step. It must be such that after MIN_MATCH steps, the oldest
   * byte no longer takes part in the hash key, that is:
   *   hash_shift * MIN_MATCH >= hash_bits
   */ this.block_start = 0;
    /* Window position at the beginning of the current output block. Gets
   * negative when the window is moved backwards.
   */ this.match_length = 0; /* length of best match */ 
    this.prev_match = 0; /* previous match */ 
    this.match_available = 0; /* set if previous match exists */ 
    this.strstart = 0; /* start of string to insert */ 
    this.match_start = 0; /* start of matching string */ 
    this.lookahead = 0; /* number of valid bytes ahead in window */ 
    this.prev_length = 0;
    /* Length of the best match at previous step. Matches not greater than this
   * are discarded. This is used in the lazy match evaluation.
   */ this.max_chain_length = 0;
    /* To speed up deflation, hash chains are never searched beyond this
   * length.  A higher limit improves compression ratio but degrades the
   * speed.
   */ this.max_lazy_match = 0;
    /* Attempt to find a better match only when the current match is strictly
   * smaller than this value. This mechanism is used only for compression
   * levels >= 4.
   */ // That's alias to max_lazy_match, don't use directly
    //this.max_insert_length = 0;
    /* Insert new strings in the hash table only if the match length is not
   * greater than this length. This saves time but degrades compression.
   * max_insert_length is used only for compression levels <= 3.
   */ this.level = 0; /* compression level (1..9) */ 
    this.strategy = 0; /* favor or force Huffman coding*/ 
    this.good_match = 0;
    /* Use a faster search when the previous match is longer than this */ this.nice_match = 0; /* Stop searching when current match exceeds this */ 
    /* used by trees.c: */ /* Didn't use ct_data typedef below to suppress compiler warning */ // struct ct_data_s dyn_ltree[HEAP_SIZE];   /* literal and length tree */
    // struct ct_data_s dyn_dtree[2*D_CODES+1]; /* distance tree */
    // struct ct_data_s bl_tree[2*BL_CODES+1];  /* Huffman tree for bit lengths */
    // Use flat array of DOUBLE size, with interleaved fata,
    // because JS does not support effective
    this.dyn_ltree = new Uint16Array($1d314fc79f930156$var$HEAP_SIZE * 2);
    this.dyn_dtree = new Uint16Array((2 * $1d314fc79f930156$var$D_CODES + 1) * 2);
    this.bl_tree = new Uint16Array((2 * $1d314fc79f930156$var$BL_CODES + 1) * 2);
    $1d314fc79f930156$var$zero(this.dyn_ltree);
    $1d314fc79f930156$var$zero(this.dyn_dtree);
    $1d314fc79f930156$var$zero(this.bl_tree);
    this.l_desc = null; /* desc. for literal tree */ 
    this.d_desc = null; /* desc. for distance tree */ 
    this.bl_desc = null; /* desc. for bit length tree */ 
    //ush bl_count[MAX_BITS+1];
    this.bl_count = new Uint16Array($1d314fc79f930156$var$MAX_BITS + 1);
    /* number of codes at each bit length for an optimal tree */ //int heap[2*L_CODES+1];      /* heap used to build the Huffman trees */
    this.heap = new Uint16Array(2 * $1d314fc79f930156$var$L_CODES + 1); /* heap used to build the Huffman trees */ 
    $1d314fc79f930156$var$zero(this.heap);
    this.heap_len = 0; /* number of elements in the heap */ 
    this.heap_max = 0; /* element of largest frequency */ 
    /* The sons of heap[n] are heap[2*n] and heap[2*n+1]. heap[0] is not used.
   * The same heap array is used to build all trees.
   */ this.depth = new Uint16Array(2 * $1d314fc79f930156$var$L_CODES + 1); //uch depth[2*L_CODES+1];
    $1d314fc79f930156$var$zero(this.depth);
    /* Depth of each subtree used as tie breaker for trees of equal frequency
   */ this.sym_buf = 0; /* buffer for distances and literals/lengths */ 
    this.lit_bufsize = 0;
    /* Size of match buffer for literals/lengths.  There are 4 reasons for
   * limiting lit_bufsize to 64K:
   *   - frequencies can be kept in 16 bit counters
   *   - if compression is not successful for the first block, all input
   *     data is still in the window so we can still emit a stored block even
   *     when input comes from standard input.  (This can also be done for
   *     all blocks if lit_bufsize is not greater than 32K.)
   *   - if compression is not successful for a file smaller than 64K, we can
   *     even emit a stored file instead of a stored block (saving 5 bytes).
   *     This is applicable only for zip (not gzip or zlib).
   *   - creating new Huffman trees less frequently may not provide fast
   *     adaptation to changes in the input data statistics. (Take for
   *     example a binary file with poorly compressible code followed by
   *     a highly compressible string table.) Smaller buffer sizes give
   *     fast adaptation but have of course the overhead of transmitting
   *     trees more frequently.
   *   - I can't count above 4
   */ this.sym_next = 0; /* running index in sym_buf */ 
    this.sym_end = 0; /* symbol table full when sym_next reaches this */ 
    this.opt_len = 0; /* bit length of current block with optimal trees */ 
    this.static_len = 0; /* bit length of current block with static trees */ 
    this.matches = 0; /* number of string matches in current block */ 
    this.insert = 0; /* bytes at end of window left to insert */ 
    this.bi_buf = 0;
    /* Output buffer. bits are inserted starting at the bottom (least
   * significant bits).
   */ this.bi_valid = 0;
/* Number of valid bits in bi_buf.  All bits above the last valid bit
   * are always zero.
   */ // Used for window memory init. We safely ignore it for JS. That makes
// sense only for pointers and memory check tools.
//this.high_water = 0;
/* High water mark offset in window for initialized bytes -- bytes above
   * this are set to zero in order to avoid memory check warnings when
   * longest match routines access bytes past the input.  This is then
   * updated to the new high water mark.
   */ }
/* =========================================================================
 * Check for a valid deflate stream state. Return 0 if ok, 1 if not.
 */ const $1d314fc79f930156$var$deflateStateCheck = (strm)=>{
    if (!strm) return 1;
    const s = strm.state;
    if (!s || s.strm !== strm || s.status !== $1d314fc79f930156$var$INIT_STATE && //#ifdef GZIP
    s.status !== $1d314fc79f930156$var$GZIP_STATE && //#endif
    s.status !== $1d314fc79f930156$var$EXTRA_STATE && s.status !== $1d314fc79f930156$var$NAME_STATE && s.status !== $1d314fc79f930156$var$COMMENT_STATE && s.status !== $1d314fc79f930156$var$HCRC_STATE && s.status !== $1d314fc79f930156$var$BUSY_STATE && s.status !== $1d314fc79f930156$var$FINISH_STATE) return 1;
    return 0;
};
const $1d314fc79f930156$var$deflateResetKeep = (strm)=>{
    if ($1d314fc79f930156$var$deflateStateCheck(strm)) return $1d314fc79f930156$var$err(strm, $1d314fc79f930156$var$Z_STREAM_ERROR$2);
    strm.total_in = strm.total_out = 0;
    strm.data_type = $1d314fc79f930156$var$Z_UNKNOWN;
    const s = strm.state;
    s.pending = 0;
    s.pending_out = 0;
    if (s.wrap < 0) s.wrap = -s.wrap;
    s.status = //#ifdef GZIP
    s.wrap === 2 ? $1d314fc79f930156$var$GZIP_STATE : //#endif
    s.wrap ? $1d314fc79f930156$var$INIT_STATE : $1d314fc79f930156$var$BUSY_STATE;
    strm.adler = s.wrap === 2 ? 0 // crc32(0, Z_NULL, 0)
     : 1; // adler32(0, Z_NULL, 0)
    s.last_flush = -2;
    $1d314fc79f930156$var$_tr_init(s);
    return $1d314fc79f930156$var$Z_OK$3;
};
const $1d314fc79f930156$var$deflateReset = (strm)=>{
    const ret = $1d314fc79f930156$var$deflateResetKeep(strm);
    if (ret === $1d314fc79f930156$var$Z_OK$3) $1d314fc79f930156$var$lm_init(strm.state);
    return ret;
};
const $1d314fc79f930156$var$deflateSetHeader = (strm, head)=>{
    if ($1d314fc79f930156$var$deflateStateCheck(strm) || strm.state.wrap !== 2) return $1d314fc79f930156$var$Z_STREAM_ERROR$2;
    strm.state.gzhead = head;
    return $1d314fc79f930156$var$Z_OK$3;
};
const $1d314fc79f930156$var$deflateInit2 = (strm, level, method, windowBits, memLevel, strategy)=>{
    if (!strm) return $1d314fc79f930156$var$Z_STREAM_ERROR$2;
    let wrap = 1;
    if (level === $1d314fc79f930156$var$Z_DEFAULT_COMPRESSION$1) level = 6;
    if (windowBits < 0) {
        wrap = 0;
        windowBits = -windowBits;
    } else if (windowBits > 15) {
        wrap = 2; /* write gzip wrapper instead */ 
        windowBits -= 16;
    }
    if (memLevel < 1 || memLevel > $1d314fc79f930156$var$MAX_MEM_LEVEL || method !== $1d314fc79f930156$var$Z_DEFLATED$2 || windowBits < 8 || windowBits > 15 || level < 0 || level > 9 || strategy < 0 || strategy > $1d314fc79f930156$var$Z_FIXED || windowBits === 8 && wrap !== 1) return $1d314fc79f930156$var$err(strm, $1d314fc79f930156$var$Z_STREAM_ERROR$2);
    if (windowBits === 8) windowBits = 9;
    /* until 256-byte window bug fixed */ const s = new $1d314fc79f930156$var$DeflateState();
    strm.state = s;
    s.strm = strm;
    s.status = $1d314fc79f930156$var$INIT_STATE; /* to pass state test in deflateReset() */ 
    s.wrap = wrap;
    s.gzhead = null;
    s.w_bits = windowBits;
    s.w_size = 1 << s.w_bits;
    s.w_mask = s.w_size - 1;
    s.hash_bits = memLevel + 7;
    s.hash_size = 1 << s.hash_bits;
    s.hash_mask = s.hash_size - 1;
    s.hash_shift = ~~((s.hash_bits + $1d314fc79f930156$var$MIN_MATCH - 1) / $1d314fc79f930156$var$MIN_MATCH);
    s.window = new Uint8Array(s.w_size * 2);
    s.head = new Uint16Array(s.hash_size);
    s.prev = new Uint16Array(s.w_size);
    // Don't need mem init magic for JS.
    //s.high_water = 0;  /* nothing written to s->window yet */
    s.lit_bufsize = 1 << memLevel + 6; /* 16K elements by default */ 
    /* We overlay pending_buf and sym_buf. This works since the average size
   * for length/distance pairs over any compressed block is assured to be 31
   * bits or less.
   *
   * Analysis: The longest fixed codes are a length code of 8 bits plus 5
   * extra bits, for lengths 131 to 257. The longest fixed distance codes are
   * 5 bits plus 13 extra bits, for distances 16385 to 32768. The longest
   * possible fixed-codes length/distance pair is then 31 bits total.
   *
   * sym_buf starts one-fourth of the way into pending_buf. So there are
   * three bytes in sym_buf for every four bytes in pending_buf. Each symbol
   * in sym_buf is three bytes -- two for the distance and one for the
   * literal/length. As each symbol is consumed, the pointer to the next
   * sym_buf value to read moves forward three bytes. From that symbol, up to
   * 31 bits are written to pending_buf. The closest the written pending_buf
   * bits gets to the next sym_buf symbol to read is just before the last
   * code is written. At that time, 31*(n-2) bits have been written, just
   * after 24*(n-2) bits have been consumed from sym_buf. sym_buf starts at
   * 8*n bits into pending_buf. (Note that the symbol buffer fills when n-1
   * symbols are written.) The closest the writing gets to what is unread is
   * then n+14 bits. Here n is lit_bufsize, which is 16384 by default, and
   * can range from 128 to 32768.
   *
   * Therefore, at a minimum, there are 142 bits of space between what is
   * written and what is read in the overlain buffers, so the symbols cannot
   * be overwritten by the compressed data. That space is actually 139 bits,
   * due to the three-bit fixed-code block header.
   *
   * That covers the case where either Z_FIXED is specified, forcing fixed
   * codes, or when the use of fixed codes is chosen, because that choice
   * results in a smaller compressed block than dynamic codes. That latter
   * condition then assures that the above analysis also covers all dynamic
   * blocks. A dynamic-code block will only be chosen to be emitted if it has
   * fewer bits than a fixed-code block would for the same set of symbols.
   * Therefore its average symbol length is assured to be less than 31. So
   * the compressed data for a dynamic block also cannot overwrite the
   * symbols from which it is being constructed.
   */ s.pending_buf_size = s.lit_bufsize * 4;
    s.pending_buf = new Uint8Array(s.pending_buf_size);
    // It is offset from `s.pending_buf` (size is `s.lit_bufsize * 2`)
    //s->sym_buf = s->pending_buf + s->lit_bufsize;
    s.sym_buf = s.lit_bufsize;
    //s->sym_end = (s->lit_bufsize - 1) * 3;
    s.sym_end = (s.lit_bufsize - 1) * 3;
    /* We avoid equality with lit_bufsize*3 because of wraparound at 64K
   * on 16 bit machines and because stored blocks are restricted to
   * 64K-1 bytes.
   */ s.level = level;
    s.strategy = strategy;
    s.method = method;
    return $1d314fc79f930156$var$deflateReset(strm);
};
const $1d314fc79f930156$var$deflateInit = (strm, level)=>{
    return $1d314fc79f930156$var$deflateInit2(strm, level, $1d314fc79f930156$var$Z_DEFLATED$2, $1d314fc79f930156$var$MAX_WBITS$1, $1d314fc79f930156$var$DEF_MEM_LEVEL, $1d314fc79f930156$var$Z_DEFAULT_STRATEGY$1);
};
/* ========================================================================= */ const $1d314fc79f930156$var$deflate$2 = (strm, flush)=>{
    if ($1d314fc79f930156$var$deflateStateCheck(strm) || flush > $1d314fc79f930156$var$Z_BLOCK$1 || flush < 0) return strm ? $1d314fc79f930156$var$err(strm, $1d314fc79f930156$var$Z_STREAM_ERROR$2) : $1d314fc79f930156$var$Z_STREAM_ERROR$2;
    const s = strm.state;
    if (!strm.output || strm.avail_in !== 0 && !strm.input || s.status === $1d314fc79f930156$var$FINISH_STATE && flush !== $1d314fc79f930156$var$Z_FINISH$3) return $1d314fc79f930156$var$err(strm, strm.avail_out === 0 ? $1d314fc79f930156$var$Z_BUF_ERROR$1 : $1d314fc79f930156$var$Z_STREAM_ERROR$2);
    const old_flush = s.last_flush;
    s.last_flush = flush;
    /* Flush as much pending output as possible */ if (s.pending !== 0) {
        $1d314fc79f930156$var$flush_pending(strm);
        if (strm.avail_out === 0) {
            /* Since avail_out is 0, deflate will be called again with
       * more output space, but possibly with both pending and
       * avail_in equal to zero. There won't be anything to do,
       * but this is not an error situation so make sure we
       * return OK instead of BUF_ERROR at next call of deflate:
       */ s.last_flush = -1;
            return $1d314fc79f930156$var$Z_OK$3;
        }
    /* Make sure there is something to do and avoid duplicate consecutive
     * flushes. For repeated and useless calls with Z_FINISH, we keep
     * returning Z_STREAM_END instead of Z_BUF_ERROR.
     */ } else if (strm.avail_in === 0 && $1d314fc79f930156$var$rank(flush) <= $1d314fc79f930156$var$rank(old_flush) && flush !== $1d314fc79f930156$var$Z_FINISH$3) return $1d314fc79f930156$var$err(strm, $1d314fc79f930156$var$Z_BUF_ERROR$1);
    /* User must not provide more input after the first FINISH: */ if (s.status === $1d314fc79f930156$var$FINISH_STATE && strm.avail_in !== 0) return $1d314fc79f930156$var$err(strm, $1d314fc79f930156$var$Z_BUF_ERROR$1);
    /* Write the header */ if (s.status === $1d314fc79f930156$var$INIT_STATE && s.wrap === 0) s.status = $1d314fc79f930156$var$BUSY_STATE;
    if (s.status === $1d314fc79f930156$var$INIT_STATE) {
        /* zlib header */ let header = $1d314fc79f930156$var$Z_DEFLATED$2 + (s.w_bits - 8 << 4) << 8;
        let level_flags = -1;
        if (s.strategy >= $1d314fc79f930156$var$Z_HUFFMAN_ONLY || s.level < 2) level_flags = 0;
        else if (s.level < 6) level_flags = 1;
        else if (s.level === 6) level_flags = 2;
        else level_flags = 3;
        header |= level_flags << 6;
        if (s.strstart !== 0) header |= $1d314fc79f930156$var$PRESET_DICT;
        header += 31 - header % 31;
        $1d314fc79f930156$var$putShortMSB(s, header);
        /* Save the adler32 of the preset dictionary: */ if (s.strstart !== 0) {
            $1d314fc79f930156$var$putShortMSB(s, strm.adler >>> 16);
            $1d314fc79f930156$var$putShortMSB(s, strm.adler & 0xffff);
        }
        strm.adler = 1; // adler32(0L, Z_NULL, 0);
        s.status = $1d314fc79f930156$var$BUSY_STATE;
        /* Compression must start with an empty pending buffer */ $1d314fc79f930156$var$flush_pending(strm);
        if (s.pending !== 0) {
            s.last_flush = -1;
            return $1d314fc79f930156$var$Z_OK$3;
        }
    }
    //#ifdef GZIP
    if (s.status === $1d314fc79f930156$var$GZIP_STATE) {
        /* gzip header */ strm.adler = 0; //crc32(0L, Z_NULL, 0);
        $1d314fc79f930156$var$put_byte(s, 31);
        $1d314fc79f930156$var$put_byte(s, 139);
        $1d314fc79f930156$var$put_byte(s, 8);
        if (!s.gzhead) {
            $1d314fc79f930156$var$put_byte(s, 0);
            $1d314fc79f930156$var$put_byte(s, 0);
            $1d314fc79f930156$var$put_byte(s, 0);
            $1d314fc79f930156$var$put_byte(s, 0);
            $1d314fc79f930156$var$put_byte(s, 0);
            $1d314fc79f930156$var$put_byte(s, s.level === 9 ? 2 : s.strategy >= $1d314fc79f930156$var$Z_HUFFMAN_ONLY || s.level < 2 ? 4 : 0);
            $1d314fc79f930156$var$put_byte(s, $1d314fc79f930156$var$OS_CODE);
            s.status = $1d314fc79f930156$var$BUSY_STATE;
            /* Compression must start with an empty pending buffer */ $1d314fc79f930156$var$flush_pending(strm);
            if (s.pending !== 0) {
                s.last_flush = -1;
                return $1d314fc79f930156$var$Z_OK$3;
            }
        } else {
            $1d314fc79f930156$var$put_byte(s, (s.gzhead.text ? 1 : 0) + (s.gzhead.hcrc ? 2 : 0) + (!s.gzhead.extra ? 0 : 4) + (!s.gzhead.name ? 0 : 8) + (!s.gzhead.comment ? 0 : 16));
            $1d314fc79f930156$var$put_byte(s, s.gzhead.time & 0xff);
            $1d314fc79f930156$var$put_byte(s, s.gzhead.time >> 8 & 0xff);
            $1d314fc79f930156$var$put_byte(s, s.gzhead.time >> 16 & 0xff);
            $1d314fc79f930156$var$put_byte(s, s.gzhead.time >> 24 & 0xff);
            $1d314fc79f930156$var$put_byte(s, s.level === 9 ? 2 : s.strategy >= $1d314fc79f930156$var$Z_HUFFMAN_ONLY || s.level < 2 ? 4 : 0);
            $1d314fc79f930156$var$put_byte(s, s.gzhead.os & 0xff);
            if (s.gzhead.extra && s.gzhead.extra.length) {
                $1d314fc79f930156$var$put_byte(s, s.gzhead.extra.length & 0xff);
                $1d314fc79f930156$var$put_byte(s, s.gzhead.extra.length >> 8 & 0xff);
            }
            if (s.gzhead.hcrc) strm.adler = $1d314fc79f930156$var$crc32_1(strm.adler, s.pending_buf, s.pending, 0);
            s.gzindex = 0;
            s.status = $1d314fc79f930156$var$EXTRA_STATE;
        }
    }
    if (s.status === $1d314fc79f930156$var$EXTRA_STATE) {
        if (s.gzhead.extra /* != Z_NULL*/ ) {
            let beg = s.pending; /* start of bytes to update crc */ 
            let left = (s.gzhead.extra.length & 0xffff) - s.gzindex;
            while(s.pending + left > s.pending_buf_size){
                let copy = s.pending_buf_size - s.pending;
                // zmemcpy(s.pending_buf + s.pending,
                //    s.gzhead.extra + s.gzindex, copy);
                s.pending_buf.set(s.gzhead.extra.subarray(s.gzindex, s.gzindex + copy), s.pending);
                s.pending = s.pending_buf_size;
                //--- HCRC_UPDATE(beg) ---//
                if (s.gzhead.hcrc && s.pending > beg) strm.adler = $1d314fc79f930156$var$crc32_1(strm.adler, s.pending_buf, s.pending - beg, beg);
                //---//
                s.gzindex += copy;
                $1d314fc79f930156$var$flush_pending(strm);
                if (s.pending !== 0) {
                    s.last_flush = -1;
                    return $1d314fc79f930156$var$Z_OK$3;
                }
                beg = 0;
                left -= copy;
            }
            // JS specific: s.gzhead.extra may be TypedArray or Array for backward compatibility
            //              TypedArray.slice and TypedArray.from don't exist in IE10-IE11
            let gzhead_extra = new Uint8Array(s.gzhead.extra);
            // zmemcpy(s->pending_buf + s->pending,
            //     s->gzhead->extra + s->gzindex, left);
            s.pending_buf.set(gzhead_extra.subarray(s.gzindex, s.gzindex + left), s.pending);
            s.pending += left;
            //--- HCRC_UPDATE(beg) ---//
            if (s.gzhead.hcrc && s.pending > beg) strm.adler = $1d314fc79f930156$var$crc32_1(strm.adler, s.pending_buf, s.pending - beg, beg);
            //---//
            s.gzindex = 0;
        }
        s.status = $1d314fc79f930156$var$NAME_STATE;
    }
    if (s.status === $1d314fc79f930156$var$NAME_STATE) {
        if (s.gzhead.name /* != Z_NULL*/ ) {
            let beg = s.pending; /* start of bytes to update crc */ 
            let val;
            do {
                if (s.pending === s.pending_buf_size) {
                    //--- HCRC_UPDATE(beg) ---//
                    if (s.gzhead.hcrc && s.pending > beg) strm.adler = $1d314fc79f930156$var$crc32_1(strm.adler, s.pending_buf, s.pending - beg, beg);
                    //---//
                    $1d314fc79f930156$var$flush_pending(strm);
                    if (s.pending !== 0) {
                        s.last_flush = -1;
                        return $1d314fc79f930156$var$Z_OK$3;
                    }
                    beg = 0;
                }
                // JS specific: little magic to add zero terminator to end of string
                if (s.gzindex < s.gzhead.name.length) val = s.gzhead.name.charCodeAt(s.gzindex++) & 0xff;
                else val = 0;
                $1d314fc79f930156$var$put_byte(s, val);
            }while (val !== 0);
            //--- HCRC_UPDATE(beg) ---//
            if (s.gzhead.hcrc && s.pending > beg) strm.adler = $1d314fc79f930156$var$crc32_1(strm.adler, s.pending_buf, s.pending - beg, beg);
            //---//
            s.gzindex = 0;
        }
        s.status = $1d314fc79f930156$var$COMMENT_STATE;
    }
    if (s.status === $1d314fc79f930156$var$COMMENT_STATE) {
        if (s.gzhead.comment /* != Z_NULL*/ ) {
            let beg = s.pending; /* start of bytes to update crc */ 
            let val;
            do {
                if (s.pending === s.pending_buf_size) {
                    //--- HCRC_UPDATE(beg) ---//
                    if (s.gzhead.hcrc && s.pending > beg) strm.adler = $1d314fc79f930156$var$crc32_1(strm.adler, s.pending_buf, s.pending - beg, beg);
                    //---//
                    $1d314fc79f930156$var$flush_pending(strm);
                    if (s.pending !== 0) {
                        s.last_flush = -1;
                        return $1d314fc79f930156$var$Z_OK$3;
                    }
                    beg = 0;
                }
                // JS specific: little magic to add zero terminator to end of string
                if (s.gzindex < s.gzhead.comment.length) val = s.gzhead.comment.charCodeAt(s.gzindex++) & 0xff;
                else val = 0;
                $1d314fc79f930156$var$put_byte(s, val);
            }while (val !== 0);
            //--- HCRC_UPDATE(beg) ---//
            if (s.gzhead.hcrc && s.pending > beg) strm.adler = $1d314fc79f930156$var$crc32_1(strm.adler, s.pending_buf, s.pending - beg, beg);
        //---//
        }
        s.status = $1d314fc79f930156$var$HCRC_STATE;
    }
    if (s.status === $1d314fc79f930156$var$HCRC_STATE) {
        if (s.gzhead.hcrc) {
            if (s.pending + 2 > s.pending_buf_size) {
                $1d314fc79f930156$var$flush_pending(strm);
                if (s.pending !== 0) {
                    s.last_flush = -1;
                    return $1d314fc79f930156$var$Z_OK$3;
                }
            }
            $1d314fc79f930156$var$put_byte(s, strm.adler & 0xff);
            $1d314fc79f930156$var$put_byte(s, strm.adler >> 8 & 0xff);
            strm.adler = 0; //crc32(0L, Z_NULL, 0);
        }
        s.status = $1d314fc79f930156$var$BUSY_STATE;
        /* Compression must start with an empty pending buffer */ $1d314fc79f930156$var$flush_pending(strm);
        if (s.pending !== 0) {
            s.last_flush = -1;
            return $1d314fc79f930156$var$Z_OK$3;
        }
    }
    //#endif
    /* Start a new block or continue the current one.
   */ if (strm.avail_in !== 0 || s.lookahead !== 0 || flush !== $1d314fc79f930156$var$Z_NO_FLUSH$2 && s.status !== $1d314fc79f930156$var$FINISH_STATE) {
        let bstate = s.level === 0 ? $1d314fc79f930156$var$deflate_stored(s, flush) : s.strategy === $1d314fc79f930156$var$Z_HUFFMAN_ONLY ? $1d314fc79f930156$var$deflate_huff(s, flush) : s.strategy === $1d314fc79f930156$var$Z_RLE ? $1d314fc79f930156$var$deflate_rle(s, flush) : $1d314fc79f930156$var$configuration_table[s.level].func(s, flush);
        if (bstate === $1d314fc79f930156$var$BS_FINISH_STARTED || bstate === $1d314fc79f930156$var$BS_FINISH_DONE) s.status = $1d314fc79f930156$var$FINISH_STATE;
        if (bstate === $1d314fc79f930156$var$BS_NEED_MORE || bstate === $1d314fc79f930156$var$BS_FINISH_STARTED) {
            if (strm.avail_out === 0) s.last_flush = -1;
            return $1d314fc79f930156$var$Z_OK$3;
        /* If flush != Z_NO_FLUSH && avail_out == 0, the next call
       * of deflate should use the same flush parameter to make sure
       * that the flush is complete. So we don't have to output an
       * empty block here, this will be done at next call. This also
       * ensures that for a very small output buffer, we emit at most
       * one empty block.
       */ }
        if (bstate === $1d314fc79f930156$var$BS_BLOCK_DONE) {
            if (flush === $1d314fc79f930156$var$Z_PARTIAL_FLUSH) $1d314fc79f930156$var$_tr_align(s);
            else if (flush !== $1d314fc79f930156$var$Z_BLOCK$1) {
                $1d314fc79f930156$var$_tr_stored_block(s, 0, 0, false);
                /* For a full flush, this empty block will be recognized
         * as a special marker by inflate_sync().
         */ if (flush === $1d314fc79f930156$var$Z_FULL_FLUSH$1) {
                    /*** CLEAR_HASH(s); ***/ /* forget history */ $1d314fc79f930156$var$zero(s.head); // Fill with NIL (= 0);
                    if (s.lookahead === 0) {
                        s.strstart = 0;
                        s.block_start = 0;
                        s.insert = 0;
                    }
                }
            }
            $1d314fc79f930156$var$flush_pending(strm);
            if (strm.avail_out === 0) {
                s.last_flush = -1; /* avoid BUF_ERROR at next call, see above */ 
                return $1d314fc79f930156$var$Z_OK$3;
            }
        }
    }
    if (flush !== $1d314fc79f930156$var$Z_FINISH$3) return $1d314fc79f930156$var$Z_OK$3;
    if (s.wrap <= 0) return $1d314fc79f930156$var$Z_STREAM_END$3;
    /* Write the trailer */ if (s.wrap === 2) {
        $1d314fc79f930156$var$put_byte(s, strm.adler & 0xff);
        $1d314fc79f930156$var$put_byte(s, strm.adler >> 8 & 0xff);
        $1d314fc79f930156$var$put_byte(s, strm.adler >> 16 & 0xff);
        $1d314fc79f930156$var$put_byte(s, strm.adler >> 24 & 0xff);
        $1d314fc79f930156$var$put_byte(s, strm.total_in & 0xff);
        $1d314fc79f930156$var$put_byte(s, strm.total_in >> 8 & 0xff);
        $1d314fc79f930156$var$put_byte(s, strm.total_in >> 16 & 0xff);
        $1d314fc79f930156$var$put_byte(s, strm.total_in >> 24 & 0xff);
    } else {
        $1d314fc79f930156$var$putShortMSB(s, strm.adler >>> 16);
        $1d314fc79f930156$var$putShortMSB(s, strm.adler & 0xffff);
    }
    $1d314fc79f930156$var$flush_pending(strm);
    /* If avail_out is zero, the application will call deflate again
   * to flush the rest.
   */ if (s.wrap > 0) s.wrap = -s.wrap;
    /* write the trailer only once! */ return s.pending !== 0 ? $1d314fc79f930156$var$Z_OK$3 : $1d314fc79f930156$var$Z_STREAM_END$3;
};
const $1d314fc79f930156$var$deflateEnd = (strm)=>{
    if ($1d314fc79f930156$var$deflateStateCheck(strm)) return $1d314fc79f930156$var$Z_STREAM_ERROR$2;
    const status = strm.state.status;
    strm.state = null;
    return status === $1d314fc79f930156$var$BUSY_STATE ? $1d314fc79f930156$var$err(strm, $1d314fc79f930156$var$Z_DATA_ERROR$2) : $1d314fc79f930156$var$Z_OK$3;
};
/* =========================================================================
 * Initializes the compression dictionary from the given byte
 * sequence without producing any compressed output.
 */ const $1d314fc79f930156$var$deflateSetDictionary = (strm, dictionary)=>{
    let dictLength = dictionary.length;
    if ($1d314fc79f930156$var$deflateStateCheck(strm)) return $1d314fc79f930156$var$Z_STREAM_ERROR$2;
    const s = strm.state;
    const wrap = s.wrap;
    if (wrap === 2 || wrap === 1 && s.status !== $1d314fc79f930156$var$INIT_STATE || s.lookahead) return $1d314fc79f930156$var$Z_STREAM_ERROR$2;
    /* when using zlib wrappers, compute Adler-32 for provided dictionary */ if (wrap === 1) /* adler32(strm->adler, dictionary, dictLength); */ strm.adler = $1d314fc79f930156$var$adler32_1(strm.adler, dictionary, dictLength, 0);
    s.wrap = 0; /* avoid computing Adler-32 in read_buf */ 
    /* if dictionary would fill window, just replace the history */ if (dictLength >= s.w_size) {
        if (wrap === 0) {
            /*** CLEAR_HASH(s); ***/ $1d314fc79f930156$var$zero(s.head); // Fill with NIL (= 0);
            s.strstart = 0;
            s.block_start = 0;
            s.insert = 0;
        }
        /* use the tail */ // dictionary = dictionary.slice(dictLength - s.w_size);
        let tmpDict = new Uint8Array(s.w_size);
        tmpDict.set(dictionary.subarray(dictLength - s.w_size, dictLength), 0);
        dictionary = tmpDict;
        dictLength = s.w_size;
    }
    /* insert dictionary into window and hash */ const avail = strm.avail_in;
    const next = strm.next_in;
    const input = strm.input;
    strm.avail_in = dictLength;
    strm.next_in = 0;
    strm.input = dictionary;
    $1d314fc79f930156$var$fill_window(s);
    while(s.lookahead >= $1d314fc79f930156$var$MIN_MATCH){
        let str = s.strstart;
        let n = s.lookahead - ($1d314fc79f930156$var$MIN_MATCH - 1);
        do {
            /* UPDATE_HASH(s, s->ins_h, s->window[str + MIN_MATCH-1]); */ s.ins_h = $1d314fc79f930156$var$HASH(s, s.ins_h, s.window[str + $1d314fc79f930156$var$MIN_MATCH - 1]);
            s.prev[str & s.w_mask] = s.head[s.ins_h];
            s.head[s.ins_h] = str;
            str++;
        }while (--n);
        s.strstart = str;
        s.lookahead = $1d314fc79f930156$var$MIN_MATCH - 1;
        $1d314fc79f930156$var$fill_window(s);
    }
    s.strstart += s.lookahead;
    s.block_start = s.strstart;
    s.insert = s.lookahead;
    s.lookahead = 0;
    s.match_length = s.prev_length = $1d314fc79f930156$var$MIN_MATCH - 1;
    s.match_available = 0;
    strm.next_in = next;
    strm.input = input;
    strm.avail_in = avail;
    s.wrap = wrap;
    return $1d314fc79f930156$var$Z_OK$3;
};
var $1d314fc79f930156$var$deflateInit_1 = $1d314fc79f930156$var$deflateInit;
var $1d314fc79f930156$var$deflateInit2_1 = $1d314fc79f930156$var$deflateInit2;
var $1d314fc79f930156$var$deflateReset_1 = $1d314fc79f930156$var$deflateReset;
var $1d314fc79f930156$var$deflateResetKeep_1 = $1d314fc79f930156$var$deflateResetKeep;
var $1d314fc79f930156$var$deflateSetHeader_1 = $1d314fc79f930156$var$deflateSetHeader;
var $1d314fc79f930156$var$deflate_2$1 = $1d314fc79f930156$var$deflate$2;
var $1d314fc79f930156$var$deflateEnd_1 = $1d314fc79f930156$var$deflateEnd;
var $1d314fc79f930156$var$deflateSetDictionary_1 = $1d314fc79f930156$var$deflateSetDictionary;
var $1d314fc79f930156$var$deflateInfo = "pako deflate (from Nodeca project)";
/* Not implemented
module.exports.deflateBound = deflateBound;
module.exports.deflateCopy = deflateCopy;
module.exports.deflateGetDictionary = deflateGetDictionary;
module.exports.deflateParams = deflateParams;
module.exports.deflatePending = deflatePending;
module.exports.deflatePrime = deflatePrime;
module.exports.deflateTune = deflateTune;
*/ var $1d314fc79f930156$var$deflate_1$2 = {
    deflateInit: $1d314fc79f930156$var$deflateInit_1,
    deflateInit2: $1d314fc79f930156$var$deflateInit2_1,
    deflateReset: $1d314fc79f930156$var$deflateReset_1,
    deflateResetKeep: $1d314fc79f930156$var$deflateResetKeep_1,
    deflateSetHeader: $1d314fc79f930156$var$deflateSetHeader_1,
    deflate: $1d314fc79f930156$var$deflate_2$1,
    deflateEnd: $1d314fc79f930156$var$deflateEnd_1,
    deflateSetDictionary: $1d314fc79f930156$var$deflateSetDictionary_1,
    deflateInfo: $1d314fc79f930156$var$deflateInfo
};
const $1d314fc79f930156$var$_has = (obj, key)=>{
    return Object.prototype.hasOwnProperty.call(obj, key);
};
var $1d314fc79f930156$var$assign = function(obj /*from1, from2, from3, ...*/ ) {
    const sources = Array.prototype.slice.call(arguments, 1);
    while(sources.length){
        const source = sources.shift();
        if (!source) continue;
        if (typeof source !== "object") throw new TypeError(source + "must be non-object");
        for(const p in source)if ($1d314fc79f930156$var$_has(source, p)) obj[p] = source[p];
    }
    return obj;
};
// Join array of chunks to single array.
var $1d314fc79f930156$var$flattenChunks = (chunks)=>{
    // calculate data length
    let len = 0;
    for(let i = 0, l = chunks.length; i < l; i++)len += chunks[i].length;
    // join chunks
    const result = new Uint8Array(len);
    for(let i = 0, pos = 0, l = chunks.length; i < l; i++){
        let chunk = chunks[i];
        result.set(chunk, pos);
        pos += chunk.length;
    }
    return result;
};
var $1d314fc79f930156$var$common = {
    assign: $1d314fc79f930156$var$assign,
    flattenChunks: $1d314fc79f930156$var$flattenChunks
};
// String encode/decode helpers
// Quick check if we can use fast array to bin string conversion
//
// - apply(Array) can fail on Android 2.2
// - apply(Uint8Array) can fail on iOS 5.1 Safari
//
let $1d314fc79f930156$var$STR_APPLY_UIA_OK = true;
try {
    String.fromCharCode.apply(null, new Uint8Array(1));
} catch (__) {
    $1d314fc79f930156$var$STR_APPLY_UIA_OK = false;
}
// Table with utf8 lengths (calculated by first byte of sequence)
// Note, that 5 & 6-byte values and some 4-byte values can not be represented in JS,
// because max possible codepoint is 0x10ffff
const $1d314fc79f930156$var$_utf8len = new Uint8Array(256);
for(let q = 0; q < 256; q++)$1d314fc79f930156$var$_utf8len[q] = q >= 252 ? 6 : q >= 248 ? 5 : q >= 240 ? 4 : q >= 224 ? 3 : q >= 192 ? 2 : 1;
$1d314fc79f930156$var$_utf8len[254] = $1d314fc79f930156$var$_utf8len[254] = 1; // Invalid sequence start
// convert string to array (typed, when possible)
var $1d314fc79f930156$var$string2buf = (str)=>{
    if (typeof TextEncoder === "function" && TextEncoder.prototype.encode) return new TextEncoder().encode(str);
    let buf, c, c2, m_pos, i, str_len = str.length, buf_len = 0;
    // count binary size
    for(m_pos = 0; m_pos < str_len; m_pos++){
        c = str.charCodeAt(m_pos);
        if ((c & 0xfc00) === 0xd800 && m_pos + 1 < str_len) {
            c2 = str.charCodeAt(m_pos + 1);
            if ((c2 & 0xfc00) === 0xdc00) {
                c = 0x10000 + (c - 0xd800 << 10) + (c2 - 0xdc00);
                m_pos++;
            }
        }
        buf_len += c < 0x80 ? 1 : c < 0x800 ? 2 : c < 0x10000 ? 3 : 4;
    }
    // allocate buffer
    buf = new Uint8Array(buf_len);
    // convert
    for(i = 0, m_pos = 0; i < buf_len; m_pos++){
        c = str.charCodeAt(m_pos);
        if ((c & 0xfc00) === 0xd800 && m_pos + 1 < str_len) {
            c2 = str.charCodeAt(m_pos + 1);
            if ((c2 & 0xfc00) === 0xdc00) {
                c = 0x10000 + (c - 0xd800 << 10) + (c2 - 0xdc00);
                m_pos++;
            }
        }
        if (c < 0x80) /* one byte */ buf[i++] = c;
        else if (c < 0x800) {
            /* two bytes */ buf[i++] = 0xC0 | c >>> 6;
            buf[i++] = 0x80 | c & 0x3f;
        } else if (c < 0x10000) {
            /* three bytes */ buf[i++] = 0xE0 | c >>> 12;
            buf[i++] = 0x80 | c >>> 6 & 0x3f;
            buf[i++] = 0x80 | c & 0x3f;
        } else {
            /* four bytes */ buf[i++] = 0xf0 | c >>> 18;
            buf[i++] = 0x80 | c >>> 12 & 0x3f;
            buf[i++] = 0x80 | c >>> 6 & 0x3f;
            buf[i++] = 0x80 | c & 0x3f;
        }
    }
    return buf;
};
// Helper
const $1d314fc79f930156$var$buf2binstring = (buf, len)=>{
    // On Chrome, the arguments in a function call that are allowed is `65534`.
    // If the length of the buffer is smaller than that, we can use this optimization,
    // otherwise we will take a slower path.
    if (len < 65534) {
        if (buf.subarray && $1d314fc79f930156$var$STR_APPLY_UIA_OK) return String.fromCharCode.apply(null, buf.length === len ? buf : buf.subarray(0, len));
    }
    let result = "";
    for(let i = 0; i < len; i++)result += String.fromCharCode(buf[i]);
    return result;
};
// convert array to string
var $1d314fc79f930156$var$buf2string = (buf, max)=>{
    const len = max || buf.length;
    if (typeof TextDecoder === "function" && TextDecoder.prototype.decode) return new TextDecoder().decode(buf.subarray(0, max));
    let i, out;
    // Reserve max possible length (2 words per char)
    // NB: by unknown reasons, Array is significantly faster for
    //     String.fromCharCode.apply than Uint16Array.
    const utf16buf = new Array(len * 2);
    for(out = 0, i = 0; i < len;){
        let c = buf[i++];
        // quick process ascii
        if (c < 0x80) {
            utf16buf[out++] = c;
            continue;
        }
        let c_len = $1d314fc79f930156$var$_utf8len[c];
        // skip 5 & 6 byte codes
        if (c_len > 4) {
            utf16buf[out++] = 0xfffd;
            i += c_len - 1;
            continue;
        }
        // apply mask on first byte
        c &= c_len === 2 ? 0x1f : c_len === 3 ? 0x0f : 0x07;
        // join the rest
        while(c_len > 1 && i < len){
            c = c << 6 | buf[i++] & 0x3f;
            c_len--;
        }
        // terminated by end of string?
        if (c_len > 1) {
            utf16buf[out++] = 0xfffd;
            continue;
        }
        if (c < 0x10000) utf16buf[out++] = c;
        else {
            c -= 0x10000;
            utf16buf[out++] = 0xd800 | c >> 10 & 0x3ff;
            utf16buf[out++] = 0xdc00 | c & 0x3ff;
        }
    }
    return $1d314fc79f930156$var$buf2binstring(utf16buf, out);
};
// Calculate max possible position in utf8 buffer,
// that will not break sequence. If that's not possible
// - (very small limits) return max size as is.
//
// buf[] - utf8 bytes array
// max   - length limit (mandatory);
var $1d314fc79f930156$var$utf8border = (buf, max)=>{
    max = max || buf.length;
    if (max > buf.length) max = buf.length;
    // go back from last position, until start of sequence found
    let pos = max - 1;
    while(pos >= 0 && (buf[pos] & 0xC0) === 0x80)pos--;
    // Very small and broken sequence,
    // return max, because we should return something anyway.
    if (pos < 0) return max;
    // If we came to start of buffer - that means buffer is too small,
    // return max too.
    if (pos === 0) return max;
    return pos + $1d314fc79f930156$var$_utf8len[buf[pos]] > max ? pos : max;
};
var $1d314fc79f930156$var$strings = {
    string2buf: $1d314fc79f930156$var$string2buf,
    buf2string: $1d314fc79f930156$var$buf2string,
    utf8border: $1d314fc79f930156$var$utf8border
};
// (C) 1995-2013 Jean-loup Gailly and Mark Adler
// (C) 2014-2017 Vitaly Puzrin and Andrey Tupitsin
//
// This software is provided 'as-is', without any express or implied
// warranty. In no event will the authors be held liable for any damages
// arising from the use of this software.
//
// Permission is granted to anyone to use this software for any purpose,
// including commercial applications, and to alter it and redistribute it
// freely, subject to the following restrictions:
//
// 1. The origin of this software must not be misrepresented; you must not
//   claim that you wrote the original software. If you use this software
//   in a product, an acknowledgment in the product documentation would be
//   appreciated but is not required.
// 2. Altered source versions must be plainly marked as such, and must not be
//   misrepresented as being the original software.
// 3. This notice may not be removed or altered from any source distribution.
function $1d314fc79f930156$var$ZStream() {
    /* next input byte */ this.input = null; // JS specific, because we have no pointers
    this.next_in = 0;
    /* number of bytes available at input */ this.avail_in = 0;
    /* total number of input bytes read so far */ this.total_in = 0;
    /* next output byte should be put there */ this.output = null; // JS specific, because we have no pointers
    this.next_out = 0;
    /* remaining free space at output */ this.avail_out = 0;
    /* total number of bytes output so far */ this.total_out = 0;
    /* last error message, NULL if no error */ this.msg = "" /*Z_NULL*/ ;
    /* not visible by applications */ this.state = null;
    /* best guess about the data type: binary or text */ this.data_type = 2 /*Z_UNKNOWN*/ ;
    /* adler32 value of the uncompressed data */ this.adler = 0;
}
var $1d314fc79f930156$var$zstream = $1d314fc79f930156$var$ZStream;
const $1d314fc79f930156$var$toString$1 = Object.prototype.toString;
/* Public constants ==========================================================*/ /* ===========================================================================*/ const { Z_NO_FLUSH: $1d314fc79f930156$var$Z_NO_FLUSH$1, Z_SYNC_FLUSH: $1d314fc79f930156$var$Z_SYNC_FLUSH, Z_FULL_FLUSH: $1d314fc79f930156$var$Z_FULL_FLUSH, Z_FINISH: $1d314fc79f930156$var$Z_FINISH$2, Z_OK: $1d314fc79f930156$var$Z_OK$2, Z_STREAM_END: $1d314fc79f930156$var$Z_STREAM_END$2, Z_DEFAULT_COMPRESSION: $1d314fc79f930156$var$Z_DEFAULT_COMPRESSION, Z_DEFAULT_STRATEGY: $1d314fc79f930156$var$Z_DEFAULT_STRATEGY, Z_DEFLATED: $1d314fc79f930156$var$Z_DEFLATED$1 } = $1d314fc79f930156$var$constants$2;
/* ===========================================================================*/ /**
 * class Deflate
 *
 * Generic JS-style wrapper for zlib calls. If you don't need
 * streaming behaviour - use more simple functions: [[deflate]],
 * [[deflateRaw]] and [[gzip]].
 **/ /* internal
 * Deflate.chunks -> Array
 *
 * Chunks of output data, if [[Deflate#onData]] not overridden.
 **/ /**
 * Deflate.result -> Uint8Array
 *
 * Compressed result, generated by default [[Deflate#onData]]
 * and [[Deflate#onEnd]] handlers. Filled after you push last chunk
 * (call [[Deflate#push]] with `Z_FINISH` / `true` param).
 **/ /**
 * Deflate.err -> Number
 *
 * Error code after deflate finished. 0 (Z_OK) on success.
 * You will not need it in real life, because deflate errors
 * are possible only on wrong options or bad `onData` / `onEnd`
 * custom handlers.
 **/ /**
 * Deflate.msg -> String
 *
 * Error message, if [[Deflate.err]] != 0
 **/ /**
 * new Deflate(options)
 * - options (Object): zlib deflate options.
 *
 * Creates new deflator instance with specified params. Throws exception
 * on bad params. Supported options:
 *
 * - `level`
 * - `windowBits`
 * - `memLevel`
 * - `strategy`
 * - `dictionary`
 *
 * [http://zlib.net/manual.html#Advanced](http://zlib.net/manual.html#Advanced)
 * for more information on these.
 *
 * Additional options, for internal needs:
 *
 * - `chunkSize` - size of generated data chunks (16K by default)
 * - `raw` (Boolean) - do raw deflate
 * - `gzip` (Boolean) - create gzip wrapper
 * - `header` (Object) - custom header for gzip
 *   - `text` (Boolean) - true if compressed data believed to be text
 *   - `time` (Number) - modification time, unix timestamp
 *   - `os` (Number) - operation system code
 *   - `extra` (Array) - array of bytes with extra data (max 65536)
 *   - `name` (String) - file name (binary string)
 *   - `comment` (String) - comment (binary string)
 *   - `hcrc` (Boolean) - true if header crc should be added
 *
 * ##### Example:
 *
 * ```javascript
 * const pako = require('pako')
 *   , chunk1 = new Uint8Array([1,2,3,4,5,6,7,8,9])
 *   , chunk2 = new Uint8Array([10,11,12,13,14,15,16,17,18,19]);
 *
 * const deflate = new pako.Deflate({ level: 3});
 *
 * deflate.push(chunk1, false);
 * deflate.push(chunk2, true);  // true -> last chunk
 *
 * if (deflate.err) { throw new Error(deflate.err); }
 *
 * console.log(deflate.result);
 * ```
 **/ function $1d314fc79f930156$var$Deflate$1(options) {
    this.options = $1d314fc79f930156$var$common.assign({
        level: $1d314fc79f930156$var$Z_DEFAULT_COMPRESSION,
        method: $1d314fc79f930156$var$Z_DEFLATED$1,
        chunkSize: 16384,
        windowBits: 15,
        memLevel: 8,
        strategy: $1d314fc79f930156$var$Z_DEFAULT_STRATEGY
    }, options || {});
    let opt = this.options;
    if (opt.raw && opt.windowBits > 0) opt.windowBits = -opt.windowBits;
    else if (opt.gzip && opt.windowBits > 0 && opt.windowBits < 16) opt.windowBits += 16;
    this.err = 0; // error code, if happens (0 = Z_OK)
    this.msg = ""; // error message
    this.ended = false; // used to avoid multiple onEnd() calls
    this.chunks = []; // chunks of compressed data
    this.strm = new $1d314fc79f930156$var$zstream();
    this.strm.avail_out = 0;
    let status = $1d314fc79f930156$var$deflate_1$2.deflateInit2(this.strm, opt.level, opt.method, opt.windowBits, opt.memLevel, opt.strategy);
    if (status !== $1d314fc79f930156$var$Z_OK$2) throw new Error($1d314fc79f930156$var$messages[status]);
    if (opt.header) $1d314fc79f930156$var$deflate_1$2.deflateSetHeader(this.strm, opt.header);
    if (opt.dictionary) {
        let dict;
        // Convert data if needed
        if (typeof opt.dictionary === "string") // If we need to compress text, change encoding to utf8.
        dict = $1d314fc79f930156$var$strings.string2buf(opt.dictionary);
        else if ($1d314fc79f930156$var$toString$1.call(opt.dictionary) === "[object ArrayBuffer]") dict = new Uint8Array(opt.dictionary);
        else dict = opt.dictionary;
        status = $1d314fc79f930156$var$deflate_1$2.deflateSetDictionary(this.strm, dict);
        if (status !== $1d314fc79f930156$var$Z_OK$2) throw new Error($1d314fc79f930156$var$messages[status]);
        this._dict_set = true;
    }
}
/**
 * Deflate#push(data[, flush_mode]) -> Boolean
 * - data (Uint8Array|ArrayBuffer|String): input data. Strings will be
 *   converted to utf8 byte sequence.
 * - flush_mode (Number|Boolean): 0..6 for corresponding Z_NO_FLUSH..Z_TREE modes.
 *   See constants. Skipped or `false` means Z_NO_FLUSH, `true` means Z_FINISH.
 *
 * Sends input data to deflate pipe, generating [[Deflate#onData]] calls with
 * new compressed chunks. Returns `true` on success. The last data block must
 * have `flush_mode` Z_FINISH (or `true`). That will flush internal pending
 * buffers and call [[Deflate#onEnd]].
 *
 * On fail call [[Deflate#onEnd]] with error code and return false.
 *
 * ##### Example
 *
 * ```javascript
 * push(chunk, false); // push one of data chunks
 * ...
 * push(chunk, true);  // push last chunk
 * ```
 **/ $1d314fc79f930156$var$Deflate$1.prototype.push = function(data, flush_mode) {
    const strm = this.strm;
    const chunkSize = this.options.chunkSize;
    let status, _flush_mode;
    if (this.ended) return false;
    if (flush_mode === ~~flush_mode) _flush_mode = flush_mode;
    else _flush_mode = flush_mode === true ? $1d314fc79f930156$var$Z_FINISH$2 : $1d314fc79f930156$var$Z_NO_FLUSH$1;
    // Convert data if needed
    if (typeof data === "string") // If we need to compress text, change encoding to utf8.
    strm.input = $1d314fc79f930156$var$strings.string2buf(data);
    else if ($1d314fc79f930156$var$toString$1.call(data) === "[object ArrayBuffer]") strm.input = new Uint8Array(data);
    else strm.input = data;
    strm.next_in = 0;
    strm.avail_in = strm.input.length;
    for(;;){
        if (strm.avail_out === 0) {
            strm.output = new Uint8Array(chunkSize);
            strm.next_out = 0;
            strm.avail_out = chunkSize;
        }
        // Make sure avail_out > 6 to avoid repeating markers
        if ((_flush_mode === $1d314fc79f930156$var$Z_SYNC_FLUSH || _flush_mode === $1d314fc79f930156$var$Z_FULL_FLUSH) && strm.avail_out <= 6) {
            this.onData(strm.output.subarray(0, strm.next_out));
            strm.avail_out = 0;
            continue;
        }
        status = $1d314fc79f930156$var$deflate_1$2.deflate(strm, _flush_mode);
        // Ended => flush and finish
        if (status === $1d314fc79f930156$var$Z_STREAM_END$2) {
            if (strm.next_out > 0) this.onData(strm.output.subarray(0, strm.next_out));
            status = $1d314fc79f930156$var$deflate_1$2.deflateEnd(this.strm);
            this.onEnd(status);
            this.ended = true;
            return status === $1d314fc79f930156$var$Z_OK$2;
        }
        // Flush if out buffer full
        if (strm.avail_out === 0) {
            this.onData(strm.output);
            continue;
        }
        // Flush if requested and has data
        if (_flush_mode > 0 && strm.next_out > 0) {
            this.onData(strm.output.subarray(0, strm.next_out));
            strm.avail_out = 0;
            continue;
        }
        if (strm.avail_in === 0) break;
    }
    return true;
};
/**
 * Deflate#onData(chunk) -> Void
 * - chunk (Uint8Array): output data.
 *
 * By default, stores data blocks in `chunks[]` property and glue
 * those in `onEnd`. Override this handler, if you need another behaviour.
 **/ $1d314fc79f930156$var$Deflate$1.prototype.onData = function(chunk) {
    this.chunks.push(chunk);
};
/**
 * Deflate#onEnd(status) -> Void
 * - status (Number): deflate status. 0 (Z_OK) on success,
 *   other if not.
 *
 * Called once after you tell deflate that the input stream is
 * complete (Z_FINISH). By default - join collected chunks,
 * free memory and fill `results` / `err` properties.
 **/ $1d314fc79f930156$var$Deflate$1.prototype.onEnd = function(status) {
    // On success - join
    if (status === $1d314fc79f930156$var$Z_OK$2) this.result = $1d314fc79f930156$var$common.flattenChunks(this.chunks);
    this.chunks = [];
    this.err = status;
    this.msg = this.strm.msg;
};
/**
 * deflate(data[, options]) -> Uint8Array
 * - data (Uint8Array|ArrayBuffer|String): input data to compress.
 * - options (Object): zlib deflate options.
 *
 * Compress `data` with deflate algorithm and `options`.
 *
 * Supported options are:
 *
 * - level
 * - windowBits
 * - memLevel
 * - strategy
 * - dictionary
 *
 * [http://zlib.net/manual.html#Advanced](http://zlib.net/manual.html#Advanced)
 * for more information on these.
 *
 * Sugar (options):
 *
 * - `raw` (Boolean) - say that we work with raw stream, if you don't wish to specify
 *   negative windowBits implicitly.
 *
 * ##### Example:
 *
 * ```javascript
 * const pako = require('pako')
 * const data = new Uint8Array([1,2,3,4,5,6,7,8,9]);
 *
 * console.log(pako.deflate(data));
 * ```
 **/ function $1d314fc79f930156$var$deflate$1(input, options) {
    const deflator = new $1d314fc79f930156$var$Deflate$1(options);
    deflator.push(input, true);
    // That will never happens, if you don't cheat with options :)
    if (deflator.err) throw deflator.msg || $1d314fc79f930156$var$messages[deflator.err];
    return deflator.result;
}
/**
 * deflateRaw(data[, options]) -> Uint8Array
 * - data (Uint8Array|ArrayBuffer|String): input data to compress.
 * - options (Object): zlib deflate options.
 *
 * The same as [[deflate]], but creates raw data, without wrapper
 * (header and adler32 crc).
 **/ function $1d314fc79f930156$var$deflateRaw$1(input, options) {
    options = options || {};
    options.raw = true;
    return $1d314fc79f930156$var$deflate$1(input, options);
}
/**
 * gzip(data[, options]) -> Uint8Array
 * - data (Uint8Array|ArrayBuffer|String): input data to compress.
 * - options (Object): zlib deflate options.
 *
 * The same as [[deflate]], but create gzip wrapper instead of
 * deflate one.
 **/ function $1d314fc79f930156$var$gzip$1(input, options) {
    options = options || {};
    options.gzip = true;
    return $1d314fc79f930156$var$deflate$1(input, options);
}
var $1d314fc79f930156$var$Deflate_1$1 = $1d314fc79f930156$var$Deflate$1;
var $1d314fc79f930156$var$deflate_2 = $1d314fc79f930156$var$deflate$1;
var $1d314fc79f930156$var$deflateRaw_1$1 = $1d314fc79f930156$var$deflateRaw$1;
var $1d314fc79f930156$var$gzip_1$1 = $1d314fc79f930156$var$gzip$1;
var $1d314fc79f930156$var$constants$1 = $1d314fc79f930156$var$constants$2;
var $1d314fc79f930156$var$deflate_1$1 = {
    Deflate: $1d314fc79f930156$var$Deflate_1$1,
    deflate: $1d314fc79f930156$var$deflate_2,
    deflateRaw: $1d314fc79f930156$var$deflateRaw_1$1,
    gzip: $1d314fc79f930156$var$gzip_1$1,
    constants: $1d314fc79f930156$var$constants$1
};
// (C) 1995-2013 Jean-loup Gailly and Mark Adler
// (C) 2014-2017 Vitaly Puzrin and Andrey Tupitsin
//
// This software is provided 'as-is', without any express or implied
// warranty. In no event will the authors be held liable for any damages
// arising from the use of this software.
//
// Permission is granted to anyone to use this software for any purpose,
// including commercial applications, and to alter it and redistribute it
// freely, subject to the following restrictions:
//
// 1. The origin of this software must not be misrepresented; you must not
//   claim that you wrote the original software. If you use this software
//   in a product, an acknowledgment in the product documentation would be
//   appreciated but is not required.
// 2. Altered source versions must be plainly marked as such, and must not be
//   misrepresented as being the original software.
// 3. This notice may not be removed or altered from any source distribution.
// See state defs from inflate.js
const $1d314fc79f930156$var$BAD$1 = 16209; /* got a data error -- remain here until reset */ 
const $1d314fc79f930156$var$TYPE$1 = 16191; /* i: waiting for type bits, including last-flag bit */ 
/*
   Decode literal, length, and distance codes and write out the resulting
   literal and match bytes until either not enough input or output is
   available, an end-of-block is encountered, or a data error is encountered.
   When large enough input and output buffers are supplied to inflate(), for
   example, a 16K input buffer and a 64K output buffer, more than 95% of the
   inflate execution time is spent in this routine.

   Entry assumptions:

        state.mode === LEN
        strm.avail_in >= 6
        strm.avail_out >= 258
        start >= strm.avail_out
        state.bits < 8

   On return, state.mode is one of:

        LEN -- ran out of enough output space or enough available input
        TYPE -- reached end of block code, inflate() to interpret next block
        BAD -- error in block data

   Notes:

    - The maximum input bits used by a length/distance pair is 15 bits for the
      length code, 5 bits for the length extra, 15 bits for the distance code,
      and 13 bits for the distance extra.  This totals 48 bits, or six bytes.
      Therefore if strm.avail_in >= 6, then there is enough input to avoid
      checking for available input while decoding.

    - The maximum bytes that a single length/distance pair can output is 258
      bytes, which is the maximum length that can be coded.  inflate_fast()
      requires strm.avail_out >= 258 for each loop to avoid checking for
      output space.
 */ var $1d314fc79f930156$var$inffast = function inflate_fast(strm, start) {
    let _in; /* local strm.input */ 
    let last; /* have enough input while in < last */ 
    let _out; /* local strm.output */ 
    let beg; /* inflate()'s initial strm.output */ 
    let end; /* while out < end, enough space available */ 
    //#ifdef INFLATE_STRICT
    let dmax; /* maximum distance from zlib header */ 
    //#endif
    let wsize; /* window size or zero if not using window */ 
    let whave; /* valid bytes in the window */ 
    let wnext; /* window write index */ 
    // Use `s_window` instead `window`, avoid conflict with instrumentation tools
    let s_window; /* allocated sliding window, if wsize != 0 */ 
    let hold; /* local strm.hold */ 
    let bits; /* local strm.bits */ 
    let lcode; /* local strm.lencode */ 
    let dcode; /* local strm.distcode */ 
    let lmask; /* mask for first level of length codes */ 
    let dmask; /* mask for first level of distance codes */ 
    let here; /* retrieved table entry */ 
    let op; /* code bits, operation, extra bits, or */ 
    /*  window position, window bytes to copy */ let len; /* match length, unused bytes */ 
    let dist; /* match distance */ 
    let from; /* where to copy match from */ 
    let from_source;
    let input, output; // JS specific, because we have no pointers
    /* copy state to local variables */ const state = strm.state;
    //here = state.here;
    _in = strm.next_in;
    input = strm.input;
    last = _in + (strm.avail_in - 5);
    _out = strm.next_out;
    output = strm.output;
    beg = _out - (start - strm.avail_out);
    end = _out + (strm.avail_out - 257);
    //#ifdef INFLATE_STRICT
    dmax = state.dmax;
    //#endif
    wsize = state.wsize;
    whave = state.whave;
    wnext = state.wnext;
    s_window = state.window;
    hold = state.hold;
    bits = state.bits;
    lcode = state.lencode;
    dcode = state.distcode;
    lmask = (1 << state.lenbits) - 1;
    dmask = (1 << state.distbits) - 1;
    /* decode literals and length/distances until end-of-block or not enough
     input data or output space */ top: do {
        if (bits < 15) {
            hold += input[_in++] << bits;
            bits += 8;
            hold += input[_in++] << bits;
            bits += 8;
        }
        here = lcode[hold & lmask];
        dolen: for(;;){
            op = here >>> 24 /*here.bits*/ ;
            hold >>>= op;
            bits -= op;
            op = here >>> 16 & 0xff /*here.op*/ ;
            if (op === 0) //Tracevv((stderr, here.val >= 0x20 && here.val < 0x7f ?
            //        "inflate:         literal '%c'\n" :
            //        "inflate:         literal 0x%02x\n", here.val));
            output[_out++] = here & 0xffff /*here.val*/ ;
            else if (op & 16) {
                len = here & 0xffff /*here.val*/ ;
                op &= 15; /* number of extra bits */ 
                if (op) {
                    if (bits < op) {
                        hold += input[_in++] << bits;
                        bits += 8;
                    }
                    len += hold & (1 << op) - 1;
                    hold >>>= op;
                    bits -= op;
                }
                //Tracevv((stderr, "inflate:         length %u\n", len));
                if (bits < 15) {
                    hold += input[_in++] << bits;
                    bits += 8;
                    hold += input[_in++] << bits;
                    bits += 8;
                }
                here = dcode[hold & dmask];
                dodist: for(;;){
                    op = here >>> 24 /*here.bits*/ ;
                    hold >>>= op;
                    bits -= op;
                    op = here >>> 16 & 0xff /*here.op*/ ;
                    if (op & 16) {
                        dist = here & 0xffff /*here.val*/ ;
                        op &= 15; /* number of extra bits */ 
                        if (bits < op) {
                            hold += input[_in++] << bits;
                            bits += 8;
                            if (bits < op) {
                                hold += input[_in++] << bits;
                                bits += 8;
                            }
                        }
                        dist += hold & (1 << op) - 1;
                        //#ifdef INFLATE_STRICT
                        if (dist > dmax) {
                            strm.msg = "invalid distance too far back";
                            state.mode = $1d314fc79f930156$var$BAD$1;
                            break top;
                        }
                        //#endif
                        hold >>>= op;
                        bits -= op;
                        //Tracevv((stderr, "inflate:         distance %u\n", dist));
                        op = _out - beg; /* max distance in output */ 
                        if (dist > op) {
                            op = dist - op; /* distance back in window */ 
                            if (op > whave) {
                                if (state.sane) {
                                    strm.msg = "invalid distance too far back";
                                    state.mode = $1d314fc79f930156$var$BAD$1;
                                    break top;
                                }
                            }
                            from = 0; // window index
                            from_source = s_window;
                            if (wnext === 0) {
                                from += wsize - op;
                                if (op < len) {
                                    len -= op;
                                    do output[_out++] = s_window[from++];
                                    while (--op);
                                    from = _out - dist; /* rest from output */ 
                                    from_source = output;
                                }
                            } else if (wnext < op) {
                                from += wsize + wnext - op;
                                op -= wnext;
                                if (op < len) {
                                    len -= op;
                                    do output[_out++] = s_window[from++];
                                    while (--op);
                                    from = 0;
                                    if (wnext < len) {
                                        op = wnext;
                                        len -= op;
                                        do output[_out++] = s_window[from++];
                                        while (--op);
                                        from = _out - dist; /* rest from output */ 
                                        from_source = output;
                                    }
                                }
                            } else {
                                from += wnext - op;
                                if (op < len) {
                                    len -= op;
                                    do output[_out++] = s_window[from++];
                                    while (--op);
                                    from = _out - dist; /* rest from output */ 
                                    from_source = output;
                                }
                            }
                            while(len > 2){
                                output[_out++] = from_source[from++];
                                output[_out++] = from_source[from++];
                                output[_out++] = from_source[from++];
                                len -= 3;
                            }
                            if (len) {
                                output[_out++] = from_source[from++];
                                if (len > 1) output[_out++] = from_source[from++];
                            }
                        } else {
                            from = _out - dist; /* copy direct from output */ 
                            do {
                                output[_out++] = output[from++];
                                output[_out++] = output[from++];
                                output[_out++] = output[from++];
                                len -= 3;
                            }while (len > 2);
                            if (len) {
                                output[_out++] = output[from++];
                                if (len > 1) output[_out++] = output[from++];
                            }
                        }
                    } else if ((op & 64) === 0) {
                        here = dcode[(here & 0xffff) + (hold & (1 << op) - 1)];
                        continue dodist;
                    } else {
                        strm.msg = "invalid distance code";
                        state.mode = $1d314fc79f930156$var$BAD$1;
                        break top;
                    }
                    break; // need to emulate goto via "continue"
                }
            } else if ((op & 64) === 0) {
                here = lcode[(here & 0xffff) + (hold & (1 << op) - 1)];
                continue dolen;
            } else if (op & 32) {
                //Tracevv((stderr, "inflate:         end of block\n"));
                state.mode = $1d314fc79f930156$var$TYPE$1;
                break top;
            } else {
                strm.msg = "invalid literal/length code";
                state.mode = $1d314fc79f930156$var$BAD$1;
                break top;
            }
            break; // need to emulate goto via "continue"
        }
    }while (_in < last && _out < end);
    /* return unused bytes (on entry, bits < 8, so in won't go too far back) */ len = bits >> 3;
    _in -= len;
    bits -= len << 3;
    hold &= (1 << bits) - 1;
    /* update state and return */ strm.next_in = _in;
    strm.next_out = _out;
    strm.avail_in = _in < last ? 5 + (last - _in) : 5 - (_in - last);
    strm.avail_out = _out < end ? 257 + (end - _out) : 257 - (_out - end);
    state.hold = hold;
    state.bits = bits;
    return;
};
// (C) 1995-2013 Jean-loup Gailly and Mark Adler
// (C) 2014-2017 Vitaly Puzrin and Andrey Tupitsin
//
// This software is provided 'as-is', without any express or implied
// warranty. In no event will the authors be held liable for any damages
// arising from the use of this software.
//
// Permission is granted to anyone to use this software for any purpose,
// including commercial applications, and to alter it and redistribute it
// freely, subject to the following restrictions:
//
// 1. The origin of this software must not be misrepresented; you must not
//   claim that you wrote the original software. If you use this software
//   in a product, an acknowledgment in the product documentation would be
//   appreciated but is not required.
// 2. Altered source versions must be plainly marked as such, and must not be
//   misrepresented as being the original software.
// 3. This notice may not be removed or altered from any source distribution.
const $1d314fc79f930156$var$MAXBITS = 15;
const $1d314fc79f930156$var$ENOUGH_LENS$1 = 852;
const $1d314fc79f930156$var$ENOUGH_DISTS$1 = 592;
//const ENOUGH = (ENOUGH_LENS+ENOUGH_DISTS);
const $1d314fc79f930156$var$CODES$1 = 0;
const $1d314fc79f930156$var$LENS$1 = 1;
const $1d314fc79f930156$var$DISTS$1 = 2;
const $1d314fc79f930156$var$lbase = new Uint16Array([
    /* Length codes 257..285 base */ 3,
    4,
    5,
    6,
    7,
    8,
    9,
    10,
    11,
    13,
    15,
    17,
    19,
    23,
    27,
    31,
    35,
    43,
    51,
    59,
    67,
    83,
    99,
    115,
    131,
    163,
    195,
    227,
    258,
    0,
    0
]);
const $1d314fc79f930156$var$lext = new Uint8Array([
    /* Length codes 257..285 extra */ 16,
    16,
    16,
    16,
    16,
    16,
    16,
    16,
    17,
    17,
    17,
    17,
    18,
    18,
    18,
    18,
    19,
    19,
    19,
    19,
    20,
    20,
    20,
    20,
    21,
    21,
    21,
    21,
    16,
    72,
    78
]);
const $1d314fc79f930156$var$dbase = new Uint16Array([
    /* Distance codes 0..29 base */ 1,
    2,
    3,
    4,
    5,
    7,
    9,
    13,
    17,
    25,
    33,
    49,
    65,
    97,
    129,
    193,
    257,
    385,
    513,
    769,
    1025,
    1537,
    2049,
    3073,
    4097,
    6145,
    8193,
    12289,
    16385,
    24577,
    0,
    0
]);
const $1d314fc79f930156$var$dext = new Uint8Array([
    /* Distance codes 0..29 extra */ 16,
    16,
    16,
    16,
    17,
    17,
    18,
    18,
    19,
    19,
    20,
    20,
    21,
    21,
    22,
    22,
    23,
    23,
    24,
    24,
    25,
    25,
    26,
    26,
    27,
    27,
    28,
    28,
    29,
    29,
    64,
    64
]);
const $1d314fc79f930156$var$inflate_table = (type, lens, lens_index, codes, table, table_index, work, opts)=>{
    const bits = opts.bits;
    //here = opts.here; /* table entry for duplication */
    let len = 0; /* a code's length in bits */ 
    let sym = 0; /* index of code symbols */ 
    let min = 0, max = 0; /* minimum and maximum code lengths */ 
    let root = 0; /* number of index bits for root table */ 
    let curr = 0; /* number of index bits for current table */ 
    let drop = 0; /* code bits to drop for sub-table */ 
    let left = 0; /* number of prefix codes available */ 
    let used = 0; /* code entries in table used */ 
    let huff = 0; /* Huffman code */ 
    let incr; /* for incrementing code, index */ 
    let fill; /* index for replicating entries */ 
    let low; /* low bits for current root entry */ 
    let mask; /* mask for low root bits */ 
    let next; /* next available space in table */ 
    let base = null; /* base value table to use */ 
    //  let shoextra;    /* extra bits table to use */
    let match; /* use base and extra for symbol >= match */ 
    const count = new Uint16Array($1d314fc79f930156$var$MAXBITS + 1); //[MAXBITS+1];    /* number of codes of each length */
    const offs = new Uint16Array($1d314fc79f930156$var$MAXBITS + 1); //[MAXBITS+1];     /* offsets in table for each length */
    let extra = null;
    let here_bits, here_op, here_val;
    /*
   Process a set of code lengths to create a canonical Huffman code.  The
   code lengths are lens[0..codes-1].  Each length corresponds to the
   symbols 0..codes-1.  The Huffman code is generated by first sorting the
   symbols by length from short to long, and retaining the symbol order
   for codes with equal lengths.  Then the code starts with all zero bits
   for the first code of the shortest length, and the codes are integer
   increments for the same length, and zeros are appended as the length
   increases.  For the deflate format, these bits are stored backwards
   from their more natural integer increment ordering, and so when the
   decoding tables are built in the large loop below, the integer codes
   are incremented backwards.

   This routine assumes, but does not check, that all of the entries in
   lens[] are in the range 0..MAXBITS.  The caller must assure this.
   1..MAXBITS is interpreted as that code length.  zero means that that
   symbol does not occur in this code.

   The codes are sorted by computing a count of codes for each length,
   creating from that a table of starting indices for each length in the
   sorted table, and then entering the symbols in order in the sorted
   table.  The sorted table is work[], with that space being provided by
   the caller.

   The length counts are used for other purposes as well, i.e. finding
   the minimum and maximum length codes, determining if there are any
   codes at all, checking for a valid set of lengths, and looking ahead
   at length counts to determine sub-table sizes when building the
   decoding tables.
   */ /* accumulate lengths for codes (assumes lens[] all in 0..MAXBITS) */ for(len = 0; len <= $1d314fc79f930156$var$MAXBITS; len++)count[len] = 0;
    for(sym = 0; sym < codes; sym++)count[lens[lens_index + sym]]++;
    /* bound code lengths, force root to be within code lengths */ root = bits;
    for(max = $1d314fc79f930156$var$MAXBITS; max >= 1; max--){
        if (count[max] !== 0) break;
    }
    if (root > max) root = max;
    if (max === 0) {
        //table.op[opts.table_index] = 64;  //here.op = (var char)64;    /* invalid code marker */
        //table.bits[opts.table_index] = 1;   //here.bits = (var char)1;
        //table.val[opts.table_index++] = 0;   //here.val = (var short)0;
        table[table_index++] = 20971520;
        //table.op[opts.table_index] = 64;
        //table.bits[opts.table_index] = 1;
        //table.val[opts.table_index++] = 0;
        table[table_index++] = 20971520;
        opts.bits = 1;
        return 0; /* no symbols, but wait for decoding to report error */ 
    }
    for(min = 1; min < max; min++){
        if (count[min] !== 0) break;
    }
    if (root < min) root = min;
    /* check for an over-subscribed or incomplete set of lengths */ left = 1;
    for(len = 1; len <= $1d314fc79f930156$var$MAXBITS; len++){
        left <<= 1;
        left -= count[len];
        if (left < 0) return -1;
         /* over-subscribed */ 
    }
    if (left > 0 && (type === $1d314fc79f930156$var$CODES$1 || max !== 1)) return -1; /* incomplete set */ 
    /* generate offsets into symbol table for each length for sorting */ offs[1] = 0;
    for(len = 1; len < $1d314fc79f930156$var$MAXBITS; len++)offs[len + 1] = offs[len] + count[len];
    /* sort symbols by length, by symbol order within each length */ for(sym = 0; sym < codes; sym++)if (lens[lens_index + sym] !== 0) work[offs[lens[lens_index + sym]]++] = sym;
    /*
   Create and fill in decoding tables.  In this loop, the table being
   filled is at next and has curr index bits.  The code being used is huff
   with length len.  That code is converted to an index by dropping drop
   bits off of the bottom.  For codes where len is less than drop + curr,
   those top drop + curr - len bits are incremented through all values to
   fill the table with replicated entries.

   root is the number of index bits for the root table.  When len exceeds
   root, sub-tables are created pointed to by the root entry with an index
   of the low root bits of huff.  This is saved in low to check for when a
   new sub-table should be started.  drop is zero when the root table is
   being filled, and drop is root when sub-tables are being filled.

   When a new sub-table is needed, it is necessary to look ahead in the
   code lengths to determine what size sub-table is needed.  The length
   counts are used for this, and so count[] is decremented as codes are
   entered in the tables.

   used keeps track of how many table entries have been allocated from the
   provided *table space.  It is checked for LENS and DIST tables against
   the constants ENOUGH_LENS and ENOUGH_DISTS to guard against changes in
   the initial root table size constants.  See the comments in inftrees.h
   for more information.

   sym increments through all symbols, and the loop terminates when
   all codes of length max, i.e. all codes, have been processed.  This
   routine permits incomplete codes, so another loop after this one fills
   in the rest of the decoding tables with invalid code markers.
   */ /* set up for code type */ // poor man optimization - use if-else instead of switch,
    // to avoid deopts in old v8
    if (type === $1d314fc79f930156$var$CODES$1) {
        base = extra = work; /* dummy value--not used */ 
        match = 20;
    } else if (type === $1d314fc79f930156$var$LENS$1) {
        base = $1d314fc79f930156$var$lbase;
        extra = $1d314fc79f930156$var$lext;
        match = 257;
    } else {
        base = $1d314fc79f930156$var$dbase;
        extra = $1d314fc79f930156$var$dext;
        match = 0;
    }
    /* initialize opts for loop */ huff = 0; /* starting code */ 
    sym = 0; /* starting code symbol */ 
    len = min; /* starting code length */ 
    next = table_index; /* current table to fill in */ 
    curr = root; /* current table index bits */ 
    drop = 0; /* current bits to drop from code for index */ 
    low = -1; /* trigger new sub-table when len > root */ 
    used = 1 << root; /* use root table entries */ 
    mask = used - 1; /* mask for comparing low */ 
    /* check available table space */ if (type === $1d314fc79f930156$var$LENS$1 && used > $1d314fc79f930156$var$ENOUGH_LENS$1 || type === $1d314fc79f930156$var$DISTS$1 && used > $1d314fc79f930156$var$ENOUGH_DISTS$1) return 1;
    /* process all codes and make table entries */ for(;;){
        /* create table entry */ here_bits = len - drop;
        if (work[sym] + 1 < match) {
            here_op = 0;
            here_val = work[sym];
        } else if (work[sym] >= match) {
            here_op = extra[work[sym] - match];
            here_val = base[work[sym] - match];
        } else {
            here_op = 96; /* end of block */ 
            here_val = 0;
        }
        /* replicate for those indices with low len bits equal to huff */ incr = 1 << len - drop;
        fill = 1 << curr;
        min = fill; /* save offset to next table */ 
        do {
            fill -= incr;
            table[next + (huff >> drop) + fill] = here_bits << 24 | here_op << 16 | here_val | 0;
        }while (fill !== 0);
        /* backwards increment the len-bit code huff */ incr = 1 << len - 1;
        while(huff & incr)incr >>= 1;
        if (incr !== 0) {
            huff &= incr - 1;
            huff += incr;
        } else huff = 0;
        /* go to next symbol, update count, len */ sym++;
        if (--count[len] === 0) {
            if (len === max) break;
            len = lens[lens_index + work[sym]];
        }
        /* create new sub-table if needed */ if (len > root && (huff & mask) !== low) {
            /* if first time, transition to sub-tables */ if (drop === 0) drop = root;
            /* increment past last table */ next += min; /* here min is 1 << curr */ 
            /* determine length of next table */ curr = len - drop;
            left = 1 << curr;
            while(curr + drop < max){
                left -= count[curr + drop];
                if (left <= 0) break;
                curr++;
                left <<= 1;
            }
            /* check for enough space */ used += 1 << curr;
            if (type === $1d314fc79f930156$var$LENS$1 && used > $1d314fc79f930156$var$ENOUGH_LENS$1 || type === $1d314fc79f930156$var$DISTS$1 && used > $1d314fc79f930156$var$ENOUGH_DISTS$1) return 1;
            /* point entry in root table to sub-table */ low = huff & mask;
            /*table.op[low] = curr;
      table.bits[low] = root;
      table.val[low] = next - opts.table_index;*/ table[low] = root << 24 | curr << 16 | next - table_index | 0;
        }
    }
    /* fill in remaining table entry if code is incomplete (guaranteed to have
   at most one remaining entry, since if the code is incomplete, the
   maximum code length that was allowed to get this far is one bit) */ if (huff !== 0) //table.op[next + huff] = 64;            /* invalid code marker */
    //table.bits[next + huff] = len - drop;
    //table.val[next + huff] = 0;
    table[next + huff] = len - drop << 24 | 4194304;
    /* set return parameters */ //opts.table_index += used;
    opts.bits = root;
    return 0;
};
var $1d314fc79f930156$var$inftrees = $1d314fc79f930156$var$inflate_table;
// (C) 1995-2013 Jean-loup Gailly and Mark Adler
// (C) 2014-2017 Vitaly Puzrin and Andrey Tupitsin
//
// This software is provided 'as-is', without any express or implied
// warranty. In no event will the authors be held liable for any damages
// arising from the use of this software.
//
// Permission is granted to anyone to use this software for any purpose,
// including commercial applications, and to alter it and redistribute it
// freely, subject to the following restrictions:
//
// 1. The origin of this software must not be misrepresented; you must not
//   claim that you wrote the original software. If you use this software
//   in a product, an acknowledgment in the product documentation would be
//   appreciated but is not required.
// 2. Altered source versions must be plainly marked as such, and must not be
//   misrepresented as being the original software.
// 3. This notice may not be removed or altered from any source distribution.
const $1d314fc79f930156$var$CODES = 0;
const $1d314fc79f930156$var$LENS = 1;
const $1d314fc79f930156$var$DISTS = 2;
/* Public constants ==========================================================*/ /* ===========================================================================*/ const { Z_FINISH: $1d314fc79f930156$var$Z_FINISH$1, Z_BLOCK: $1d314fc79f930156$var$Z_BLOCK, Z_TREES: $1d314fc79f930156$var$Z_TREES, Z_OK: $1d314fc79f930156$var$Z_OK$1, Z_STREAM_END: $1d314fc79f930156$var$Z_STREAM_END$1, Z_NEED_DICT: $1d314fc79f930156$var$Z_NEED_DICT$1, Z_STREAM_ERROR: $1d314fc79f930156$var$Z_STREAM_ERROR$1, Z_DATA_ERROR: $1d314fc79f930156$var$Z_DATA_ERROR$1, Z_MEM_ERROR: $1d314fc79f930156$var$Z_MEM_ERROR$1, Z_BUF_ERROR: $1d314fc79f930156$var$Z_BUF_ERROR, Z_DEFLATED: $1d314fc79f930156$var$Z_DEFLATED } = $1d314fc79f930156$var$constants$2;
/* STATES ====================================================================*/ /* ===========================================================================*/ const $1d314fc79f930156$var$HEAD = 16180; /* i: waiting for magic header */ 
const $1d314fc79f930156$var$FLAGS = 16181; /* i: waiting for method and flags (gzip) */ 
const $1d314fc79f930156$var$TIME = 16182; /* i: waiting for modification time (gzip) */ 
const $1d314fc79f930156$var$OS = 16183; /* i: waiting for extra flags and operating system (gzip) */ 
const $1d314fc79f930156$var$EXLEN = 16184; /* i: waiting for extra length (gzip) */ 
const $1d314fc79f930156$var$EXTRA = 16185; /* i: waiting for extra bytes (gzip) */ 
const $1d314fc79f930156$var$NAME = 16186; /* i: waiting for end of file name (gzip) */ 
const $1d314fc79f930156$var$COMMENT = 16187; /* i: waiting for end of comment (gzip) */ 
const $1d314fc79f930156$var$HCRC = 16188; /* i: waiting for header crc (gzip) */ 
const $1d314fc79f930156$var$DICTID = 16189; /* i: waiting for dictionary check value */ 
const $1d314fc79f930156$var$DICT = 16190; /* waiting for inflateSetDictionary() call */ 
const $1d314fc79f930156$var$TYPE = 16191; /* i: waiting for type bits, including last-flag bit */ 
const $1d314fc79f930156$var$TYPEDO = 16192; /* i: same, but skip check to exit inflate on new block */ 
const $1d314fc79f930156$var$STORED = 16193; /* i: waiting for stored size (length and complement) */ 
const $1d314fc79f930156$var$COPY_ = 16194; /* i/o: same as COPY below, but only first time in */ 
const $1d314fc79f930156$var$COPY = 16195; /* i/o: waiting for input or output to copy stored block */ 
const $1d314fc79f930156$var$TABLE = 16196; /* i: waiting for dynamic block table lengths */ 
const $1d314fc79f930156$var$LENLENS = 16197; /* i: waiting for code length code lengths */ 
const $1d314fc79f930156$var$CODELENS = 16198; /* i: waiting for length/lit and distance code lengths */ 
const $1d314fc79f930156$var$LEN_ = 16199; /* i: same as LEN below, but only first time in */ 
const $1d314fc79f930156$var$LEN = 16200; /* i: waiting for length/lit/eob code */ 
const $1d314fc79f930156$var$LENEXT = 16201; /* i: waiting for length extra bits */ 
const $1d314fc79f930156$var$DIST = 16202; /* i: waiting for distance code */ 
const $1d314fc79f930156$var$DISTEXT = 16203; /* i: waiting for distance extra bits */ 
const $1d314fc79f930156$var$MATCH = 16204; /* o: waiting for output space to copy string */ 
const $1d314fc79f930156$var$LIT = 16205; /* o: waiting for output space to write literal */ 
const $1d314fc79f930156$var$CHECK = 16206; /* i: waiting for 32-bit check value */ 
const $1d314fc79f930156$var$LENGTH = 16207; /* i: waiting for 32-bit length (gzip) */ 
const $1d314fc79f930156$var$DONE = 16208; /* finished check, done -- remain here until reset */ 
const $1d314fc79f930156$var$BAD = 16209; /* got a data error -- remain here until reset */ 
const $1d314fc79f930156$var$MEM = 16210; /* got an inflate() memory error -- remain here until reset */ 
const $1d314fc79f930156$var$SYNC = 16211; /* looking for synchronization bytes to restart inflate() */ 
/* ===========================================================================*/ const $1d314fc79f930156$var$ENOUGH_LENS = 852;
const $1d314fc79f930156$var$ENOUGH_DISTS = 592;
//const ENOUGH =  (ENOUGH_LENS+ENOUGH_DISTS);
const $1d314fc79f930156$var$MAX_WBITS = 15;
/* 32K LZ77 window */ const $1d314fc79f930156$var$DEF_WBITS = $1d314fc79f930156$var$MAX_WBITS;
const $1d314fc79f930156$var$zswap32 = (q)=>{
    return (q >>> 24 & 0xff) + (q >>> 8 & 0xff00) + ((q & 0xff00) << 8) + ((q & 0xff) << 24);
};
function $1d314fc79f930156$var$InflateState() {
    this.strm = null; /* pointer back to this zlib stream */ 
    this.mode = 0; /* current inflate mode */ 
    this.last = false; /* true if processing last block */ 
    this.wrap = 0; /* bit 0 true for zlib, bit 1 true for gzip,
                                 bit 2 true to validate check value */ 
    this.havedict = false; /* true if dictionary provided */ 
    this.flags = 0; /* gzip header method and flags (0 if zlib), or
                                 -1 if raw or no header yet */ 
    this.dmax = 0; /* zlib header max distance (INFLATE_STRICT) */ 
    this.check = 0; /* protected copy of check value */ 
    this.total = 0; /* protected copy of output count */ 
    // TODO: may be {}
    this.head = null; /* where to save gzip header information */ 
    /* sliding window */ this.wbits = 0; /* log base 2 of requested window size */ 
    this.wsize = 0; /* window size or zero if not using window */ 
    this.whave = 0; /* valid bytes in the window */ 
    this.wnext = 0; /* window write index */ 
    this.window = null; /* allocated sliding window, if needed */ 
    /* bit accumulator */ this.hold = 0; /* input bit accumulator */ 
    this.bits = 0; /* number of bits in "in" */ 
    /* for string and stored block copying */ this.length = 0; /* literal or length of data to copy */ 
    this.offset = 0; /* distance back to copy string from */ 
    /* for table and code decoding */ this.extra = 0; /* extra bits needed */ 
    /* fixed and dynamic code tables */ this.lencode = null; /* starting table for length/literal codes */ 
    this.distcode = null; /* starting table for distance codes */ 
    this.lenbits = 0; /* index bits for lencode */ 
    this.distbits = 0; /* index bits for distcode */ 
    /* dynamic table building */ this.ncode = 0; /* number of code length code lengths */ 
    this.nlen = 0; /* number of length code lengths */ 
    this.ndist = 0; /* number of distance code lengths */ 
    this.have = 0; /* number of code lengths in lens[] */ 
    this.next = null; /* next available space in codes[] */ 
    this.lens = new Uint16Array(320); /* temporary storage for code lengths */ 
    this.work = new Uint16Array(288); /* work area for code table building */ 
    /*
   because we don't have pointers in js, we use lencode and distcode directly
   as buffers so we don't need codes
  */ //this.codes = new Int32Array(ENOUGH);       /* space for code tables */
    this.lendyn = null; /* dynamic table for length/literal codes (JS specific) */ 
    this.distdyn = null; /* dynamic table for distance codes (JS specific) */ 
    this.sane = 0; /* if false, allow invalid distance too far */ 
    this.back = 0; /* bits back of last unprocessed length/lit */ 
    this.was = 0; /* initial length of match */ 
}
const $1d314fc79f930156$var$inflateStateCheck = (strm)=>{
    if (!strm) return 1;
    const state = strm.state;
    if (!state || state.strm !== strm || state.mode < $1d314fc79f930156$var$HEAD || state.mode > $1d314fc79f930156$var$SYNC) return 1;
    return 0;
};
const $1d314fc79f930156$var$inflateResetKeep = (strm)=>{
    if ($1d314fc79f930156$var$inflateStateCheck(strm)) return $1d314fc79f930156$var$Z_STREAM_ERROR$1;
    const state = strm.state;
    strm.total_in = strm.total_out = state.total = 0;
    strm.msg = ""; /*Z_NULL*/ 
    if (state.wrap) strm.adler = state.wrap & 1;
    state.mode = $1d314fc79f930156$var$HEAD;
    state.last = 0;
    state.havedict = 0;
    state.flags = -1;
    state.dmax = 32768;
    state.head = null /*Z_NULL*/ ;
    state.hold = 0;
    state.bits = 0;
    //state.lencode = state.distcode = state.next = state.codes;
    state.lencode = state.lendyn = new Int32Array($1d314fc79f930156$var$ENOUGH_LENS);
    state.distcode = state.distdyn = new Int32Array($1d314fc79f930156$var$ENOUGH_DISTS);
    state.sane = 1;
    state.back = -1;
    //Tracev((stderr, "inflate: reset\n"));
    return $1d314fc79f930156$var$Z_OK$1;
};
const $1d314fc79f930156$var$inflateReset = (strm)=>{
    if ($1d314fc79f930156$var$inflateStateCheck(strm)) return $1d314fc79f930156$var$Z_STREAM_ERROR$1;
    const state = strm.state;
    state.wsize = 0;
    state.whave = 0;
    state.wnext = 0;
    return $1d314fc79f930156$var$inflateResetKeep(strm);
};
const $1d314fc79f930156$var$inflateReset2 = (strm, windowBits)=>{
    let wrap;
    /* get the state */ if ($1d314fc79f930156$var$inflateStateCheck(strm)) return $1d314fc79f930156$var$Z_STREAM_ERROR$1;
    const state = strm.state;
    /* extract wrap request from windowBits parameter */ if (windowBits < 0) {
        wrap = 0;
        windowBits = -windowBits;
    } else {
        wrap = (windowBits >> 4) + 5;
        if (windowBits < 48) windowBits &= 15;
    }
    /* set number of window bits, free window if different */ if (windowBits && (windowBits < 8 || windowBits > 15)) return $1d314fc79f930156$var$Z_STREAM_ERROR$1;
    if (state.window !== null && state.wbits !== windowBits) state.window = null;
    /* update state and reset the rest of it */ state.wrap = wrap;
    state.wbits = windowBits;
    return $1d314fc79f930156$var$inflateReset(strm);
};
const $1d314fc79f930156$var$inflateInit2 = (strm, windowBits)=>{
    if (!strm) return $1d314fc79f930156$var$Z_STREAM_ERROR$1;
    //strm.msg = Z_NULL;                 /* in case we return an error */
    const state = new $1d314fc79f930156$var$InflateState();
    //if (state === Z_NULL) return Z_MEM_ERROR;
    //Tracev((stderr, "inflate: allocated\n"));
    strm.state = state;
    state.strm = strm;
    state.window = null /*Z_NULL*/ ;
    state.mode = $1d314fc79f930156$var$HEAD; /* to pass state test in inflateReset2() */ 
    const ret = $1d314fc79f930156$var$inflateReset2(strm, windowBits);
    if (ret !== $1d314fc79f930156$var$Z_OK$1) strm.state = null /*Z_NULL*/ ;
    return ret;
};
const $1d314fc79f930156$var$inflateInit = (strm)=>{
    return $1d314fc79f930156$var$inflateInit2(strm, $1d314fc79f930156$var$DEF_WBITS);
};
/*
 Return state with length and distance decoding tables and index sizes set to
 fixed code decoding.  Normally this returns fixed tables from inffixed.h.
 If BUILDFIXED is defined, then instead this routine builds the tables the
 first time it's called, and returns those tables the first time and
 thereafter.  This reduces the size of the code by about 2K bytes, in
 exchange for a little execution time.  However, BUILDFIXED should not be
 used for threaded applications, since the rewriting of the tables and virgin
 may not be thread-safe.
 */ let $1d314fc79f930156$var$virgin = true;
let $1d314fc79f930156$var$lenfix, $1d314fc79f930156$var$distfix; // We have no pointers in JS, so keep tables separate
const $1d314fc79f930156$var$fixedtables = (state)=>{
    /* build fixed huffman tables if first call (may not be thread safe) */ if ($1d314fc79f930156$var$virgin) {
        $1d314fc79f930156$var$lenfix = new Int32Array(512);
        $1d314fc79f930156$var$distfix = new Int32Array(32);
        /* literal/length table */ let sym = 0;
        while(sym < 144)state.lens[sym++] = 8;
        while(sym < 256)state.lens[sym++] = 9;
        while(sym < 280)state.lens[sym++] = 7;
        while(sym < 288)state.lens[sym++] = 8;
        $1d314fc79f930156$var$inftrees($1d314fc79f930156$var$LENS, state.lens, 0, 288, $1d314fc79f930156$var$lenfix, 0, state.work, {
            bits: 9
        });
        /* distance table */ sym = 0;
        while(sym < 32)state.lens[sym++] = 5;
        $1d314fc79f930156$var$inftrees($1d314fc79f930156$var$DISTS, state.lens, 0, 32, $1d314fc79f930156$var$distfix, 0, state.work, {
            bits: 5
        });
        /* do this just once */ $1d314fc79f930156$var$virgin = false;
    }
    state.lencode = $1d314fc79f930156$var$lenfix;
    state.lenbits = 9;
    state.distcode = $1d314fc79f930156$var$distfix;
    state.distbits = 5;
};
/*
 Update the window with the last wsize (normally 32K) bytes written before
 returning.  If window does not exist yet, create it.  This is only called
 when a window is already in use, or when output has been written during this
 inflate call, but the end of the deflate stream has not been reached yet.
 It is also called to create a window for dictionary data when a dictionary
 is loaded.

 Providing output buffers larger than 32K to inflate() should provide a speed
 advantage, since only the last 32K of output is copied to the sliding window
 upon return from inflate(), and since all distances after the first 32K of
 output will fall in the output data, making match copies simpler and faster.
 The advantage may be dependent on the size of the processor's data caches.
 */ const $1d314fc79f930156$var$updatewindow = (strm, src, end, copy)=>{
    let dist;
    const state = strm.state;
    /* if it hasn't been done already, allocate space for the window */ if (state.window === null) {
        state.wsize = 1 << state.wbits;
        state.wnext = 0;
        state.whave = 0;
        state.window = new Uint8Array(state.wsize);
    }
    /* copy state->wsize or less output bytes into the circular window */ if (copy >= state.wsize) {
        state.window.set(src.subarray(end - state.wsize, end), 0);
        state.wnext = 0;
        state.whave = state.wsize;
    } else {
        dist = state.wsize - state.wnext;
        if (dist > copy) dist = copy;
        //zmemcpy(state->window + state->wnext, end - copy, dist);
        state.window.set(src.subarray(end - copy, end - copy + dist), state.wnext);
        copy -= dist;
        if (copy) {
            //zmemcpy(state->window, end - copy, copy);
            state.window.set(src.subarray(end - copy, end), 0);
            state.wnext = copy;
            state.whave = state.wsize;
        } else {
            state.wnext += dist;
            if (state.wnext === state.wsize) state.wnext = 0;
            if (state.whave < state.wsize) state.whave += dist;
        }
    }
    return 0;
};
const $1d314fc79f930156$var$inflate$2 = (strm, flush)=>{
    let state;
    let input, output; // input/output buffers
    let next; /* next input INDEX */ 
    let put; /* next output INDEX */ 
    let have, left; /* available input and output */ 
    let hold; /* bit buffer */ 
    let bits; /* bits in bit buffer */ 
    let _in, _out; /* save starting available input and output */ 
    let copy; /* number of stored or match bytes to copy */ 
    let from; /* where to copy match bytes from */ 
    let from_source;
    let here = 0; /* current decoding table entry */ 
    let here_bits, here_op, here_val; // paked "here" denormalized (JS specific)
    //let last;                   /* parent table entry */
    let last_bits, last_op, last_val; // paked "last" denormalized (JS specific)
    let len; /* length to copy for repeats, bits to drop */ 
    let ret; /* return code */ 
    const hbuf = new Uint8Array(4); /* buffer for gzip header crc calculation */ 
    let opts;
    let n; // temporary variable for NEED_BITS
    const order = /* permutation of code lengths */ new Uint8Array([
        16,
        17,
        18,
        0,
        8,
        7,
        9,
        6,
        10,
        5,
        11,
        4,
        12,
        3,
        13,
        2,
        14,
        1,
        15
    ]);
    if ($1d314fc79f930156$var$inflateStateCheck(strm) || !strm.output || !strm.input && strm.avail_in !== 0) return $1d314fc79f930156$var$Z_STREAM_ERROR$1;
    state = strm.state;
    if (state.mode === $1d314fc79f930156$var$TYPE) state.mode = $1d314fc79f930156$var$TYPEDO;
     /* skip check */ 
    //--- LOAD() ---
    put = strm.next_out;
    output = strm.output;
    left = strm.avail_out;
    next = strm.next_in;
    input = strm.input;
    have = strm.avail_in;
    hold = state.hold;
    bits = state.bits;
    //---
    _in = have;
    _out = left;
    ret = $1d314fc79f930156$var$Z_OK$1;
    inf_leave: for(;;)switch(state.mode){
        case $1d314fc79f930156$var$HEAD:
            if (state.wrap === 0) {
                state.mode = $1d314fc79f930156$var$TYPEDO;
                break;
            }
            //=== NEEDBITS(16);
            while(bits < 16){
                if (have === 0) break inf_leave;
                have--;
                hold += input[next++] << bits;
                bits += 8;
            }
            //===//
            if (state.wrap & 2 && hold === 0x8b1f) {
                if (state.wbits === 0) state.wbits = 15;
                state.check = 0 /*crc32(0L, Z_NULL, 0)*/ ;
                //=== CRC2(state.check, hold);
                hbuf[0] = hold & 0xff;
                hbuf[1] = hold >>> 8 & 0xff;
                state.check = $1d314fc79f930156$var$crc32_1(state.check, hbuf, 2, 0);
                //===//
                //=== INITBITS();
                hold = 0;
                bits = 0;
                //===//
                state.mode = $1d314fc79f930156$var$FLAGS;
                break;
            }
            if (state.head) state.head.done = false;
            if (!(state.wrap & 1) || /* check if zlib header allowed */ (((hold & 0xff) << 8) + (hold >> 8)) % 31) {
                strm.msg = "incorrect header check";
                state.mode = $1d314fc79f930156$var$BAD;
                break;
            }
            if ((hold & 0x0f) !== $1d314fc79f930156$var$Z_DEFLATED) {
                strm.msg = "unknown compression method";
                state.mode = $1d314fc79f930156$var$BAD;
                break;
            }
            //--- DROPBITS(4) ---//
            hold >>>= 4;
            bits -= 4;
            //---//
            len = (hold & 0x0f) + 8;
            if (state.wbits === 0) state.wbits = len;
            if (len > 15 || len > state.wbits) {
                strm.msg = "invalid window size";
                state.mode = $1d314fc79f930156$var$BAD;
                break;
            }
            // !!! pako patch. Force use `options.windowBits` if passed.
            // Required to always use max window size by default.
            state.dmax = 1 << state.wbits;
            //state.dmax = 1 << len;
            state.flags = 0; /* indicate zlib header */ 
            //Tracev((stderr, "inflate:   zlib header ok\n"));
            strm.adler = state.check = 1 /*adler32(0L, Z_NULL, 0)*/ ;
            state.mode = hold & 0x200 ? $1d314fc79f930156$var$DICTID : $1d314fc79f930156$var$TYPE;
            //=== INITBITS();
            hold = 0;
            bits = 0;
            break;
        case $1d314fc79f930156$var$FLAGS:
            //=== NEEDBITS(16); */
            while(bits < 16){
                if (have === 0) break inf_leave;
                have--;
                hold += input[next++] << bits;
                bits += 8;
            }
            //===//
            state.flags = hold;
            if ((state.flags & 0xff) !== $1d314fc79f930156$var$Z_DEFLATED) {
                strm.msg = "unknown compression method";
                state.mode = $1d314fc79f930156$var$BAD;
                break;
            }
            if (state.flags & 0xe000) {
                strm.msg = "unknown header flags set";
                state.mode = $1d314fc79f930156$var$BAD;
                break;
            }
            if (state.head) state.head.text = hold >> 8 & 1;
            if (state.flags & 0x0200 && state.wrap & 4) {
                //=== CRC2(state.check, hold);
                hbuf[0] = hold & 0xff;
                hbuf[1] = hold >>> 8 & 0xff;
                state.check = $1d314fc79f930156$var$crc32_1(state.check, hbuf, 2, 0);
            //===//
            }
            //=== INITBITS();
            hold = 0;
            bits = 0;
            //===//
            state.mode = $1d314fc79f930156$var$TIME;
        /* falls through */ case $1d314fc79f930156$var$TIME:
            //=== NEEDBITS(32); */
            while(bits < 32){
                if (have === 0) break inf_leave;
                have--;
                hold += input[next++] << bits;
                bits += 8;
            }
            //===//
            if (state.head) state.head.time = hold;
            if (state.flags & 0x0200 && state.wrap & 4) {
                //=== CRC4(state.check, hold)
                hbuf[0] = hold & 0xff;
                hbuf[1] = hold >>> 8 & 0xff;
                hbuf[2] = hold >>> 16 & 0xff;
                hbuf[3] = hold >>> 24 & 0xff;
                state.check = $1d314fc79f930156$var$crc32_1(state.check, hbuf, 4, 0);
            //===
            }
            //=== INITBITS();
            hold = 0;
            bits = 0;
            //===//
            state.mode = $1d314fc79f930156$var$OS;
        /* falls through */ case $1d314fc79f930156$var$OS:
            //=== NEEDBITS(16); */
            while(bits < 16){
                if (have === 0) break inf_leave;
                have--;
                hold += input[next++] << bits;
                bits += 8;
            }
            //===//
            if (state.head) {
                state.head.xflags = hold & 0xff;
                state.head.os = hold >> 8;
            }
            if (state.flags & 0x0200 && state.wrap & 4) {
                //=== CRC2(state.check, hold);
                hbuf[0] = hold & 0xff;
                hbuf[1] = hold >>> 8 & 0xff;
                state.check = $1d314fc79f930156$var$crc32_1(state.check, hbuf, 2, 0);
            //===//
            }
            //=== INITBITS();
            hold = 0;
            bits = 0;
            //===//
            state.mode = $1d314fc79f930156$var$EXLEN;
        /* falls through */ case $1d314fc79f930156$var$EXLEN:
            if (state.flags & 0x0400) {
                //=== NEEDBITS(16); */
                while(bits < 16){
                    if (have === 0) break inf_leave;
                    have--;
                    hold += input[next++] << bits;
                    bits += 8;
                }
                //===//
                state.length = hold;
                if (state.head) state.head.extra_len = hold;
                if (state.flags & 0x0200 && state.wrap & 4) {
                    //=== CRC2(state.check, hold);
                    hbuf[0] = hold & 0xff;
                    hbuf[1] = hold >>> 8 & 0xff;
                    state.check = $1d314fc79f930156$var$crc32_1(state.check, hbuf, 2, 0);
                //===//
                }
                //=== INITBITS();
                hold = 0;
                bits = 0;
            //===//
            } else if (state.head) state.head.extra = null /*Z_NULL*/ ;
            state.mode = $1d314fc79f930156$var$EXTRA;
        /* falls through */ case $1d314fc79f930156$var$EXTRA:
            if (state.flags & 0x0400) {
                copy = state.length;
                if (copy > have) copy = have;
                if (copy) {
                    if (state.head) {
                        len = state.head.extra_len - state.length;
                        if (!state.head.extra) // Use untyped array for more convenient processing later
                        state.head.extra = new Uint8Array(state.head.extra_len);
                        state.head.extra.set(input.subarray(next, // extra field is limited to 65536 bytes
                        // - no need for additional size check
                        next + copy), /*len + copy > state.head.extra_max - len ? state.head.extra_max : copy,*/ len);
                    //zmemcpy(state.head.extra + len, next,
                    //        len + copy > state.head.extra_max ?
                    //        state.head.extra_max - len : copy);
                    }
                    if (state.flags & 0x0200 && state.wrap & 4) state.check = $1d314fc79f930156$var$crc32_1(state.check, input, copy, next);
                    have -= copy;
                    next += copy;
                    state.length -= copy;
                }
                if (state.length) break inf_leave;
            }
            state.length = 0;
            state.mode = $1d314fc79f930156$var$NAME;
        /* falls through */ case $1d314fc79f930156$var$NAME:
            if (state.flags & 0x0800) {
                if (have === 0) break inf_leave;
                copy = 0;
                do {
                    // TODO: 2 or 1 bytes?
                    len = input[next + copy++];
                    /* use constant limit because in js we should not preallocate memory */ if (state.head && len && state.length < 65536 /*state.head.name_max*/ ) state.head.name += String.fromCharCode(len);
                }while (len && copy < have);
                if (state.flags & 0x0200 && state.wrap & 4) state.check = $1d314fc79f930156$var$crc32_1(state.check, input, copy, next);
                have -= copy;
                next += copy;
                if (len) break inf_leave;
            } else if (state.head) state.head.name = null;
            state.length = 0;
            state.mode = $1d314fc79f930156$var$COMMENT;
        /* falls through */ case $1d314fc79f930156$var$COMMENT:
            if (state.flags & 0x1000) {
                if (have === 0) break inf_leave;
                copy = 0;
                do {
                    len = input[next + copy++];
                    /* use constant limit because in js we should not preallocate memory */ if (state.head && len && state.length < 65536 /*state.head.comm_max*/ ) state.head.comment += String.fromCharCode(len);
                }while (len && copy < have);
                if (state.flags & 0x0200 && state.wrap & 4) state.check = $1d314fc79f930156$var$crc32_1(state.check, input, copy, next);
                have -= copy;
                next += copy;
                if (len) break inf_leave;
            } else if (state.head) state.head.comment = null;
            state.mode = $1d314fc79f930156$var$HCRC;
        /* falls through */ case $1d314fc79f930156$var$HCRC:
            if (state.flags & 0x0200) {
                //=== NEEDBITS(16); */
                while(bits < 16){
                    if (have === 0) break inf_leave;
                    have--;
                    hold += input[next++] << bits;
                    bits += 8;
                }
                //===//
                if (state.wrap & 4 && hold !== (state.check & 0xffff)) {
                    strm.msg = "header crc mismatch";
                    state.mode = $1d314fc79f930156$var$BAD;
                    break;
                }
                //=== INITBITS();
                hold = 0;
                bits = 0;
            //===//
            }
            if (state.head) {
                state.head.hcrc = state.flags >> 9 & 1;
                state.head.done = true;
            }
            strm.adler = state.check = 0;
            state.mode = $1d314fc79f930156$var$TYPE;
            break;
        case $1d314fc79f930156$var$DICTID:
            //=== NEEDBITS(32); */
            while(bits < 32){
                if (have === 0) break inf_leave;
                have--;
                hold += input[next++] << bits;
                bits += 8;
            }
            //===//
            strm.adler = state.check = $1d314fc79f930156$var$zswap32(hold);
            //=== INITBITS();
            hold = 0;
            bits = 0;
            //===//
            state.mode = $1d314fc79f930156$var$DICT;
        /* falls through */ case $1d314fc79f930156$var$DICT:
            if (state.havedict === 0) {
                //--- RESTORE() ---
                strm.next_out = put;
                strm.avail_out = left;
                strm.next_in = next;
                strm.avail_in = have;
                state.hold = hold;
                state.bits = bits;
                //---
                return $1d314fc79f930156$var$Z_NEED_DICT$1;
            }
            strm.adler = state.check = 1 /*adler32(0L, Z_NULL, 0)*/ ;
            state.mode = $1d314fc79f930156$var$TYPE;
        /* falls through */ case $1d314fc79f930156$var$TYPE:
            if (flush === $1d314fc79f930156$var$Z_BLOCK || flush === $1d314fc79f930156$var$Z_TREES) break inf_leave;
        /* falls through */ case $1d314fc79f930156$var$TYPEDO:
            if (state.last) {
                //--- BYTEBITS() ---//
                hold >>>= bits & 7;
                bits -= bits & 7;
                //---//
                state.mode = $1d314fc79f930156$var$CHECK;
                break;
            }
            //=== NEEDBITS(3); */
            while(bits < 3){
                if (have === 0) break inf_leave;
                have--;
                hold += input[next++] << bits;
                bits += 8;
            }
            //===//
            state.last = hold & 0x01 /*BITS(1)*/ ;
            //--- DROPBITS(1) ---//
            hold >>>= 1;
            bits -= 1;
            //---//
            switch(hold & 0x03){
                case 0:
                    /* stored block */ //Tracev((stderr, "inflate:     stored block%s\n",
                    //        state.last ? " (last)" : ""));
                    state.mode = $1d314fc79f930156$var$STORED;
                    break;
                case 1:
                    /* fixed block */ $1d314fc79f930156$var$fixedtables(state);
                    //Tracev((stderr, "inflate:     fixed codes block%s\n",
                    //        state.last ? " (last)" : ""));
                    state.mode = $1d314fc79f930156$var$LEN_; /* decode codes */ 
                    if (flush === $1d314fc79f930156$var$Z_TREES) {
                        //--- DROPBITS(2) ---//
                        hold >>>= 2;
                        bits -= 2;
                        break inf_leave;
                    }
                    break;
                case 2:
                    /* dynamic block */ //Tracev((stderr, "inflate:     dynamic codes block%s\n",
                    //        state.last ? " (last)" : ""));
                    state.mode = $1d314fc79f930156$var$TABLE;
                    break;
                case 3:
                    strm.msg = "invalid block type";
                    state.mode = $1d314fc79f930156$var$BAD;
            }
            //--- DROPBITS(2) ---//
            hold >>>= 2;
            bits -= 2;
            break;
        case $1d314fc79f930156$var$STORED:
            //--- BYTEBITS() ---// /* go to byte boundary */
            hold >>>= bits & 7;
            bits -= bits & 7;
            //---//
            //=== NEEDBITS(32); */
            while(bits < 32){
                if (have === 0) break inf_leave;
                have--;
                hold += input[next++] << bits;
                bits += 8;
            }
            //===//
            if ((hold & 0xffff) !== (hold >>> 16 ^ 0xffff)) {
                strm.msg = "invalid stored block lengths";
                state.mode = $1d314fc79f930156$var$BAD;
                break;
            }
            state.length = hold & 0xffff;
            //Tracev((stderr, "inflate:       stored length %u\n",
            //        state.length));
            //=== INITBITS();
            hold = 0;
            bits = 0;
            //===//
            state.mode = $1d314fc79f930156$var$COPY_;
            if (flush === $1d314fc79f930156$var$Z_TREES) break inf_leave;
        /* falls through */ case $1d314fc79f930156$var$COPY_:
            state.mode = $1d314fc79f930156$var$COPY;
        /* falls through */ case $1d314fc79f930156$var$COPY:
            copy = state.length;
            if (copy) {
                if (copy > have) copy = have;
                if (copy > left) copy = left;
                if (copy === 0) break inf_leave;
                //--- zmemcpy(put, next, copy); ---
                output.set(input.subarray(next, next + copy), put);
                //---//
                have -= copy;
                next += copy;
                left -= copy;
                put += copy;
                state.length -= copy;
                break;
            }
            //Tracev((stderr, "inflate:       stored end\n"));
            state.mode = $1d314fc79f930156$var$TYPE;
            break;
        case $1d314fc79f930156$var$TABLE:
            //=== NEEDBITS(14); */
            while(bits < 14){
                if (have === 0) break inf_leave;
                have--;
                hold += input[next++] << bits;
                bits += 8;
            }
            //===//
            state.nlen = (hold & 0x1f) + 257;
            //--- DROPBITS(5) ---//
            hold >>>= 5;
            bits -= 5;
            //---//
            state.ndist = (hold & 0x1f) + 1;
            //--- DROPBITS(5) ---//
            hold >>>= 5;
            bits -= 5;
            //---//
            state.ncode = (hold & 0x0f) + 4;
            //--- DROPBITS(4) ---//
            hold >>>= 4;
            bits -= 4;
            //---//
            //#ifndef PKZIP_BUG_WORKAROUND
            if (state.nlen > 286 || state.ndist > 30) {
                strm.msg = "too many length or distance symbols";
                state.mode = $1d314fc79f930156$var$BAD;
                break;
            }
            //#endif
            //Tracev((stderr, "inflate:       table sizes ok\n"));
            state.have = 0;
            state.mode = $1d314fc79f930156$var$LENLENS;
        /* falls through */ case $1d314fc79f930156$var$LENLENS:
            while(state.have < state.ncode){
                //=== NEEDBITS(3);
                while(bits < 3){
                    if (have === 0) break inf_leave;
                    have--;
                    hold += input[next++] << bits;
                    bits += 8;
                }
                //===//
                state.lens[order[state.have++]] = hold & 0x07; //BITS(3);
                //--- DROPBITS(3) ---//
                hold >>>= 3;
                bits -= 3;
            //---//
            }
            while(state.have < 19)state.lens[order[state.have++]] = 0;
            // We have separate tables & no pointers. 2 commented lines below not needed.
            //state.next = state.codes;
            //state.lencode = state.next;
            // Switch to use dynamic table
            state.lencode = state.lendyn;
            state.lenbits = 7;
            opts = {
                bits: state.lenbits
            };
            ret = $1d314fc79f930156$var$inftrees($1d314fc79f930156$var$CODES, state.lens, 0, 19, state.lencode, 0, state.work, opts);
            state.lenbits = opts.bits;
            if (ret) {
                strm.msg = "invalid code lengths set";
                state.mode = $1d314fc79f930156$var$BAD;
                break;
            }
            //Tracev((stderr, "inflate:       code lengths ok\n"));
            state.have = 0;
            state.mode = $1d314fc79f930156$var$CODELENS;
        /* falls through */ case $1d314fc79f930156$var$CODELENS:
            while(state.have < state.nlen + state.ndist){
                for(;;){
                    here = state.lencode[hold & (1 << state.lenbits) - 1]; /*BITS(state.lenbits)*/ 
                    here_bits = here >>> 24;
                    here_op = here >>> 16 & 0xff;
                    here_val = here & 0xffff;
                    if (here_bits <= bits) break;
                    //--- PULLBYTE() ---//
                    if (have === 0) break inf_leave;
                    have--;
                    hold += input[next++] << bits;
                    bits += 8;
                //---//
                }
                if (here_val < 16) {
                    //--- DROPBITS(here.bits) ---//
                    hold >>>= here_bits;
                    bits -= here_bits;
                    //---//
                    state.lens[state.have++] = here_val;
                } else {
                    if (here_val === 16) {
                        //=== NEEDBITS(here.bits + 2);
                        n = here_bits + 2;
                        while(bits < n){
                            if (have === 0) break inf_leave;
                            have--;
                            hold += input[next++] << bits;
                            bits += 8;
                        }
                        //===//
                        //--- DROPBITS(here.bits) ---//
                        hold >>>= here_bits;
                        bits -= here_bits;
                        //---//
                        if (state.have === 0) {
                            strm.msg = "invalid bit length repeat";
                            state.mode = $1d314fc79f930156$var$BAD;
                            break;
                        }
                        len = state.lens[state.have - 1];
                        copy = 3 + (hold & 0x03); //BITS(2);
                        //--- DROPBITS(2) ---//
                        hold >>>= 2;
                        bits -= 2;
                    //---//
                    } else if (here_val === 17) {
                        //=== NEEDBITS(here.bits + 3);
                        n = here_bits + 3;
                        while(bits < n){
                            if (have === 0) break inf_leave;
                            have--;
                            hold += input[next++] << bits;
                            bits += 8;
                        }
                        //===//
                        //--- DROPBITS(here.bits) ---//
                        hold >>>= here_bits;
                        bits -= here_bits;
                        //---//
                        len = 0;
                        copy = 3 + (hold & 0x07); //BITS(3);
                        //--- DROPBITS(3) ---//
                        hold >>>= 3;
                        bits -= 3;
                    //---//
                    } else {
                        //=== NEEDBITS(here.bits + 7);
                        n = here_bits + 7;
                        while(bits < n){
                            if (have === 0) break inf_leave;
                            have--;
                            hold += input[next++] << bits;
                            bits += 8;
                        }
                        //===//
                        //--- DROPBITS(here.bits) ---//
                        hold >>>= here_bits;
                        bits -= here_bits;
                        //---//
                        len = 0;
                        copy = 11 + (hold & 0x7f); //BITS(7);
                        //--- DROPBITS(7) ---//
                        hold >>>= 7;
                        bits -= 7;
                    //---//
                    }
                    if (state.have + copy > state.nlen + state.ndist) {
                        strm.msg = "invalid bit length repeat";
                        state.mode = $1d314fc79f930156$var$BAD;
                        break;
                    }
                    while(copy--)state.lens[state.have++] = len;
                }
            }
            /* handle error breaks in while */ if (state.mode === $1d314fc79f930156$var$BAD) break;
            /* check for end-of-block code (better have one) */ if (state.lens[256] === 0) {
                strm.msg = "invalid code -- missing end-of-block";
                state.mode = $1d314fc79f930156$var$BAD;
                break;
            }
            /* build code tables -- note: do not change the lenbits or distbits
           values here (9 and 6) without reading the comments in inftrees.h
           concerning the ENOUGH constants, which depend on those values */ state.lenbits = 9;
            opts = {
                bits: state.lenbits
            };
            ret = $1d314fc79f930156$var$inftrees($1d314fc79f930156$var$LENS, state.lens, 0, state.nlen, state.lencode, 0, state.work, opts);
            // We have separate tables & no pointers. 2 commented lines below not needed.
            // state.next_index = opts.table_index;
            state.lenbits = opts.bits;
            // state.lencode = state.next;
            if (ret) {
                strm.msg = "invalid literal/lengths set";
                state.mode = $1d314fc79f930156$var$BAD;
                break;
            }
            state.distbits = 6;
            //state.distcode.copy(state.codes);
            // Switch to use dynamic table
            state.distcode = state.distdyn;
            opts = {
                bits: state.distbits
            };
            ret = $1d314fc79f930156$var$inftrees($1d314fc79f930156$var$DISTS, state.lens, state.nlen, state.ndist, state.distcode, 0, state.work, opts);
            // We have separate tables & no pointers. 2 commented lines below not needed.
            // state.next_index = opts.table_index;
            state.distbits = opts.bits;
            // state.distcode = state.next;
            if (ret) {
                strm.msg = "invalid distances set";
                state.mode = $1d314fc79f930156$var$BAD;
                break;
            }
            //Tracev((stderr, 'inflate:       codes ok\n'));
            state.mode = $1d314fc79f930156$var$LEN_;
            if (flush === $1d314fc79f930156$var$Z_TREES) break inf_leave;
        /* falls through */ case $1d314fc79f930156$var$LEN_:
            state.mode = $1d314fc79f930156$var$LEN;
        /* falls through */ case $1d314fc79f930156$var$LEN:
            if (have >= 6 && left >= 258) {
                //--- RESTORE() ---
                strm.next_out = put;
                strm.avail_out = left;
                strm.next_in = next;
                strm.avail_in = have;
                state.hold = hold;
                state.bits = bits;
                //---
                $1d314fc79f930156$var$inffast(strm, _out);
                //--- LOAD() ---
                put = strm.next_out;
                output = strm.output;
                left = strm.avail_out;
                next = strm.next_in;
                input = strm.input;
                have = strm.avail_in;
                hold = state.hold;
                bits = state.bits;
                //---
                if (state.mode === $1d314fc79f930156$var$TYPE) state.back = -1;
                break;
            }
            state.back = 0;
            for(;;){
                here = state.lencode[hold & (1 << state.lenbits) - 1]; /*BITS(state.lenbits)*/ 
                here_bits = here >>> 24;
                here_op = here >>> 16 & 0xff;
                here_val = here & 0xffff;
                if (here_bits <= bits) break;
                //--- PULLBYTE() ---//
                if (have === 0) break inf_leave;
                have--;
                hold += input[next++] << bits;
                bits += 8;
            //---//
            }
            if (here_op && (here_op & 0xf0) === 0) {
                last_bits = here_bits;
                last_op = here_op;
                last_val = here_val;
                for(;;){
                    here = state.lencode[last_val + ((hold & (1 << last_bits + last_op) - 1) >> last_bits)];
                    here_bits = here >>> 24;
                    here_op = here >>> 16 & 0xff;
                    here_val = here & 0xffff;
                    if (last_bits + here_bits <= bits) break;
                    //--- PULLBYTE() ---//
                    if (have === 0) break inf_leave;
                    have--;
                    hold += input[next++] << bits;
                    bits += 8;
                //---//
                }
                //--- DROPBITS(last.bits) ---//
                hold >>>= last_bits;
                bits -= last_bits;
                //---//
                state.back += last_bits;
            }
            //--- DROPBITS(here.bits) ---//
            hold >>>= here_bits;
            bits -= here_bits;
            //---//
            state.back += here_bits;
            state.length = here_val;
            if (here_op === 0) {
                //Tracevv((stderr, here.val >= 0x20 && here.val < 0x7f ?
                //        "inflate:         literal '%c'\n" :
                //        "inflate:         literal 0x%02x\n", here.val));
                state.mode = $1d314fc79f930156$var$LIT;
                break;
            }
            if (here_op & 32) {
                //Tracevv((stderr, "inflate:         end of block\n"));
                state.back = -1;
                state.mode = $1d314fc79f930156$var$TYPE;
                break;
            }
            if (here_op & 64) {
                strm.msg = "invalid literal/length code";
                state.mode = $1d314fc79f930156$var$BAD;
                break;
            }
            state.extra = here_op & 15;
            state.mode = $1d314fc79f930156$var$LENEXT;
        /* falls through */ case $1d314fc79f930156$var$LENEXT:
            if (state.extra) {
                //=== NEEDBITS(state.extra);
                n = state.extra;
                while(bits < n){
                    if (have === 0) break inf_leave;
                    have--;
                    hold += input[next++] << bits;
                    bits += 8;
                }
                //===//
                state.length += hold & (1 << state.extra) - 1 /*BITS(state.extra)*/ ;
                //--- DROPBITS(state.extra) ---//
                hold >>>= state.extra;
                bits -= state.extra;
                //---//
                state.back += state.extra;
            }
            //Tracevv((stderr, "inflate:         length %u\n", state.length));
            state.was = state.length;
            state.mode = $1d314fc79f930156$var$DIST;
        /* falls through */ case $1d314fc79f930156$var$DIST:
            for(;;){
                here = state.distcode[hold & (1 << state.distbits) - 1]; /*BITS(state.distbits)*/ 
                here_bits = here >>> 24;
                here_op = here >>> 16 & 0xff;
                here_val = here & 0xffff;
                if (here_bits <= bits) break;
                //--- PULLBYTE() ---//
                if (have === 0) break inf_leave;
                have--;
                hold += input[next++] << bits;
                bits += 8;
            //---//
            }
            if ((here_op & 0xf0) === 0) {
                last_bits = here_bits;
                last_op = here_op;
                last_val = here_val;
                for(;;){
                    here = state.distcode[last_val + ((hold & (1 << last_bits + last_op) - 1) >> last_bits)];
                    here_bits = here >>> 24;
                    here_op = here >>> 16 & 0xff;
                    here_val = here & 0xffff;
                    if (last_bits + here_bits <= bits) break;
                    //--- PULLBYTE() ---//
                    if (have === 0) break inf_leave;
                    have--;
                    hold += input[next++] << bits;
                    bits += 8;
                //---//
                }
                //--- DROPBITS(last.bits) ---//
                hold >>>= last_bits;
                bits -= last_bits;
                //---//
                state.back += last_bits;
            }
            //--- DROPBITS(here.bits) ---//
            hold >>>= here_bits;
            bits -= here_bits;
            //---//
            state.back += here_bits;
            if (here_op & 64) {
                strm.msg = "invalid distance code";
                state.mode = $1d314fc79f930156$var$BAD;
                break;
            }
            state.offset = here_val;
            state.extra = here_op & 15;
            state.mode = $1d314fc79f930156$var$DISTEXT;
        /* falls through */ case $1d314fc79f930156$var$DISTEXT:
            if (state.extra) {
                //=== NEEDBITS(state.extra);
                n = state.extra;
                while(bits < n){
                    if (have === 0) break inf_leave;
                    have--;
                    hold += input[next++] << bits;
                    bits += 8;
                }
                //===//
                state.offset += hold & (1 << state.extra) - 1 /*BITS(state.extra)*/ ;
                //--- DROPBITS(state.extra) ---//
                hold >>>= state.extra;
                bits -= state.extra;
                //---//
                state.back += state.extra;
            }
            //#ifdef INFLATE_STRICT
            if (state.offset > state.dmax) {
                strm.msg = "invalid distance too far back";
                state.mode = $1d314fc79f930156$var$BAD;
                break;
            }
            //#endif
            //Tracevv((stderr, "inflate:         distance %u\n", state.offset));
            state.mode = $1d314fc79f930156$var$MATCH;
        /* falls through */ case $1d314fc79f930156$var$MATCH:
            if (left === 0) break inf_leave;
            copy = _out - left;
            if (state.offset > copy) {
                copy = state.offset - copy;
                if (copy > state.whave) {
                    if (state.sane) {
                        strm.msg = "invalid distance too far back";
                        state.mode = $1d314fc79f930156$var$BAD;
                        break;
                    }
                }
                if (copy > state.wnext) {
                    copy -= state.wnext;
                    from = state.wsize - copy;
                } else from = state.wnext - copy;
                if (copy > state.length) copy = state.length;
                from_source = state.window;
            } else {
                from_source = output;
                from = put - state.offset;
                copy = state.length;
            }
            if (copy > left) copy = left;
            left -= copy;
            state.length -= copy;
            do output[put++] = from_source[from++];
            while (--copy);
            if (state.length === 0) state.mode = $1d314fc79f930156$var$LEN;
            break;
        case $1d314fc79f930156$var$LIT:
            if (left === 0) break inf_leave;
            output[put++] = state.length;
            left--;
            state.mode = $1d314fc79f930156$var$LEN;
            break;
        case $1d314fc79f930156$var$CHECK:
            if (state.wrap) {
                //=== NEEDBITS(32);
                while(bits < 32){
                    if (have === 0) break inf_leave;
                    have--;
                    // Use '|' instead of '+' to make sure that result is signed
                    hold |= input[next++] << bits;
                    bits += 8;
                }
                //===//
                _out -= left;
                strm.total_out += _out;
                state.total += _out;
                if (state.wrap & 4 && _out) strm.adler = state.check = /*UPDATE_CHECK(state.check, put - _out, _out);*/ state.flags ? $1d314fc79f930156$var$crc32_1(state.check, output, _out, put - _out) : $1d314fc79f930156$var$adler32_1(state.check, output, _out, put - _out);
                _out = left;
                // NB: crc32 stored as signed 32-bit int, zswap32 returns signed too
                if (state.wrap & 4 && (state.flags ? hold : $1d314fc79f930156$var$zswap32(hold)) !== state.check) {
                    strm.msg = "incorrect data check";
                    state.mode = $1d314fc79f930156$var$BAD;
                    break;
                }
                //=== INITBITS();
                hold = 0;
                bits = 0;
            //===//
            //Tracev((stderr, "inflate:   check matches trailer\n"));
            }
            state.mode = $1d314fc79f930156$var$LENGTH;
        /* falls through */ case $1d314fc79f930156$var$LENGTH:
            if (state.wrap && state.flags) {
                //=== NEEDBITS(32);
                while(bits < 32){
                    if (have === 0) break inf_leave;
                    have--;
                    hold += input[next++] << bits;
                    bits += 8;
                }
                //===//
                if (state.wrap & 4 && hold !== (state.total & 0xffffffff)) {
                    strm.msg = "incorrect length check";
                    state.mode = $1d314fc79f930156$var$BAD;
                    break;
                }
                //=== INITBITS();
                hold = 0;
                bits = 0;
            //===//
            //Tracev((stderr, "inflate:   length matches trailer\n"));
            }
            state.mode = $1d314fc79f930156$var$DONE;
        /* falls through */ case $1d314fc79f930156$var$DONE:
            ret = $1d314fc79f930156$var$Z_STREAM_END$1;
            break inf_leave;
        case $1d314fc79f930156$var$BAD:
            ret = $1d314fc79f930156$var$Z_DATA_ERROR$1;
            break inf_leave;
        case $1d314fc79f930156$var$MEM:
            return $1d314fc79f930156$var$Z_MEM_ERROR$1;
        case $1d314fc79f930156$var$SYNC:
        /* falls through */ default:
            return $1d314fc79f930156$var$Z_STREAM_ERROR$1;
    }
    // inf_leave <- here is real place for "goto inf_leave", emulated via "break inf_leave"
    /*
     Return from inflate(), updating the total counts and the check value.
     If there was no progress during the inflate() call, return a buffer
     error.  Call updatewindow() to create and/or update the window state.
     Note: a memory error from inflate() is non-recoverable.
   */ //--- RESTORE() ---
    strm.next_out = put;
    strm.avail_out = left;
    strm.next_in = next;
    strm.avail_in = have;
    state.hold = hold;
    state.bits = bits;
    //---
    if (state.wsize || _out !== strm.avail_out && state.mode < $1d314fc79f930156$var$BAD && (state.mode < $1d314fc79f930156$var$CHECK || flush !== $1d314fc79f930156$var$Z_FINISH$1)) $1d314fc79f930156$var$updatewindow(strm, strm.output, strm.next_out, _out - strm.avail_out);
    _in -= strm.avail_in;
    _out -= strm.avail_out;
    strm.total_in += _in;
    strm.total_out += _out;
    state.total += _out;
    if (state.wrap & 4 && _out) strm.adler = state.check = /*UPDATE_CHECK(state.check, strm.next_out - _out, _out);*/ state.flags ? $1d314fc79f930156$var$crc32_1(state.check, output, _out, strm.next_out - _out) : $1d314fc79f930156$var$adler32_1(state.check, output, _out, strm.next_out - _out);
    strm.data_type = state.bits + (state.last ? 64 : 0) + (state.mode === $1d314fc79f930156$var$TYPE ? 128 : 0) + (state.mode === $1d314fc79f930156$var$LEN_ || state.mode === $1d314fc79f930156$var$COPY_ ? 256 : 0);
    if ((_in === 0 && _out === 0 || flush === $1d314fc79f930156$var$Z_FINISH$1) && ret === $1d314fc79f930156$var$Z_OK$1) ret = $1d314fc79f930156$var$Z_BUF_ERROR;
    return ret;
};
const $1d314fc79f930156$var$inflateEnd = (strm)=>{
    if ($1d314fc79f930156$var$inflateStateCheck(strm)) return $1d314fc79f930156$var$Z_STREAM_ERROR$1;
    let state = strm.state;
    if (state.window) state.window = null;
    strm.state = null;
    return $1d314fc79f930156$var$Z_OK$1;
};
const $1d314fc79f930156$var$inflateGetHeader = (strm, head)=>{
    /* check state */ if ($1d314fc79f930156$var$inflateStateCheck(strm)) return $1d314fc79f930156$var$Z_STREAM_ERROR$1;
    const state = strm.state;
    if ((state.wrap & 2) === 0) return $1d314fc79f930156$var$Z_STREAM_ERROR$1;
    /* save header structure */ state.head = head;
    head.done = false;
    return $1d314fc79f930156$var$Z_OK$1;
};
const $1d314fc79f930156$var$inflateSetDictionary = (strm, dictionary)=>{
    const dictLength = dictionary.length;
    let state;
    let dictid;
    let ret;
    /* check state */ if ($1d314fc79f930156$var$inflateStateCheck(strm)) return $1d314fc79f930156$var$Z_STREAM_ERROR$1;
    state = strm.state;
    if (state.wrap !== 0 && state.mode !== $1d314fc79f930156$var$DICT) return $1d314fc79f930156$var$Z_STREAM_ERROR$1;
    /* check for correct dictionary identifier */ if (state.mode === $1d314fc79f930156$var$DICT) {
        dictid = 1; /* adler32(0, null, 0)*/ 
        /* dictid = adler32(dictid, dictionary, dictLength); */ dictid = $1d314fc79f930156$var$adler32_1(dictid, dictionary, dictLength, 0);
        if (dictid !== state.check) return $1d314fc79f930156$var$Z_DATA_ERROR$1;
    }
    /* copy dictionary to window using updatewindow(), which will amend the
   existing dictionary if appropriate */ ret = $1d314fc79f930156$var$updatewindow(strm, dictionary, dictLength, dictLength);
    if (ret) {
        state.mode = $1d314fc79f930156$var$MEM;
        return $1d314fc79f930156$var$Z_MEM_ERROR$1;
    }
    state.havedict = 1;
    // Tracev((stderr, "inflate:   dictionary set\n"));
    return $1d314fc79f930156$var$Z_OK$1;
};
var $1d314fc79f930156$var$inflateReset_1 = $1d314fc79f930156$var$inflateReset;
var $1d314fc79f930156$var$inflateReset2_1 = $1d314fc79f930156$var$inflateReset2;
var $1d314fc79f930156$var$inflateResetKeep_1 = $1d314fc79f930156$var$inflateResetKeep;
var $1d314fc79f930156$var$inflateInit_1 = $1d314fc79f930156$var$inflateInit;
var $1d314fc79f930156$var$inflateInit2_1 = $1d314fc79f930156$var$inflateInit2;
var $1d314fc79f930156$var$inflate_2$1 = $1d314fc79f930156$var$inflate$2;
var $1d314fc79f930156$var$inflateEnd_1 = $1d314fc79f930156$var$inflateEnd;
var $1d314fc79f930156$var$inflateGetHeader_1 = $1d314fc79f930156$var$inflateGetHeader;
var $1d314fc79f930156$var$inflateSetDictionary_1 = $1d314fc79f930156$var$inflateSetDictionary;
var $1d314fc79f930156$var$inflateInfo = "pako inflate (from Nodeca project)";
/* Not implemented
module.exports.inflateCodesUsed = inflateCodesUsed;
module.exports.inflateCopy = inflateCopy;
module.exports.inflateGetDictionary = inflateGetDictionary;
module.exports.inflateMark = inflateMark;
module.exports.inflatePrime = inflatePrime;
module.exports.inflateSync = inflateSync;
module.exports.inflateSyncPoint = inflateSyncPoint;
module.exports.inflateUndermine = inflateUndermine;
module.exports.inflateValidate = inflateValidate;
*/ var $1d314fc79f930156$var$inflate_1$2 = {
    inflateReset: $1d314fc79f930156$var$inflateReset_1,
    inflateReset2: $1d314fc79f930156$var$inflateReset2_1,
    inflateResetKeep: $1d314fc79f930156$var$inflateResetKeep_1,
    inflateInit: $1d314fc79f930156$var$inflateInit_1,
    inflateInit2: $1d314fc79f930156$var$inflateInit2_1,
    inflate: $1d314fc79f930156$var$inflate_2$1,
    inflateEnd: $1d314fc79f930156$var$inflateEnd_1,
    inflateGetHeader: $1d314fc79f930156$var$inflateGetHeader_1,
    inflateSetDictionary: $1d314fc79f930156$var$inflateSetDictionary_1,
    inflateInfo: $1d314fc79f930156$var$inflateInfo
};
// (C) 1995-2013 Jean-loup Gailly and Mark Adler
// (C) 2014-2017 Vitaly Puzrin and Andrey Tupitsin
//
// This software is provided 'as-is', without any express or implied
// warranty. In no event will the authors be held liable for any damages
// arising from the use of this software.
//
// Permission is granted to anyone to use this software for any purpose,
// including commercial applications, and to alter it and redistribute it
// freely, subject to the following restrictions:
//
// 1. The origin of this software must not be misrepresented; you must not
//   claim that you wrote the original software. If you use this software
//   in a product, an acknowledgment in the product documentation would be
//   appreciated but is not required.
// 2. Altered source versions must be plainly marked as such, and must not be
//   misrepresented as being the original software.
// 3. This notice may not be removed or altered from any source distribution.
function $1d314fc79f930156$var$GZheader() {
    /* true if compressed data believed to be text */ this.text = 0;
    /* modification time */ this.time = 0;
    /* extra flags (not used when writing a gzip file) */ this.xflags = 0;
    /* operating system */ this.os = 0;
    /* pointer to extra field or Z_NULL if none */ this.extra = null;
    /* extra field length (valid if extra != Z_NULL) */ this.extra_len = 0; // Actually, we don't need it in JS,
    // but leave for few code modifications
    //
    // Setup limits is not necessary because in js we should not preallocate memory
    // for inflate use constant limit in 65536 bytes
    //
    /* space at extra (only when reading header) */ // this.extra_max  = 0;
    /* pointer to zero-terminated file name or Z_NULL */ this.name = "";
    /* space at name (only when reading header) */ // this.name_max   = 0;
    /* pointer to zero-terminated comment or Z_NULL */ this.comment = "";
    /* space at comment (only when reading header) */ // this.comm_max   = 0;
    /* true if there was or will be a header crc */ this.hcrc = 0;
    /* true when done reading gzip header (not used when writing a gzip file) */ this.done = false;
}
var $1d314fc79f930156$var$gzheader = $1d314fc79f930156$var$GZheader;
const $1d314fc79f930156$var$toString = Object.prototype.toString;
/* Public constants ==========================================================*/ /* ===========================================================================*/ const { Z_NO_FLUSH: $1d314fc79f930156$var$Z_NO_FLUSH, Z_FINISH: $1d314fc79f930156$var$Z_FINISH, Z_OK: $1d314fc79f930156$var$Z_OK, Z_STREAM_END: $1d314fc79f930156$var$Z_STREAM_END, Z_NEED_DICT: $1d314fc79f930156$var$Z_NEED_DICT, Z_STREAM_ERROR: $1d314fc79f930156$var$Z_STREAM_ERROR, Z_DATA_ERROR: $1d314fc79f930156$var$Z_DATA_ERROR, Z_MEM_ERROR: $1d314fc79f930156$var$Z_MEM_ERROR } = $1d314fc79f930156$var$constants$2;
/* ===========================================================================*/ /**
 * class Inflate
 *
 * Generic JS-style wrapper for zlib calls. If you don't need
 * streaming behaviour - use more simple functions: [[inflate]]
 * and [[inflateRaw]].
 **/ /* internal
 * inflate.chunks -> Array
 *
 * Chunks of output data, if [[Inflate#onData]] not overridden.
 **/ /**
 * Inflate.result -> Uint8Array|String
 *
 * Uncompressed result, generated by default [[Inflate#onData]]
 * and [[Inflate#onEnd]] handlers. Filled after you push last chunk
 * (call [[Inflate#push]] with `Z_FINISH` / `true` param).
 **/ /**
 * Inflate.err -> Number
 *
 * Error code after inflate finished. 0 (Z_OK) on success.
 * Should be checked if broken data possible.
 **/ /**
 * Inflate.msg -> String
 *
 * Error message, if [[Inflate.err]] != 0
 **/ /**
 * new Inflate(options)
 * - options (Object): zlib inflate options.
 *
 * Creates new inflator instance with specified params. Throws exception
 * on bad params. Supported options:
 *
 * - `windowBits`
 * - `dictionary`
 *
 * [http://zlib.net/manual.html#Advanced](http://zlib.net/manual.html#Advanced)
 * for more information on these.
 *
 * Additional options, for internal needs:
 *
 * - `chunkSize` - size of generated data chunks (16K by default)
 * - `raw` (Boolean) - do raw inflate
 * - `to` (String) - if equal to 'string', then result will be converted
 *   from utf8 to utf16 (javascript) string. When string output requested,
 *   chunk length can differ from `chunkSize`, depending on content.
 *
 * By default, when no options set, autodetect deflate/gzip data format via
 * wrapper header.
 *
 * ##### Example:
 *
 * ```javascript
 * const pako = require('pako')
 * const chunk1 = new Uint8Array([1,2,3,4,5,6,7,8,9])
 * const chunk2 = new Uint8Array([10,11,12,13,14,15,16,17,18,19]);
 *
 * const inflate = new pako.Inflate({ level: 3});
 *
 * inflate.push(chunk1, false);
 * inflate.push(chunk2, true);  // true -> last chunk
 *
 * if (inflate.err) { throw new Error(inflate.err); }
 *
 * console.log(inflate.result);
 * ```
 **/ function $1d314fc79f930156$var$Inflate$1(options) {
    this.options = $1d314fc79f930156$var$common.assign({
        chunkSize: 65536,
        windowBits: 15,
        to: ""
    }, options || {});
    const opt = this.options;
    // Force window size for `raw` data, if not set directly,
    // because we have no header for autodetect.
    if (opt.raw && opt.windowBits >= 0 && opt.windowBits < 16) {
        opt.windowBits = -opt.windowBits;
        if (opt.windowBits === 0) opt.windowBits = -15;
    }
    // If `windowBits` not defined (and mode not raw) - set autodetect flag for gzip/deflate
    if (opt.windowBits >= 0 && opt.windowBits < 16 && !(options && options.windowBits)) opt.windowBits += 32;
    // Gzip header has no info about windows size, we can do autodetect only
    // for deflate. So, if window size not set, force it to max when gzip possible
    if (opt.windowBits > 15 && opt.windowBits < 48) // bit 3 (16) -> gzipped data
    // bit 4 (32) -> autodetect gzip/deflate
    {
        if ((opt.windowBits & 15) === 0) opt.windowBits |= 15;
    }
    this.err = 0; // error code, if happens (0 = Z_OK)
    this.msg = ""; // error message
    this.ended = false; // used to avoid multiple onEnd() calls
    this.chunks = []; // chunks of compressed data
    this.strm = new $1d314fc79f930156$var$zstream();
    this.strm.avail_out = 0;
    let status = $1d314fc79f930156$var$inflate_1$2.inflateInit2(this.strm, opt.windowBits);
    if (status !== $1d314fc79f930156$var$Z_OK) throw new Error($1d314fc79f930156$var$messages[status]);
    this.header = new $1d314fc79f930156$var$gzheader();
    $1d314fc79f930156$var$inflate_1$2.inflateGetHeader(this.strm, this.header);
    // Setup dictionary
    if (opt.dictionary) {
        // Convert data if needed
        if (typeof opt.dictionary === "string") opt.dictionary = $1d314fc79f930156$var$strings.string2buf(opt.dictionary);
        else if ($1d314fc79f930156$var$toString.call(opt.dictionary) === "[object ArrayBuffer]") opt.dictionary = new Uint8Array(opt.dictionary);
        if (opt.raw) {
            status = $1d314fc79f930156$var$inflate_1$2.inflateSetDictionary(this.strm, opt.dictionary);
            if (status !== $1d314fc79f930156$var$Z_OK) throw new Error($1d314fc79f930156$var$messages[status]);
        }
    }
}
/**
 * Inflate#push(data[, flush_mode]) -> Boolean
 * - data (Uint8Array|ArrayBuffer): input data
 * - flush_mode (Number|Boolean): 0..6 for corresponding Z_NO_FLUSH..Z_TREE
 *   flush modes. See constants. Skipped or `false` means Z_NO_FLUSH,
 *   `true` means Z_FINISH.
 *
 * Sends input data to inflate pipe, generating [[Inflate#onData]] calls with
 * new output chunks. Returns `true` on success. If end of stream detected,
 * [[Inflate#onEnd]] will be called.
 *
 * `flush_mode` is not needed for normal operation, because end of stream
 * detected automatically. You may try to use it for advanced things, but
 * this functionality was not tested.
 *
 * On fail call [[Inflate#onEnd]] with error code and return false.
 *
 * ##### Example
 *
 * ```javascript
 * push(chunk, false); // push one of data chunks
 * ...
 * push(chunk, true);  // push last chunk
 * ```
 **/ $1d314fc79f930156$var$Inflate$1.prototype.push = function(data, flush_mode) {
    const strm = this.strm;
    const chunkSize = this.options.chunkSize;
    const dictionary = this.options.dictionary;
    let status, _flush_mode, last_avail_out;
    if (this.ended) return false;
    if (flush_mode === ~~flush_mode) _flush_mode = flush_mode;
    else _flush_mode = flush_mode === true ? $1d314fc79f930156$var$Z_FINISH : $1d314fc79f930156$var$Z_NO_FLUSH;
    // Convert data if needed
    if ($1d314fc79f930156$var$toString.call(data) === "[object ArrayBuffer]") strm.input = new Uint8Array(data);
    else strm.input = data;
    strm.next_in = 0;
    strm.avail_in = strm.input.length;
    for(;;){
        if (strm.avail_out === 0) {
            strm.output = new Uint8Array(chunkSize);
            strm.next_out = 0;
            strm.avail_out = chunkSize;
        }
        status = $1d314fc79f930156$var$inflate_1$2.inflate(strm, _flush_mode);
        if (status === $1d314fc79f930156$var$Z_NEED_DICT && dictionary) {
            status = $1d314fc79f930156$var$inflate_1$2.inflateSetDictionary(strm, dictionary);
            if (status === $1d314fc79f930156$var$Z_OK) status = $1d314fc79f930156$var$inflate_1$2.inflate(strm, _flush_mode);
            else if (status === $1d314fc79f930156$var$Z_DATA_ERROR) // Replace code with more verbose
            status = $1d314fc79f930156$var$Z_NEED_DICT;
        }
        // Skip snyc markers if more data follows and not raw mode
        while(strm.avail_in > 0 && status === $1d314fc79f930156$var$Z_STREAM_END && strm.state.wrap > 0 && data[strm.next_in] !== 0){
            $1d314fc79f930156$var$inflate_1$2.inflateReset(strm);
            status = $1d314fc79f930156$var$inflate_1$2.inflate(strm, _flush_mode);
        }
        switch(status){
            case $1d314fc79f930156$var$Z_STREAM_ERROR:
            case $1d314fc79f930156$var$Z_DATA_ERROR:
            case $1d314fc79f930156$var$Z_NEED_DICT:
            case $1d314fc79f930156$var$Z_MEM_ERROR:
                this.onEnd(status);
                this.ended = true;
                return false;
        }
        // Remember real `avail_out` value, because we may patch out buffer content
        // to align utf8 strings boundaries.
        last_avail_out = strm.avail_out;
        if (strm.next_out) {
            if (strm.avail_out === 0 || status === $1d314fc79f930156$var$Z_STREAM_END) {
                if (this.options.to === "string") {
                    let next_out_utf8 = $1d314fc79f930156$var$strings.utf8border(strm.output, strm.next_out);
                    let tail = strm.next_out - next_out_utf8;
                    let utf8str = $1d314fc79f930156$var$strings.buf2string(strm.output, next_out_utf8);
                    // move tail & realign counters
                    strm.next_out = tail;
                    strm.avail_out = chunkSize - tail;
                    if (tail) strm.output.set(strm.output.subarray(next_out_utf8, next_out_utf8 + tail), 0);
                    this.onData(utf8str);
                } else this.onData(strm.output.length === strm.next_out ? strm.output : strm.output.subarray(0, strm.next_out));
            }
        }
        // Must repeat iteration if out buffer is full
        if (status === $1d314fc79f930156$var$Z_OK && last_avail_out === 0) continue;
        // Finalize if end of stream reached.
        if (status === $1d314fc79f930156$var$Z_STREAM_END) {
            status = $1d314fc79f930156$var$inflate_1$2.inflateEnd(this.strm);
            this.onEnd(status);
            this.ended = true;
            return true;
        }
        if (strm.avail_in === 0) break;
    }
    return true;
};
/**
 * Inflate#onData(chunk) -> Void
 * - chunk (Uint8Array|String): output data. When string output requested,
 *   each chunk will be string.
 *
 * By default, stores data blocks in `chunks[]` property and glue
 * those in `onEnd`. Override this handler, if you need another behaviour.
 **/ $1d314fc79f930156$var$Inflate$1.prototype.onData = function(chunk) {
    this.chunks.push(chunk);
};
/**
 * Inflate#onEnd(status) -> Void
 * - status (Number): inflate status. 0 (Z_OK) on success,
 *   other if not.
 *
 * Called either after you tell inflate that the input stream is
 * complete (Z_FINISH). By default - join collected chunks,
 * free memory and fill `results` / `err` properties.
 **/ $1d314fc79f930156$var$Inflate$1.prototype.onEnd = function(status) {
    // On success - join
    if (status === $1d314fc79f930156$var$Z_OK) {
        if (this.options.to === "string") this.result = this.chunks.join("");
        else this.result = $1d314fc79f930156$var$common.flattenChunks(this.chunks);
    }
    this.chunks = [];
    this.err = status;
    this.msg = this.strm.msg;
};
/**
 * inflate(data[, options]) -> Uint8Array|String
 * - data (Uint8Array|ArrayBuffer): input data to decompress.
 * - options (Object): zlib inflate options.
 *
 * Decompress `data` with inflate/ungzip and `options`. Autodetect
 * format via wrapper header by default. That's why we don't provide
 * separate `ungzip` method.
 *
 * Supported options are:
 *
 * - windowBits
 *
 * [http://zlib.net/manual.html#Advanced](http://zlib.net/manual.html#Advanced)
 * for more information.
 *
 * Sugar (options):
 *
 * - `raw` (Boolean) - say that we work with raw stream, if you don't wish to specify
 *   negative windowBits implicitly.
 * - `to` (String) - if equal to 'string', then result will be converted
 *   from utf8 to utf16 (javascript) string. When string output requested,
 *   chunk length can differ from `chunkSize`, depending on content.
 *
 *
 * ##### Example:
 *
 * ```javascript
 * const pako = require('pako');
 * const input = pako.deflate(new Uint8Array([1,2,3,4,5,6,7,8,9]));
 * let output;
 *
 * try {
 *   output = pako.inflate(input);
 * } catch (err) {
 *   console.log(err);
 * }
 * ```
 **/ function $1d314fc79f930156$var$inflate$1(input, options) {
    const inflator = new $1d314fc79f930156$var$Inflate$1(options);
    inflator.push(input);
    // That will never happens, if you don't cheat with options :)
    if (inflator.err) throw inflator.msg || $1d314fc79f930156$var$messages[inflator.err];
    return inflator.result;
}
/**
 * inflateRaw(data[, options]) -> Uint8Array|String
 * - data (Uint8Array|ArrayBuffer): input data to decompress.
 * - options (Object): zlib inflate options.
 *
 * The same as [[inflate]], but creates raw data, without wrapper
 * (header and adler32 crc).
 **/ function $1d314fc79f930156$var$inflateRaw$1(input, options) {
    options = options || {};
    options.raw = true;
    return $1d314fc79f930156$var$inflate$1(input, options);
}
/**
 * ungzip(data[, options]) -> Uint8Array|String
 * - data (Uint8Array|ArrayBuffer): input data to decompress.
 * - options (Object): zlib inflate options.
 *
 * Just shortcut to [[inflate]], because it autodetects format
 * by header.content. Done for convenience.
 **/ var $1d314fc79f930156$var$Inflate_1$1 = $1d314fc79f930156$var$Inflate$1;
var $1d314fc79f930156$var$inflate_2 = $1d314fc79f930156$var$inflate$1;
var $1d314fc79f930156$var$inflateRaw_1$1 = $1d314fc79f930156$var$inflateRaw$1;
var $1d314fc79f930156$var$ungzip$1 = $1d314fc79f930156$var$inflate$1;
var $1d314fc79f930156$var$constants = $1d314fc79f930156$var$constants$2;
var $1d314fc79f930156$var$inflate_1$1 = {
    Inflate: $1d314fc79f930156$var$Inflate_1$1,
    inflate: $1d314fc79f930156$var$inflate_2,
    inflateRaw: $1d314fc79f930156$var$inflateRaw_1$1,
    ungzip: $1d314fc79f930156$var$ungzip$1,
    constants: $1d314fc79f930156$var$constants
};
const { Deflate: $1d314fc79f930156$var$Deflate, deflate: $1d314fc79f930156$var$deflate, deflateRaw: $1d314fc79f930156$var$deflateRaw, gzip: $1d314fc79f930156$var$gzip } = $1d314fc79f930156$var$deflate_1$1;
const { Inflate: $1d314fc79f930156$var$Inflate, inflate: $1d314fc79f930156$var$inflate, inflateRaw: $1d314fc79f930156$var$inflateRaw, ungzip: $1d314fc79f930156$var$ungzip } = $1d314fc79f930156$var$inflate_1$1;
var $1d314fc79f930156$export$ae157b6234afe138 = $1d314fc79f930156$var$Deflate;
var $1d314fc79f930156$export$2316623ecd1285ab = $1d314fc79f930156$var$deflate;
var $1d314fc79f930156$export$e95d6a8f69fb340a = $1d314fc79f930156$var$deflateRaw;
var $1d314fc79f930156$export$69f0ea7cf3a331a8 = $1d314fc79f930156$var$gzip;
var $1d314fc79f930156$export$d1de70a877d6e43c = $1d314fc79f930156$var$Inflate;
var $1d314fc79f930156$export$cae1ce83fe4a1782 = $1d314fc79f930156$var$inflate;
var $1d314fc79f930156$export$d0f0aa2d05c905c5 = $1d314fc79f930156$var$inflateRaw;
var $1d314fc79f930156$export$95adf9d270383091 = $1d314fc79f930156$var$ungzip;
var $1d314fc79f930156$export$1a988e7317c65621 = $1d314fc79f930156$var$constants$2;
var $1d314fc79f930156$export$2e2bcd8739ae039 = {
    Deflate: $1d314fc79f930156$export$ae157b6234afe138,
    deflate: $1d314fc79f930156$export$2316623ecd1285ab,
    deflateRaw: $1d314fc79f930156$export$e95d6a8f69fb340a,
    gzip: $1d314fc79f930156$export$69f0ea7cf3a331a8,
    Inflate: $1d314fc79f930156$export$d1de70a877d6e43c,
    inflate: $1d314fc79f930156$export$cae1ce83fe4a1782,
    inflateRaw: $1d314fc79f930156$export$d0f0aa2d05c905c5,
    ungzip: $1d314fc79f930156$export$95adf9d270383091,
    constants: $1d314fc79f930156$export$1a988e7317c65621
};


class $a0cf3cac0d21f701$export$86495b081fef8e52 {
    constructor(device){
        this.device = device;
        this.slip_reader_enabled = false;
        this.left_over = new Uint8Array(0);
        this.baudrate = 0;
        this._DTR_state = false;
    }
    get_info() {
        const info = this.device.getInfo();
        return info.usbVendorId && info.usbProductId ? `WebSerial VendorID 0x${info.usbVendorId.toString(16)} ProductID 0x${info.usbProductId.toString(16)}` : "";
    }
    get_pid() {
        return this.device.getInfo().usbProductId;
    }
    slip_writer(data) {
        let count_esc = 0;
        let i = 0, j = 0;
        for(i = 0; i < data.length; i++)if (data[i] === 0xc0 || data[i] === 0xdb) count_esc++;
        const out_data = new Uint8Array(2 + count_esc + data.length);
        out_data[0] = 0xc0;
        j = 1;
        for(i = 0; i < data.length; i++, j++){
            if (data[i] === 0xc0) {
                out_data[j++] = 0xdb;
                out_data[j] = 0xdc;
                continue;
            }
            if (data[i] === 0xdb) {
                out_data[j++] = 0xdb;
                out_data[j] = 0xdd;
                continue;
            }
            out_data[j] = data[i];
        }
        out_data[j] = 0xc0;
        return out_data;
    }
    async write(data) {
        const out_data = this.slip_writer(data);
        if (this.device.writable) {
            const writer = this.device.writable.getWriter();
            await writer.write(out_data);
            writer.releaseLock();
        }
    }
    _appendBuffer(buffer1, buffer2) {
        const tmp = new Uint8Array(buffer1.byteLength + buffer2.byteLength);
        tmp.set(new Uint8Array(buffer1), 0);
        tmp.set(new Uint8Array(buffer2), buffer1.byteLength);
        return tmp.buffer;
    }
    /* this function expects complete packet (hence reader reads for atleast 8 bytes. This function is
     * stateless and returns the first wellformed packet only after replacing escape sequence */ slip_reader(data) {
        let i = 0;
        let data_start = 0, data_end = 0;
        let state = "init";
        while(i < data.length){
            if (state === "init" && data[i] == 0xc0) {
                data_start = i + 1;
                state = "valid_data";
                i++;
                continue;
            }
            if (state === "valid_data" && data[i] == 0xc0) {
                data_end = i - 1;
                state = "packet_complete";
                break;
            }
            i++;
        }
        if (state !== "packet_complete") {
            this.left_over = data;
            return new Uint8Array(0);
        }
        this.left_over = data.slice(data_end + 2);
        const temp_pkt = new Uint8Array(data_end - data_start + 1);
        let j = 0;
        for(i = data_start; i <= data_end; i++, j++){
            if (data[i] === 0xdb && data[i + 1] === 0xdc) {
                temp_pkt[j] = 0xc0;
                i++;
                continue;
            }
            if (data[i] === 0xdb && data[i + 1] === 0xdd) {
                temp_pkt[j] = 0xdb;
                i++;
                continue;
            }
            temp_pkt[j] = data[i];
        }
        const packet = temp_pkt.slice(0, j); /* Remove unused bytes due to escape seq */ 
        return packet;
    }
    async read(timeout = 0, min_data = 12) {
        let t;
        let packet = this.left_over;
        this.left_over = new Uint8Array(0);
        if (this.slip_reader_enabled) {
            const val_final = this.slip_reader(packet);
            if (val_final.length > 0) return val_final;
            packet = this.left_over;
            this.left_over = new Uint8Array(0);
        }
        if (this.device.readable == null) return this.left_over;
        const reader = this.device.readable.getReader();
        try {
            if (timeout > 0) t = setTimeout(function() {
                reader.cancel();
            }, timeout);
            do {
                const { value: value, done: done } = await reader.read();
                if (done) {
                    this.left_over = packet;
                    throw new Error("Timeout");
                }
                const p = new Uint8Array(this._appendBuffer(packet.buffer, value.buffer));
                packet = p;
            }while (packet.length < min_data);
        } finally{
            if (timeout > 0) clearTimeout(t);
            reader.releaseLock();
        }
        if (this.slip_reader_enabled) return this.slip_reader(packet);
        return packet;
    }
    async rawRead(timeout = 0) {
        if (this.left_over.length != 0) {
            const p = this.left_over;
            this.left_over = new Uint8Array(0);
            return p;
        }
        if (!this.device.readable) return this.left_over;
        const reader = this.device.readable.getReader();
        let t;
        try {
            if (timeout > 0) t = setTimeout(function() {
                reader.cancel();
            }, timeout);
            const { value: value, done: done } = await reader.read();
            if (done) throw new Error("Timeout");
            return value;
        } finally{
            if (timeout > 0) clearTimeout(t);
            reader.releaseLock();
        }
    }
    async setRTS(state) {
        await this.device.setSignals({
            requestToSend: state
        });
        // # Work-around for adapters on Windows using the usbser.sys driver:
        // # generate a dummy change to DTR so that the set-control-line-state
        // # request is sent with the updated RTS state and the same DTR state
        // Referenced to esptool.py
        await this.setDTR(this._DTR_state);
    }
    async setDTR(state) {
        this._DTR_state = state;
        await this.device.setSignals({
            dataTerminalReady: state
        });
    }
    async connect(baud = 115200, serialOptions = {}) {
        await this.device.open({
            baudRate: baud,
            dataBits: serialOptions === null || serialOptions === void 0 ? void 0 : serialOptions.dataBits,
            stopBits: serialOptions === null || serialOptions === void 0 ? void 0 : serialOptions.stopBits,
            bufferSize: serialOptions === null || serialOptions === void 0 ? void 0 : serialOptions.bufferSize,
            parity: serialOptions === null || serialOptions === void 0 ? void 0 : serialOptions.parity,
            flowControl: serialOptions === null || serialOptions === void 0 ? void 0 : serialOptions.flowControl
        });
        this.baudrate = baud;
        this.left_over = new Uint8Array(0);
    }
    async sleep(ms) {
        return new Promise((resolve)=>setTimeout(resolve, ms));
    }
    async waitForUnlock(timeout) {
        while(this.device.readable && this.device.readable.locked || this.device.writable && this.device.writable.locked)await this.sleep(timeout);
    }
    async disconnect() {
        await this.waitForUnlock(400);
        await this.device.close();
    }
}


const $566461229c0b5a39$var$DEFAULT_RESET_DELAY = 50;
function $566461229c0b5a39$var$sleep(ms) {
    return new Promise((resolve)=>setTimeout(resolve, ms));
}
async function $566461229c0b5a39$export$d3423661fbfd2b60(transport, resetDelay = $566461229c0b5a39$var$DEFAULT_RESET_DELAY) {
    await transport.setDTR(false);
    await transport.setRTS(true);
    await $566461229c0b5a39$var$sleep(100);
    await transport.setDTR(true);
    await transport.setRTS(false);
    await $566461229c0b5a39$var$sleep(resetDelay);
    await transport.setDTR(false);
}
async function $566461229c0b5a39$export$3252bdf5627aa8a3(transport) {
    await transport.setRTS(false);
    await transport.setDTR(false);
    await $566461229c0b5a39$var$sleep(100);
    await transport.setDTR(true);
    await transport.setRTS(false);
    await $566461229c0b5a39$var$sleep(100);
    await transport.setRTS(true);
    await transport.setDTR(false);
    await transport.setRTS(true);
    await $566461229c0b5a39$var$sleep(100);
    await transport.setRTS(false);
    await transport.setDTR(false);
}
async function $566461229c0b5a39$export$4ff35e5b1175d6f6(transport, usingUsbOtg = false) {
    if (usingUsbOtg) {
        await $566461229c0b5a39$var$sleep(200);
        await transport.setRTS(false);
        await $566461229c0b5a39$var$sleep(200);
    } else {
        await $566461229c0b5a39$var$sleep(100);
        await transport.setRTS(false);
    }
}
function $566461229c0b5a39$export$929a22f56823f4cb(seqStr) {
    const commands = [
        "D",
        "R",
        "W"
    ];
    const commandsList = seqStr.split("|");
    for (const cmd of commandsList){
        const code = cmd[0];
        const arg = cmd.slice(1);
        if (!commands.includes(code)) return false; // Invalid command code
        if (code === "D" || code === "R") {
            if (arg !== "0" && arg !== "1") return false; // Invalid argument for D and R commands
        } else if (code === "W") {
            const delay = parseInt(arg);
            if (isNaN(delay) || delay <= 0) return false; // Invalid argument for W command
        }
    }
    return true; // All commands are valid
}
async function $566461229c0b5a39$export$e5e6796b349bcc84(transport, sequenceString) {
    const resetDictionary = {
        D: async (arg)=>await transport.setDTR(arg),
        R: async (arg)=>await transport.setRTS(arg),
        W: async (delay)=>await $566461229c0b5a39$var$sleep(delay)
    };
    try {
        const isValidSequence = $566461229c0b5a39$export$929a22f56823f4cb(sequenceString);
        if (!isValidSequence) return;
        const cmds = sequenceString.split("|");
        for (const cmd of cmds){
            const cmdKey = cmd[0];
            const cmdVal = cmd.slice(1);
            if (cmdKey === "W") await resetDictionary["W"](Number(cmdVal));
            else if (cmdKey === "D" || cmdKey === "R") await resetDictionary[cmdKey](cmdVal === "1");
        }
    } catch (error) {
        throw new Error("Invalid custom reset sequence");
    }
}
var $566461229c0b5a39$export$2e2bcd8739ae039 = {
    classicReset: $566461229c0b5a39$export$d3423661fbfd2b60,
    customReset: $566461229c0b5a39$export$e5e6796b349bcc84,
    hardReset: $566461229c0b5a39$export$4ff35e5b1175d6f6,
    usbJTAGSerialReset: $566461229c0b5a39$export$3252bdf5627aa8a3,
    validateCustomResetStringSequence: $566461229c0b5a39$export$929a22f56823f4cb
};









async function $45ec30e66bc4f018$var$magic2Chip(magic) {
    switch(magic){
        case 0x00f01d83:
            {
                const { ESP32ROM: ESP32ROM } = await (parcelRequire("5GoMm"));
                return new ESP32ROM();
            }
        case 0x6921506f:
        case 0x1b31506f:
            {
                const { ESP32C3ROM: ESP32C3ROM } = await (parcelRequire("eV5qQ"));
                return new ESP32C3ROM();
            }
        case 0x2ce0806f:
            {
                const { ESP32C6ROM: ESP32C6ROM } = await (parcelRequire("3vVHm"));
                return new ESP32C6ROM();
            }
        case 0xd7b73e80:
            {
                const { ESP32H2ROM: ESP32H2ROM } = await (parcelRequire("2HErZ"));
                return new ESP32H2ROM();
            }
        case 0x09:
            {
                const { ESP32S3ROM: ESP32S3ROM } = await (parcelRequire("erTWI"));
                return new ESP32S3ROM();
            }
        case 0x000007c6:
            {
                const { ESP32S2ROM: ESP32S2ROM } = await (parcelRequire("3Cwwi"));
                return new ESP32S2ROM();
            }
        case 0xfff0c101:
            {
                const { ESP8266ROM: ESP8266ROM } = await (parcelRequire("cPWZz"));
                return new ESP8266ROM();
            }
        default:
            return null;
    }
}
class $45ec30e66bc4f018$export$b0f7a6c745790308 {
    constructor(options){
        this.ESP_RAM_BLOCK = 0x1800;
        this.ESP_FLASH_BEGIN = 0x02;
        this.ESP_FLASH_DATA = 0x03;
        this.ESP_FLASH_END = 0x04;
        this.ESP_MEM_BEGIN = 0x05;
        this.ESP_MEM_END = 0x06;
        this.ESP_MEM_DATA = 0x07;
        this.ESP_WRITE_REG = 0x09;
        this.ESP_READ_REG = 0x0a;
        this.ESP_SPI_ATTACH = 0x0d;
        this.ESP_CHANGE_BAUDRATE = 0x0f;
        this.ESP_FLASH_DEFL_BEGIN = 0x10;
        this.ESP_FLASH_DEFL_DATA = 0x11;
        this.ESP_FLASH_DEFL_END = 0x12;
        this.ESP_SPI_FLASH_MD5 = 0x13;
        // Only Stub supported commands
        this.ESP_ERASE_FLASH = 0xd0;
        this.ESP_ERASE_REGION = 0xd1;
        this.ESP_READ_FLASH = 0xd2;
        this.ESP_RUN_USER_CODE = 0xd3;
        this.ESP_IMAGE_MAGIC = 0xe9;
        this.ESP_CHECKSUM_MAGIC = 0xef;
        // Response code(s) sent by ROM
        this.ROM_INVALID_RECV_MSG = 0x05; // response if an invalid message is received
        this.ERASE_REGION_TIMEOUT_PER_MB = 30000;
        this.ERASE_WRITE_TIMEOUT_PER_MB = 40000;
        this.MD5_TIMEOUT_PER_MB = 8000;
        this.CHIP_ERASE_TIMEOUT = 120000;
        this.FLASH_READ_TIMEOUT = 100000;
        this.MAX_TIMEOUT = this.CHIP_ERASE_TIMEOUT * 2;
        this.CHIP_DETECT_MAGIC_REG_ADDR = 0x40001000;
        this.DETECTED_FLASH_SIZES = {
            0x12: "256KB",
            0x13: "512KB",
            0x14: "1MB",
            0x15: "2MB",
            0x16: "4MB",
            0x17: "8MB",
            0x18: "16MB"
        };
        this.DETECTED_FLASH_SIZES_NUM = {
            0x12: 256,
            0x13: 512,
            0x14: 1024,
            0x15: 2048,
            0x16: 4096,
            0x17: 8192,
            0x18: 16384
        };
        this.USB_JTAG_SERIAL_PID = 0x1001;
        this.romBaudrate = 115200;
        this.debugLogging = false;
        this.checksum = function(data) {
            let i;
            let chk = 0xef;
            for(i = 0; i < data.length; i++)chk ^= data[i];
            return chk;
        };
        this.timeout_per_mb = function(seconds_per_mb, size_bytes) {
            const result = seconds_per_mb * (size_bytes / 1000000);
            if (result < 3000) return 3000;
            else return result;
        };
        this.flash_size_bytes = function(flash_size) {
            let flash_size_b = -1;
            if (flash_size.indexOf("KB") !== -1) flash_size_b = parseInt(flash_size.slice(0, flash_size.indexOf("KB"))) * 1024;
            else if (flash_size.indexOf("MB") !== -1) flash_size_b = parseInt(flash_size.slice(0, flash_size.indexOf("MB"))) * 1048576;
            return flash_size_b;
        };
        this.IS_STUB = false;
        this.FLASH_WRITE_SIZE = 0x4000;
        this.transport = options.transport;
        this.baudrate = options.baudrate;
        if (options.serialOptions) this.serialOptions = options.serialOptions;
        if (options.romBaudrate) this.romBaudrate = options.romBaudrate;
        if (options.terminal) {
            this.terminal = options.terminal;
            this.terminal.clean();
        }
        if (options.debugLogging) this.debugLogging = options.debugLogging;
        if (options.port) this.transport = new (0, $a0cf3cac0d21f701$export$86495b081fef8e52)(options.port);
        this.info("esptool.js");
        this.info("Serial port " + this.transport.get_info());
    }
    _sleep(ms) {
        return new Promise((resolve)=>setTimeout(resolve, ms));
    }
    write(str, withNewline = true) {
        if (this.terminal) {
            if (withNewline) this.terminal.writeLine(str);
            else this.terminal.write(str);
        } else // eslint-disable-next-line no-console
        console.log(str);
    }
    error(str, withNewline = true) {
        this.write(`Error: ${str}`, withNewline);
    }
    info(str, withNewline = true) {
        this.write(str, withNewline);
    }
    debug(str, withNewline = true) {
        if (this.debugLogging) this.write(`Debug: ${str}`, withNewline);
    }
    _short_to_bytearray(i) {
        return new Uint8Array([
            i & 0xff,
            i >> 8 & 0xff
        ]);
    }
    _int_to_bytearray(i) {
        return new Uint8Array([
            i & 0xff,
            i >> 8 & 0xff,
            i >> 16 & 0xff,
            i >> 24 & 0xff
        ]);
    }
    _bytearray_to_short(i, j) {
        return i | j >> 8;
    }
    _bytearray_to_int(i, j, k, l) {
        return i | j << 8 | k << 16 | l << 24;
    }
    _appendBuffer(buffer1, buffer2) {
        const tmp = new Uint8Array(buffer1.byteLength + buffer2.byteLength);
        tmp.set(new Uint8Array(buffer1), 0);
        tmp.set(new Uint8Array(buffer2), buffer1.byteLength);
        return tmp.buffer;
    }
    _appendArray(arr1, arr2) {
        const c = new Uint8Array(arr1.length + arr2.length);
        c.set(arr1, 0);
        c.set(arr2, arr1.length);
        return c;
    }
    ui8ToBstr(u8Array) {
        let b_str = "";
        for(let i = 0; i < u8Array.length; i++)b_str += String.fromCharCode(u8Array[i]);
        return b_str;
    }
    bstrToUi8(bStr) {
        const u8_array = new Uint8Array(bStr.length);
        for(let i = 0; i < bStr.length; i++)u8_array[i] = bStr.charCodeAt(i);
        return u8_array;
    }
    async flush_input() {
        try {
            await this.transport.rawRead(200);
        } catch (e) {
            this.error(e.message);
        }
    }
    async read_packet(op = null, timeout = 3000) {
        // Check up-to next 100 packets for valid response packet
        for(let i = 0; i < 100; i++){
            const p = await this.transport.read(timeout);
            const resp = p[0];
            const op_ret = p[1];
            const val = this._bytearray_to_int(p[4], p[5], p[6], p[7]);
            const data = p.slice(8);
            if (resp == 1) {
                if (op == null || op_ret == op) return [
                    val,
                    data
                ];
                else if (data[0] != 0 && data[1] == this.ROM_INVALID_RECV_MSG) {
                    await this.flush_input();
                    throw new (0, $07442ebb96f65399$export$5b519f82636185ec)("unsupported command error");
                }
            }
        }
        throw new (0, $07442ebb96f65399$export$5b519f82636185ec)("invalid response");
    }
    async command(op = null, data = new Uint8Array(0), chk = 0, waitResponse = true, timeout = 3000) {
        if (op != null) {
            const pkt = new Uint8Array(8 + data.length);
            pkt[0] = 0x00;
            pkt[1] = op;
            pkt[2] = this._short_to_bytearray(data.length)[0];
            pkt[3] = this._short_to_bytearray(data.length)[1];
            pkt[4] = this._int_to_bytearray(chk)[0];
            pkt[5] = this._int_to_bytearray(chk)[1];
            pkt[6] = this._int_to_bytearray(chk)[2];
            pkt[7] = this._int_to_bytearray(chk)[3];
            let i;
            for(i = 0; i < data.length; i++)pkt[8 + i] = data[i];
            await this.transport.write(pkt);
        }
        if (!waitResponse) return [
            0,
            new Uint8Array(0)
        ];
        return this.read_packet(op, timeout);
    }
    async read_reg(addr, timeout = 3000) {
        const pkt = this._int_to_bytearray(addr);
        const val = await this.command(this.ESP_READ_REG, pkt, undefined, undefined, timeout);
        return val[0];
    }
    async write_reg(addr, value, mask = 0xffffffff, delay_us = 0, delay_after_us = 0) {
        let pkt = this._appendArray(this._int_to_bytearray(addr), this._int_to_bytearray(value));
        pkt = this._appendArray(pkt, this._int_to_bytearray(mask));
        pkt = this._appendArray(pkt, this._int_to_bytearray(delay_us));
        if (delay_after_us > 0) {
            pkt = this._appendArray(pkt, this._int_to_bytearray(this.chip.UART_DATE_REG_ADDR));
            pkt = this._appendArray(pkt, this._int_to_bytearray(0));
            pkt = this._appendArray(pkt, this._int_to_bytearray(0));
            pkt = this._appendArray(pkt, this._int_to_bytearray(delay_after_us));
        }
        await this.check_command("write target memory", this.ESP_WRITE_REG, pkt);
    }
    async sync() {
        this.debug("Sync");
        const cmd = new Uint8Array(36);
        let i;
        cmd[0] = 0x07;
        cmd[1] = 0x07;
        cmd[2] = 0x12;
        cmd[3] = 0x20;
        for(i = 0; i < 32; i++)cmd[4 + i] = 0x55;
        try {
            const resp = await this.command(0x08, cmd, undefined, undefined, 100);
            return resp;
        } catch (e) {
            this.debug("Sync err " + e);
            throw e;
        }
    }
    async _connect_attempt(mode = "default_reset", esp32r0_delay = false) {
        this.debug("_connect_attempt " + mode + " " + esp32r0_delay);
        if (mode !== "no_reset") {
            if (this.transport.get_pid() === this.USB_JTAG_SERIAL_PID) // Custom reset sequence, which is required when the device
            // is connecting via its USB-JTAG-Serial peripheral
            await (0, $566461229c0b5a39$export$3252bdf5627aa8a3)(this.transport);
            else {
                const strSequence = esp32r0_delay ? "D0|R1|W100|W2000|D1|R0|W50|D0" : "D0|R1|W100|D1|R0|W50|D0";
                await (0, $566461229c0b5a39$export$e5e6796b349bcc84)(this.transport, strSequence);
            }
        }
        let i = 0;
        let keepReading = true;
        while(keepReading){
            try {
                const res = await this.transport.read(1000);
                i += res.length;
            } catch (e) {
                this.debug(e.message);
                if (e instanceof Error) {
                    keepReading = false;
                    break;
                }
            }
            await this._sleep(50);
        }
        this.transport.slip_reader_enabled = true;
        i = 7;
        while(i--){
            try {
                const resp = await this.sync();
                this.debug(resp[0].toString());
                return "success";
            } catch (error) {
                if (error instanceof Error) {
                    if (esp32r0_delay) this.info("_", false);
                    else this.info(".", false);
                }
            }
            await this._sleep(50);
        }
        return "error";
    }
    async connect(mode = "default_reset", attempts = 7, detecting = false) {
        let i;
        let resp;
        this.info("Connecting...", false);
        await this.transport.connect(this.romBaudrate, this.serialOptions);
        for(i = 0; i < attempts; i++){
            resp = await this._connect_attempt(mode, false);
            if (resp === "success") break;
            resp = await this._connect_attempt(mode, true);
            if (resp === "success") break;
        }
        if (resp !== "success") throw new (0, $07442ebb96f65399$export$5b519f82636185ec)("Failed to connect with the device");
        this.info("\n\r", false);
        if (!detecting) {
            const chip_magic_value = await this.read_reg(0x40001000) >>> 0;
            this.debug("Chip Magic " + chip_magic_value.toString(16));
            const chip = await $45ec30e66bc4f018$var$magic2Chip(chip_magic_value);
            if (this.chip === null) throw new (0, $07442ebb96f65399$export$5b519f82636185ec)(`Unexpected CHIP magic value ${chip_magic_value}. Failed to autodetect chip type.`);
            else this.chip = chip;
        }
    }
    async detect_chip(mode = "default_reset") {
        await this.connect(mode);
        this.info("Detecting chip type... ", false);
        if (this.chip != null) this.info(this.chip.CHIP_NAME);
        else this.info("unknown!");
    }
    async check_command(op_description = "", op = null, data = new Uint8Array(0), chk = 0, timeout = 3000) {
        this.debug("check_command " + op_description);
        const resp = await this.command(op, data, chk, undefined, timeout);
        if (resp[1].length > 4) return resp[1];
        else return resp[0];
    }
    async mem_begin(size, blocks, blocksize, offset) {
        /* XXX: Add check to ensure that STUB is not getting overwritten */ this.debug("mem_begin " + size + " " + blocks + " " + blocksize + " " + offset.toString(16));
        let pkt = this._appendArray(this._int_to_bytearray(size), this._int_to_bytearray(blocks));
        pkt = this._appendArray(pkt, this._int_to_bytearray(blocksize));
        pkt = this._appendArray(pkt, this._int_to_bytearray(offset));
        await this.check_command("enter RAM download mode", this.ESP_MEM_BEGIN, pkt);
    }
    async mem_block(buffer, seq) {
        let pkt = this._appendArray(this._int_to_bytearray(buffer.length), this._int_to_bytearray(seq));
        pkt = this._appendArray(pkt, this._int_to_bytearray(0));
        pkt = this._appendArray(pkt, this._int_to_bytearray(0));
        pkt = this._appendArray(pkt, buffer);
        const checksum = this.checksum(buffer);
        await this.check_command("write to target RAM", this.ESP_MEM_DATA, pkt, checksum);
    }
    async mem_finish(entrypoint) {
        const is_entry = entrypoint === 0 ? 1 : 0;
        const pkt = this._appendArray(this._int_to_bytearray(is_entry), this._int_to_bytearray(entrypoint));
        await this.check_command("leave RAM download mode", this.ESP_MEM_END, pkt, undefined, 50); // XXX: handle non-stub with diff timeout
    }
    async flash_spi_attach(hspi_arg) {
        const pkt = this._int_to_bytearray(hspi_arg);
        await this.check_command("configure SPI flash pins", this.ESP_SPI_ATTACH, pkt);
    }
    async flash_begin(size, offset) {
        const num_blocks = Math.floor((size + this.FLASH_WRITE_SIZE - 1) / this.FLASH_WRITE_SIZE);
        const erase_size = this.chip.get_erase_size(offset, size);
        const d = new Date();
        const t1 = d.getTime();
        let timeout = 3000;
        if (this.IS_STUB == false) timeout = this.timeout_per_mb(this.ERASE_REGION_TIMEOUT_PER_MB, size);
        this.debug("flash begin " + erase_size + " " + num_blocks + " " + this.FLASH_WRITE_SIZE + " " + offset + " " + size);
        let pkt = this._appendArray(this._int_to_bytearray(erase_size), this._int_to_bytearray(num_blocks));
        pkt = this._appendArray(pkt, this._int_to_bytearray(this.FLASH_WRITE_SIZE));
        pkt = this._appendArray(pkt, this._int_to_bytearray(offset));
        if (this.IS_STUB == false) pkt = this._appendArray(pkt, this._int_to_bytearray(0)); // XXX: Support encrypted
        await this.check_command("enter Flash download mode", this.ESP_FLASH_BEGIN, pkt, undefined, timeout);
        const t2 = d.getTime();
        if (size != 0 && this.IS_STUB == false) this.info("Took " + (t2 - t1) / 1000 + "." + (t2 - t1) % 1000 + "s to erase flash block");
        return num_blocks;
    }
    async flash_defl_begin(size, compsize, offset) {
        const num_blocks = Math.floor((compsize + this.FLASH_WRITE_SIZE - 1) / this.FLASH_WRITE_SIZE);
        const erase_blocks = Math.floor((size + this.FLASH_WRITE_SIZE - 1) / this.FLASH_WRITE_SIZE);
        const d = new Date();
        const t1 = d.getTime();
        let write_size, timeout;
        if (this.IS_STUB) {
            write_size = size;
            timeout = 3000;
        } else {
            write_size = erase_blocks * this.FLASH_WRITE_SIZE;
            timeout = this.timeout_per_mb(this.ERASE_REGION_TIMEOUT_PER_MB, write_size);
        }
        this.info("Compressed " + size + " bytes to " + compsize + "...");
        let pkt = this._appendArray(this._int_to_bytearray(write_size), this._int_to_bytearray(num_blocks));
        pkt = this._appendArray(pkt, this._int_to_bytearray(this.FLASH_WRITE_SIZE));
        pkt = this._appendArray(pkt, this._int_to_bytearray(offset));
        if ((this.chip.CHIP_NAME === "ESP32-S2" || this.chip.CHIP_NAME === "ESP32-S3" || this.chip.CHIP_NAME === "ESP32-C3") && this.IS_STUB === false) pkt = this._appendArray(pkt, this._int_to_bytearray(0));
        await this.check_command("enter compressed flash mode", this.ESP_FLASH_DEFL_BEGIN, pkt, undefined, timeout);
        const t2 = d.getTime();
        if (size != 0 && this.IS_STUB === false) this.info("Took " + (t2 - t1) / 1000 + "." + (t2 - t1) % 1000 + "s to erase flash block");
        return num_blocks;
    }
    async flash_block(data, seq, timeout) {
        let pkt = this._appendArray(this._int_to_bytearray(data.length), this._int_to_bytearray(seq));
        pkt = this._appendArray(pkt, this._int_to_bytearray(0));
        pkt = this._appendArray(pkt, this._int_to_bytearray(0));
        pkt = this._appendArray(pkt, data);
        const checksum = this.checksum(data);
        await this.check_command("write to target Flash after seq " + seq, this.ESP_FLASH_DATA, pkt, checksum, timeout);
    }
    async flash_defl_block(data, seq, timeout) {
        let pkt = this._appendArray(this._int_to_bytearray(data.length), this._int_to_bytearray(seq));
        pkt = this._appendArray(pkt, this._int_to_bytearray(0));
        pkt = this._appendArray(pkt, this._int_to_bytearray(0));
        pkt = this._appendArray(pkt, data);
        const checksum = this.checksum(data);
        this.debug("flash_defl_block " + data[0].toString(16) + " " + data[1].toString(16));
        await this.check_command("write compressed data to flash after seq " + seq, this.ESP_FLASH_DEFL_DATA, pkt, checksum, timeout);
    }
    async flash_finish(reboot = false) {
        const val = reboot ? 0 : 1;
        const pkt = this._int_to_bytearray(val);
        await this.check_command("leave Flash mode", this.ESP_FLASH_END, pkt);
    }
    async flash_defl_finish(reboot = false) {
        const val = reboot ? 0 : 1;
        const pkt = this._int_to_bytearray(val);
        await this.check_command("leave compressed flash mode", this.ESP_FLASH_DEFL_END, pkt);
    }
    async run_spiflash_command(spiflash_command, data, read_bits) {
        // SPI_USR register flags
        const SPI_USR_COMMAND = -2147483648;
        const SPI_USR_MISO = 268435456;
        const SPI_USR_MOSI = 134217728;
        // SPI registers, base address differs ESP32* vs 8266
        const base = this.chip.SPI_REG_BASE;
        const SPI_CMD_REG = base + 0x00;
        const SPI_USR_REG = base + this.chip.SPI_USR_OFFS;
        const SPI_USR1_REG = base + this.chip.SPI_USR1_OFFS;
        const SPI_USR2_REG = base + this.chip.SPI_USR2_OFFS;
        const SPI_W0_REG = base + this.chip.SPI_W0_OFFS;
        let set_data_lengths;
        if (this.chip.SPI_MOSI_DLEN_OFFS != null) set_data_lengths = async (mosi_bits, miso_bits)=>{
            const SPI_MOSI_DLEN_REG = base + this.chip.SPI_MOSI_DLEN_OFFS;
            const SPI_MISO_DLEN_REG = base + this.chip.SPI_MISO_DLEN_OFFS;
            if (mosi_bits > 0) await this.write_reg(SPI_MOSI_DLEN_REG, mosi_bits - 1);
            if (miso_bits > 0) await this.write_reg(SPI_MISO_DLEN_REG, miso_bits - 1);
        };
        else set_data_lengths = async (mosi_bits, miso_bits)=>{
            const SPI_DATA_LEN_REG = SPI_USR1_REG;
            const SPI_MOSI_BITLEN_S = 17;
            const SPI_MISO_BITLEN_S = 8;
            const mosi_mask = mosi_bits === 0 ? 0 : mosi_bits - 1;
            const miso_mask = miso_bits === 0 ? 0 : miso_bits - 1;
            const val = miso_mask << SPI_MISO_BITLEN_S | mosi_mask << SPI_MOSI_BITLEN_S;
            await this.write_reg(SPI_DATA_LEN_REG, val);
        };
        const SPI_CMD_USR = 262144;
        const SPI_USR2_COMMAND_LEN_SHIFT = 28;
        if (read_bits > 32) throw new (0, $07442ebb96f65399$export$5b519f82636185ec)("Reading more than 32 bits back from a SPI flash operation is unsupported");
        if (data.length > 64) throw new (0, $07442ebb96f65399$export$5b519f82636185ec)("Writing more than 64 bytes of data with one SPI command is unsupported");
        const data_bits = data.length * 8;
        const old_spi_usr = await this.read_reg(SPI_USR_REG);
        const old_spi_usr2 = await this.read_reg(SPI_USR2_REG);
        let flags = SPI_USR_COMMAND;
        let i;
        if (read_bits > 0) flags |= SPI_USR_MISO;
        if (data_bits > 0) flags |= SPI_USR_MOSI;
        await set_data_lengths(data_bits, read_bits);
        await this.write_reg(SPI_USR_REG, flags);
        let val = 7 << SPI_USR2_COMMAND_LEN_SHIFT | spiflash_command;
        await this.write_reg(SPI_USR2_REG, val);
        if (data_bits == 0) await this.write_reg(SPI_W0_REG, 0);
        else {
            if (data.length % 4 != 0) {
                const padding = new Uint8Array(data.length % 4);
                data = this._appendArray(data, padding);
            }
            let next_reg = SPI_W0_REG;
            for(i = 0; i < data.length - 4; i += 4){
                val = this._bytearray_to_int(data[i], data[i + 1], data[i + 2], data[i + 3]);
                await this.write_reg(next_reg, val);
                next_reg += 4;
            }
        }
        await this.write_reg(SPI_CMD_REG, SPI_CMD_USR);
        for(i = 0; i < 10; i++){
            val = await this.read_reg(SPI_CMD_REG) & SPI_CMD_USR;
            if (val == 0) break;
        }
        if (i === 10) throw new (0, $07442ebb96f65399$export$5b519f82636185ec)("SPI command did not complete in time");
        const stat = await this.read_reg(SPI_W0_REG);
        await this.write_reg(SPI_USR_REG, old_spi_usr);
        await this.write_reg(SPI_USR2_REG, old_spi_usr2);
        return stat;
    }
    async read_flash_id() {
        const SPIFLASH_RDID = 0x9f;
        const pkt = new Uint8Array(0);
        return await this.run_spiflash_command(SPIFLASH_RDID, pkt, 24);
    }
    async erase_flash() {
        this.info("Erasing flash (this may take a while)...");
        let d = new Date();
        const t1 = d.getTime();
        const ret = await this.check_command("erase flash", this.ESP_ERASE_FLASH, undefined, undefined, this.CHIP_ERASE_TIMEOUT);
        d = new Date();
        const t2 = d.getTime();
        this.info("Chip erase completed successfully in " + (t2 - t1) / 1000 + "s");
        return ret;
    }
    toHex(buffer) {
        return Array.prototype.map.call(buffer, (x)=>("00" + x.toString(16)).slice(-2)).join("");
    }
    async flash_md5sum(addr, size) {
        const timeout = this.timeout_per_mb(this.MD5_TIMEOUT_PER_MB, size);
        let pkt = this._appendArray(this._int_to_bytearray(addr), this._int_to_bytearray(size));
        pkt = this._appendArray(pkt, this._int_to_bytearray(0));
        pkt = this._appendArray(pkt, this._int_to_bytearray(0));
        let res = await this.check_command("calculate md5sum", this.ESP_SPI_FLASH_MD5, pkt, undefined, timeout);
        if (res instanceof Uint8Array && res.length > 16) res = res.slice(0, 16);
        const strmd5 = this.toHex(res);
        return strmd5;
    }
    async read_flash(addr, size, onPacketReceived = null) {
        let pkt = this._appendArray(this._int_to_bytearray(addr), this._int_to_bytearray(size));
        pkt = this._appendArray(pkt, this._int_to_bytearray(0x1000));
        pkt = this._appendArray(pkt, this._int_to_bytearray(1024));
        const res = await this.check_command("read flash", this.ESP_READ_FLASH, pkt);
        if (res != 0) throw new (0, $07442ebb96f65399$export$5b519f82636185ec)("Failed to read memory: " + res);
        let resp = new Uint8Array(0);
        while(resp.length < size){
            const packet = await this.transport.read(this.FLASH_READ_TIMEOUT);
            if (packet instanceof Uint8Array) {
                if (packet.length > 0) {
                    resp = this._appendArray(resp, packet);
                    await this.transport.write(this._int_to_bytearray(resp.length));
                    if (onPacketReceived) onPacketReceived(packet, resp.length, size);
                }
            } else throw new (0, $07442ebb96f65399$export$5b519f82636185ec)("Failed to read memory: " + packet);
        }
        return resp;
    }
    async run_stub() {
        this.info("Uploading stub...");
        let decoded = atob(this.chip.ROM_TEXT);
        let chardata = decoded.split("").map(function(x) {
            return x.charCodeAt(0);
        });
        const text = new Uint8Array(chardata);
        decoded = atob(this.chip.ROM_DATA);
        chardata = decoded.split("").map(function(x) {
            return x.charCodeAt(0);
        });
        const data = new Uint8Array(chardata);
        let blocks = Math.floor((text.length + this.ESP_RAM_BLOCK - 1) / this.ESP_RAM_BLOCK);
        let i;
        await this.mem_begin(text.length, blocks, this.ESP_RAM_BLOCK, this.chip.TEXT_START);
        for(i = 0; i < blocks; i++){
            const from_offs = i * this.ESP_RAM_BLOCK;
            const to_offs = from_offs + this.ESP_RAM_BLOCK;
            await this.mem_block(text.slice(from_offs, to_offs), i);
        }
        blocks = Math.floor((data.length + this.ESP_RAM_BLOCK - 1) / this.ESP_RAM_BLOCK);
        await this.mem_begin(data.length, blocks, this.ESP_RAM_BLOCK, this.chip.DATA_START);
        for(i = 0; i < blocks; i++){
            const from_offs = i * this.ESP_RAM_BLOCK;
            const to_offs = from_offs + this.ESP_RAM_BLOCK;
            await this.mem_block(data.slice(from_offs, to_offs), i);
        }
        this.info("Running stub...");
        await this.mem_finish(this.chip.ENTRY);
        // Check up-to next 100 packets to see if stub is running
        for(let i = 0; i < 100; i++){
            const res = await this.transport.read(1000, 6);
            if (res[0] === 79 && res[1] === 72 && res[2] === 65 && res[3] === 73) {
                this.info("Stub running...");
                this.IS_STUB = true;
                this.FLASH_WRITE_SIZE = 0x4000;
                return this.chip;
            }
        }
        throw new (0, $07442ebb96f65399$export$5b519f82636185ec)("Failed to start stub. Unexpected response");
    }
    async change_baud() {
        this.info("Changing baudrate to " + this.baudrate);
        const second_arg = this.IS_STUB ? this.transport.baudrate : 0;
        const pkt = this._appendArray(this._int_to_bytearray(this.baudrate), this._int_to_bytearray(second_arg));
        const resp = await this.command(this.ESP_CHANGE_BAUDRATE, pkt);
        this.debug(resp[0].toString());
        this.info("Changed");
        await this.transport.disconnect();
        await this._sleep(50);
        await this.transport.connect(this.baudrate, this.serialOptions);
        /* original code seemed absolutely unreliable. use retries and less sleep */ try {
            let i = 64;
            while(i--){
                try {
                    await this.sync();
                    break;
                } catch (error) {
                    this.debug(error.message);
                }
                await this._sleep(10);
            }
        } catch (e) {
            this.debug(e.message);
        }
    }
    async main_fn(mode = "default_reset") {
        await this.detect_chip(mode);
        const chip = await this.chip.get_chip_description(this);
        this.info("Chip is " + chip);
        this.info("Features: " + await this.chip.get_chip_features(this));
        this.info("Crystal is " + await this.chip.get_crystal_freq(this) + "MHz");
        this.info("MAC: " + await this.chip.read_mac(this));
        await this.chip.read_mac(this);
        if (typeof this.chip._post_connect != "undefined") await this.chip._post_connect(this);
        await this.run_stub();
        if (this.romBaudrate !== this.baudrate) await this.change_baud();
        return chip;
    }
    parse_flash_size_arg(flsz) {
        if (typeof this.chip.FLASH_SIZES[flsz] === "undefined") throw new (0, $07442ebb96f65399$export$5b519f82636185ec)("Flash size " + flsz + " is not supported by this chip type. Supported sizes: " + this.chip.FLASH_SIZES);
        return this.chip.FLASH_SIZES[flsz];
    }
    _update_image_flash_params(image, address, flash_size, flash_mode, flash_freq) {
        this.debug("_update_image_flash_params " + flash_size + " " + flash_mode + " " + flash_freq);
        if (image.length < 8) return image;
        if (address != this.chip.BOOTLOADER_FLASH_OFFSET) return image;
        if (flash_size === "keep" && flash_mode === "keep" && flash_freq === "keep") {
            this.info("Not changing the image");
            return image;
        }
        const magic = parseInt(image[0]);
        let a_flash_mode = parseInt(image[2]);
        const flash_size_freq = parseInt(image[3]);
        if (magic !== this.ESP_IMAGE_MAGIC) {
            this.info("Warning: Image file at 0x" + address.toString(16) + " doesn't look like an image file, so not changing any flash settings.");
            return image;
        }
        /* XXX: Yet to implement actual image verification */ if (flash_mode !== "keep") {
            const flash_modes = {
                qio: 0,
                qout: 1,
                dio: 2,
                dout: 3
            };
            a_flash_mode = flash_modes[flash_mode];
        }
        let a_flash_freq = flash_size_freq & 0x0f;
        if (flash_freq !== "keep") {
            const flash_freqs = {
                "40m": 0,
                "26m": 1,
                "20m": 2,
                "80m": 0xf
            };
            a_flash_freq = flash_freqs[flash_freq];
        }
        let a_flash_size = flash_size_freq & 0xf0;
        if (flash_size !== "keep") a_flash_size = this.parse_flash_size_arg(flash_size);
        const flash_params = a_flash_mode << 8 | a_flash_freq + a_flash_size;
        this.info("Flash params set to " + flash_params.toString(16));
        if (parseInt(image[2]) !== a_flash_mode << 8) image = image.substring(0, 2) + (a_flash_mode << 8).toString() + image.substring(3);
        if (parseInt(image[3]) !== a_flash_freq + a_flash_size) image = image.substring(0, 3) + (a_flash_freq + a_flash_size).toString() + image.substring(4);
        return image;
    }
    async write_flash(options) {
        this.debug("EspLoader program");
        if (options.flashSize !== "keep") {
            const flash_end = this.flash_size_bytes(options.flashSize);
            for(let i = 0; i < options.fileArray.length; i++){
                if (options.fileArray[i].data.length + options.fileArray[i].address > flash_end) throw new (0, $07442ebb96f65399$export$5b519f82636185ec)(`File ${i + 1} doesn't fit in the available flash`);
            }
        }
        if (this.IS_STUB === true && options.eraseAll === true) await this.erase_flash();
        let image, address;
        for(let i = 0; i < options.fileArray.length; i++){
            this.debug("Data Length " + options.fileArray[i].data.length);
            image = options.fileArray[i].data;
            const reminder = options.fileArray[i].data.length % 4;
            if (reminder > 0) image += "\xff\xff\xff\xff".substring(4 - reminder);
            address = options.fileArray[i].address;
            this.debug("Image Length " + image.length);
            if (image.length === 0) {
                this.debug("Warning: File is empty");
                continue;
            }
            image = this._update_image_flash_params(image, address, options.flashSize, options.flashMode, options.flashFreq);
            let calcmd5 = null;
            if (options.calculateMD5Hash) {
                calcmd5 = options.calculateMD5Hash(image);
                this.debug("Image MD5 " + calcmd5);
            }
            const uncsize = image.length;
            let blocks;
            if (options.compress) {
                const uncimage = this.bstrToUi8(image);
                image = this.ui8ToBstr((0, $1d314fc79f930156$export$2316623ecd1285ab)(uncimage, {
                    level: 9
                }));
                blocks = await this.flash_defl_begin(uncsize, image.length, address);
            } else blocks = await this.flash_begin(uncsize, address);
            let seq = 0;
            let bytes_sent = 0;
            const totalBytes = image.length;
            if (options.reportProgress) options.reportProgress(i, 0, totalBytes);
            let d = new Date();
            const t1 = d.getTime();
            let timeout = 5000;
            // Create a decompressor to keep track of the size of uncompressed data
            // to be written in each chunk.
            const inflate = new (0, $1d314fc79f930156$export$d1de70a877d6e43c)({
                chunkSize: 1
            });
            let total_len_uncompressed = 0;
            inflate.onData = function(chunk) {
                total_len_uncompressed += chunk.byteLength;
            };
            while(image.length > 0){
                this.debug("Write loop " + address + " " + seq + " " + blocks);
                this.info("Writing at 0x" + (address + total_len_uncompressed).toString(16) + "... (" + Math.floor(100 * (seq + 1) / blocks) + "%)");
                const block = this.bstrToUi8(image.slice(0, this.FLASH_WRITE_SIZE));
                if (options.compress) {
                    const len_uncompressed_previous = total_len_uncompressed;
                    inflate.push(block, false);
                    const block_uncompressed = total_len_uncompressed - len_uncompressed_previous;
                    let block_timeout = 3000;
                    if (this.timeout_per_mb(this.ERASE_WRITE_TIMEOUT_PER_MB, block_uncompressed) > 3000) block_timeout = this.timeout_per_mb(this.ERASE_WRITE_TIMEOUT_PER_MB, block_uncompressed);
                    if (this.IS_STUB === false) // ROM code writes block to flash before ACKing
                    timeout = block_timeout;
                    await this.flash_defl_block(block, seq, timeout);
                    if (this.IS_STUB) // Stub ACKs when block is received, then writes to flash while receiving the block after it
                    timeout = block_timeout;
                } else throw new (0, $07442ebb96f65399$export$5b519f82636185ec)("Yet to handle Non Compressed writes");
                bytes_sent += block.length;
                image = image.slice(this.FLASH_WRITE_SIZE, image.length);
                seq++;
                if (options.reportProgress) options.reportProgress(i, bytes_sent, totalBytes);
            }
            if (this.IS_STUB) await this.read_reg(this.CHIP_DETECT_MAGIC_REG_ADDR, timeout);
            d = new Date();
            const t = d.getTime() - t1;
            if (options.compress) this.info("Wrote " + uncsize + " bytes (" + bytes_sent + " compressed) at 0x" + address.toString(16) + " in " + t / 1000 + " seconds.");
            if (calcmd5) {
                const res = await this.flash_md5sum(address, uncsize);
                if (new String(res).valueOf() != new String(calcmd5).valueOf()) {
                    this.info("File  md5: " + calcmd5);
                    this.info("Flash md5: " + res);
                    throw new (0, $07442ebb96f65399$export$5b519f82636185ec)("MD5 of file does not match data in flash!");
                } else this.info("Hash of data verified.");
            }
        }
        this.info("Leaving...");
        if (this.IS_STUB) {
            await this.flash_begin(0, 0);
            if (options.compress) await this.flash_defl_finish();
            else await this.flash_finish();
        }
    }
    async flash_id() {
        this.debug("flash_id");
        const flashid = await this.read_flash_id();
        this.info("Manufacturer: " + (flashid & 0xff).toString(16));
        const flid_lowbyte = flashid >> 16 & 0xff;
        this.info("Device: " + (flashid >> 8 & 0xff).toString(16) + flid_lowbyte.toString(16));
        this.info("Detected flash size: " + this.DETECTED_FLASH_SIZES[flid_lowbyte]);
    }
    async get_flash_size() {
        this.debug("flash_id");
        const flashid = await this.read_flash_id();
        const flid_lowbyte = flashid >> 16 & 0xff;
        return this.DETECTED_FLASH_SIZES_NUM[flid_lowbyte];
    }
    async hard_reset() {
        await this.transport.setRTS(true); // EN->LOW
        await this._sleep(100);
        await this.transport.setRTS(false);
    }
    async soft_reset() {
        if (!this.IS_STUB) {
            // "run user code" is as close to a soft reset as we can do
            await this.flash_begin(0, 0);
            await this.flash_finish(false);
        } else if (this.chip.CHIP_NAME != "ESP8266") throw new (0, $07442ebb96f65399$export$5b519f82636185ec)("Soft resetting is currently only supported on ESP8266");
        else // running user code from stub loader requires some hacks
        // in the stub loader
        await this.command(this.ESP_RUN_USER_CODE, undefined, undefined, false);
    }
}





const $382e02c9bbd5d50b$var$baudrates = document.getElementById("baudrates");
const $382e02c9bbd5d50b$var$connectButton = document.getElementById("connectButton");
const $382e02c9bbd5d50b$var$disconnectButton = document.getElementById("disconnectButton");
const $382e02c9bbd5d50b$var$resetButton = document.getElementById("resetButton");
const $382e02c9bbd5d50b$var$consoleStartButton = document.getElementById("consoleStartButton");
const $382e02c9bbd5d50b$var$consoleStopButton = document.getElementById("consoleStopButton");
const $382e02c9bbd5d50b$var$eraseButton = document.getElementById("eraseButton");
const $382e02c9bbd5d50b$var$programButton = document.getElementById("programButton");
const $382e02c9bbd5d50b$var$filesDiv = document.getElementById("files");
const $382e02c9bbd5d50b$var$terminal = document.getElementById("terminal");
const $382e02c9bbd5d50b$var$programDiv = document.getElementById("program");
const $382e02c9bbd5d50b$var$consoleDiv = document.getElementById("console");
const $382e02c9bbd5d50b$var$lblBaudrate = document.getElementById("lblBaudrate");
const $382e02c9bbd5d50b$var$lblConsoleFor = document.getElementById("lblConsoleFor");
const $382e02c9bbd5d50b$var$lblConnTo = document.getElementById("lblConnTo");
const $382e02c9bbd5d50b$var$table = document.getElementById("fileTable");
const $382e02c9bbd5d50b$var$alertDiv = document.getElementById("alertDiv");
const $382e02c9bbd5d50b$var$term = new Terminal({
    cols: 120,
    rows: 40
});
$382e02c9bbd5d50b$var$term.open($382e02c9bbd5d50b$var$terminal);
let $382e02c9bbd5d50b$var$device = null;
let $382e02c9bbd5d50b$var$transport;
let $382e02c9bbd5d50b$var$chip = null;
let $382e02c9bbd5d50b$var$esploader;
$382e02c9bbd5d50b$var$disconnectButton.style.display = "none";
$382e02c9bbd5d50b$var$eraseButton.style.display = "none";
$382e02c9bbd5d50b$var$consoleStopButton.style.display = "none";
$382e02c9bbd5d50b$var$filesDiv.style.display = "none";
/**
 * The built in Event object.
 * @external Event
 * @see {@link https://developer.mozilla.org/en-US/docs/Web/API/Event}
 */ /**
 * File reader handler to read given local file.
 * @param {Event} evt File Select event
 */ function $382e02c9bbd5d50b$var$handleFileSelect(evt) {
    const file = evt.target.files[0];
    if (!file) return;
    const reader = new FileReader();
    reader.onload = (ev)=>{
        evt.target.data = ev.target.result;
    };
    reader.readAsBinaryString(file);
}
const $382e02c9bbd5d50b$var$espLoaderTerminal = {
    clean () {
        $382e02c9bbd5d50b$var$term.clear();
    },
    writeLine (data) {
        $382e02c9bbd5d50b$var$term.writeln(data);
    },
    write (data) {
        $382e02c9bbd5d50b$var$term.write(data);
    }
};
$382e02c9bbd5d50b$var$connectButton.onclick = async ()=>{
    if ($382e02c9bbd5d50b$var$device === null) {
        $382e02c9bbd5d50b$var$device = await navigator.serial.requestPort({});
        $382e02c9bbd5d50b$var$transport = new (0, $a0cf3cac0d21f701$export$86495b081fef8e52)($382e02c9bbd5d50b$var$device);
    }
    try {
        const flashOptions = {
            transport: $382e02c9bbd5d50b$var$transport,
            baudrate: parseInt($382e02c9bbd5d50b$var$baudrates.value),
            terminal: $382e02c9bbd5d50b$var$espLoaderTerminal
        };
        $382e02c9bbd5d50b$var$esploader = new (0, $45ec30e66bc4f018$export$b0f7a6c745790308)(flashOptions);
        $382e02c9bbd5d50b$var$chip = await $382e02c9bbd5d50b$var$esploader.main_fn();
    // Temporarily broken
    // await esploader.flash_id();
    } catch (e) {
        console.error(e);
        $382e02c9bbd5d50b$var$term.writeln(`Error: ${e.message}`);
    }
    console.log("Settings done for :" + $382e02c9bbd5d50b$var$chip);
    $382e02c9bbd5d50b$var$lblBaudrate.style.display = "none";
    $382e02c9bbd5d50b$var$lblConnTo.innerHTML = "Connected to device: " + $382e02c9bbd5d50b$var$chip;
    $382e02c9bbd5d50b$var$lblConnTo.style.display = "block";
    $382e02c9bbd5d50b$var$baudrates.style.display = "none";
    $382e02c9bbd5d50b$var$connectButton.style.display = "none";
    $382e02c9bbd5d50b$var$disconnectButton.style.display = "initial";
    $382e02c9bbd5d50b$var$eraseButton.style.display = "initial";
    $382e02c9bbd5d50b$var$filesDiv.style.display = "initial";
    $382e02c9bbd5d50b$var$consoleDiv.style.display = "none";
};
$382e02c9bbd5d50b$var$resetButton.onclick = async ()=>{
    if ($382e02c9bbd5d50b$var$device === null) {
        $382e02c9bbd5d50b$var$device = await navigator.serial.requestPort({});
        $382e02c9bbd5d50b$var$transport = new (0, $a0cf3cac0d21f701$export$86495b081fef8e52)($382e02c9bbd5d50b$var$device);
    }
    await $382e02c9bbd5d50b$var$transport.setDTR(false);
    await new Promise((resolve)=>setTimeout(resolve, 100));
    await $382e02c9bbd5d50b$var$transport.setDTR(true);
};
function $382e02c9bbd5d50b$var$addFile() {
    const rowCount = $382e02c9bbd5d50b$var$table.rows.length;
    const row = $382e02c9bbd5d50b$var$table.insertRow(rowCount);
    // Column 2 - File selector
    const cell2 = row.insertCell(0);
    const element2 = document.createElement("input");
    element2.type = "file";
    element2.id = "selectFile" + rowCount;
    element2.name = "selected_File" + rowCount;
    element2.addEventListener("change", $382e02c9bbd5d50b$var$handleFileSelect, false);
    cell2.appendChild(element2);
    // Column 3  - Progress
    const cell3 = row.insertCell(1);
    cell3.classList.add("progress-cell");
    cell3.style.display = "none";
    cell3.innerHTML = `<progress value="0" max="100"></progress>`;
}
/**
 * The built in HTMLTableRowElement object.
 * @external HTMLTableRowElement
 * @see {@link https://developer.mozilla.org/en-US/docs/Web/API/HTMLTableRowElement}
 */ /**
 * Clean devices variables on chip disconnect. Remove stale references if any.
 */ function $382e02c9bbd5d50b$var$cleanUp() {
    $382e02c9bbd5d50b$var$device = null;
    $382e02c9bbd5d50b$var$transport = null;
    $382e02c9bbd5d50b$var$chip = null;
}
$382e02c9bbd5d50b$var$disconnectButton.onclick = async ()=>{
    if ($382e02c9bbd5d50b$var$transport) await $382e02c9bbd5d50b$var$transport.disconnect();
    $382e02c9bbd5d50b$var$term.clear();
    $382e02c9bbd5d50b$var$baudrates.style.display = "initial";
    $382e02c9bbd5d50b$var$connectButton.style.display = "initial";
    $382e02c9bbd5d50b$var$disconnectButton.style.display = "none";
    $382e02c9bbd5d50b$var$eraseButton.style.display = "none";
    $382e02c9bbd5d50b$var$lblConnTo.style.display = "none";
    $382e02c9bbd5d50b$var$filesDiv.style.display = "none";
    $382e02c9bbd5d50b$var$alertDiv.style.display = "none";
    $382e02c9bbd5d50b$var$consoleDiv.style.display = "initial";
    $382e02c9bbd5d50b$var$cleanUp();
};
let $382e02c9bbd5d50b$var$isConsoleClosed = false;
$382e02c9bbd5d50b$var$consoleStartButton.onclick = async ()=>{
    if ($382e02c9bbd5d50b$var$device === null) {
        $382e02c9bbd5d50b$var$device = await navigator.serial.requestPort({});
        $382e02c9bbd5d50b$var$transport = new (0, $a0cf3cac0d21f701$export$86495b081fef8e52)($382e02c9bbd5d50b$var$device);
    }
    $382e02c9bbd5d50b$var$lblConsoleFor.style.display = "block";
    $382e02c9bbd5d50b$var$consoleStartButton.style.display = "none";
    $382e02c9bbd5d50b$var$consoleStopButton.style.display = "initial";
    $382e02c9bbd5d50b$var$programDiv.style.display = "none";
    await $382e02c9bbd5d50b$var$transport.connect();
    $382e02c9bbd5d50b$var$isConsoleClosed = false;
    while(!$382e02c9bbd5d50b$var$isConsoleClosed){
        const val = await $382e02c9bbd5d50b$var$transport.rawRead();
        if (typeof val !== "undefined") $382e02c9bbd5d50b$var$term.write(val);
        else break;
    }
    console.log("quitting console");
};
$382e02c9bbd5d50b$var$consoleStopButton.onclick = async ()=>{
    $382e02c9bbd5d50b$var$isConsoleClosed = true;
    await $382e02c9bbd5d50b$var$transport.disconnect();
    await $382e02c9bbd5d50b$var$transport.waitForUnlock(1500);
    $382e02c9bbd5d50b$var$term.clear();
    $382e02c9bbd5d50b$var$consoleStartButton.style.display = "initial";
    $382e02c9bbd5d50b$var$consoleStopButton.style.display = "none";
    $382e02c9bbd5d50b$var$programDiv.style.display = "initial";
};
/**
 * Validate the provided files images and offset to see if they're valid.
 * @returns {string} Program input validation result
 */ function $382e02c9bbd5d50b$var$validateProgramInputs() {
    const offsetArr = [];
    const rowCount = $382e02c9bbd5d50b$var$table.rows.length;
    let row;
    let offset = 0;
    let fileData = null;
    // check for mandatory fields
    for(let index = 0; index < rowCount; index++){
        row = $382e02c9bbd5d50b$var$table.rows[index];
        offset = 0x10000;
        // Non-numeric or blank offset
        if (Number.isNaN(offset)) return "Offset field in row " + index + " is not a valid address!";
        else if (offsetArr.includes(offset)) return "Offset field in row " + index + " is already in use!";
        else offsetArr.push(offset);
        const fileObj = row.cells[0].childNodes[0];
        fileData = fileObj.data;
        if (fileData == null) return "No file selected!";
    }
    return "success";
}
async function $382e02c9bbd5d50b$var$getFileData(fileUrl) {
    const response = await fetch(fileUrl);
    const data = await response.blob();
    const metadata = {
        type: "application/octet-stream"
    };
    const file = new File([
        data
    ], "file.bin", metadata);
    const readFileData = (file)=>{
        const reader = new FileReader();
        return new Promise((resolve, reject)=>{
            reader.onerror = ()=>{
                reader.abort();
                reject(new DOMException("Problem parsing input file."));
            };
            reader.onload = (ev)=>{
                resolve(ev.target.result);
            };
            reader.readAsBinaryString(file);
        });
    };
    return await readFileData(file);
}
$382e02c9bbd5d50b$var$programButton.onclick = async ()=>{
    const alertMsg = document.getElementById("alertmsg");
    const err = $382e02c9bbd5d50b$var$validateProgramInputs();
    if (err != "success") {
        alertMsg.innerHTML = "<strong>" + err + "</strong>";
        $382e02c9bbd5d50b$var$alertDiv.style.display = "block";
        return;
    }
    // Hide error message
    $382e02c9bbd5d50b$var$alertDiv.style.display = "none";
    const fileArray = [];
    const progressBars = [];
    fileArray.push({
        data: $382e02c9bbd5d50b$var$getFileData("./bootloader.bin"),
        address: 0x1000
    });
    fileArray.push({
        data: $382e02c9bbd5d50b$var$getFileData("./partitions.bin"),
        address: 0x8000
    });
    for(let index = 0; index < $382e02c9bbd5d50b$var$table.rows.length; index++){
        const row = $382e02c9bbd5d50b$var$table.rows[index];
        const offset = 0x10000;
        const fileObj = row.cells[0].childNodes[0];
        const progressBar = row.cells[1].childNodes[0];
        progressBar.textContent = "0";
        progressBars.push(progressBar);
        row.cells[0].style.display = "initial";
        row.cells[1].style.display = "none";
        fileArray.push({
            data: fileObj.data,
            address: offset
        });
    }
    try {
        const flashOptions = {
            fileArray: fileArray,
            flashSize: "keep",
            eraseAll: false,
            compress: true,
            reportProgress: (fileIndex, written, total)=>{
            //progressBars[fileIndex].value = (written / total) * 100;
            },
            calculateMD5Hash: (image)=>CryptoJS.MD5(CryptoJS.enc.Latin1.parse(image))
        };
        await $382e02c9bbd5d50b$var$esploader.write_flash(flashOptions);
    } catch (e) {
        console.error(e);
        $382e02c9bbd5d50b$var$term.writeln(`Error: ${e.message}`);
    } finally{
        // Hide progress bars and show erase buttons
        for(let index = 1; index < $382e02c9bbd5d50b$var$table.rows.length; index++)$382e02c9bbd5d50b$var$table.rows[index].cells[1].style.display = "none";
    }
};
$382e02c9bbd5d50b$var$addFile();


//# sourceMappingURL=index.0f6ac79c.js.map
