/*
 * Copyright (c) 1999 Apple Computer, Inc. All rights reserved.
 *
 * @APPLE_LICENSE_HEADER_START@
 *
 * The contents of this file constitute Original Code as defined in and
 * are subject to the Apple Public Source License Version 1.1 (the
 * "License").  You may not use this file except in compliance with the
 * License.  Please obtain a copy of the License at
 * http://www.apple.com/publicsource and read it before using this file.
 *
 * This Original Code and all software distributed under the License are
 * distributed on an "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, EITHER
 * EXPRESS OR IMPLIED, AND APPLE HEREBY DISCLAIMS ALL SUCH WARRANTIES,
 * INCLUDING WITHOUT LIMITATION, ANY WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE OR NON-INFRINGEMENT.  Please see the
 * License for the specific language governing rights and limitations
 * under the License.
 *
 * @APPLE_LICENSE_HEADER_END@
 */
/*
 * Copyright (c) 1987, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 *PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE
 *LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 *CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 *SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 *INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 *CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 *ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 *POSSIBILITY OF SUCH DAMAGE.
 */

//#include <sys/cdefs.h>
//#include <string.h>

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)strcasecmp.c	8.1 (Berkeley) 6/4/93";
#endif /* LIBC_SCCS and not lint */

typedef unsigned char u_char;

/*
 * This array is designed for mapping upper and lower case letter
 * together for a case independent comparison.  The mappings are
 * based upon ascii character sequences.
 */
static const u_char charmap[] = {
  (u_char)'\000',
  (u_char)'\001',
  (u_char)'\002',
  (u_char)'\003',
  (u_char)'\004',
  (u_char)'\005',
  (u_char)'\006',
  (u_char)'\007',
  (u_char)'\010',
  (u_char)'\011',
  (u_char)'\012',
  (u_char)'\013',
  (u_char)'\014',
  (u_char)'\015',
  (u_char)'\016',
  (u_char)'\017',
  (u_char)'\020',
  (u_char)'\021',
  (u_char)'\022',
  (u_char)'\023',
  (u_char)'\024',
  (u_char)'\025',
  (u_char)'\026',
  (u_char)'\027',
  (u_char)'\030',
  (u_char)'\031',
  (u_char)'\032',
  (u_char)'\033',
  (u_char)'\034',
  (u_char)'\035',
  (u_char)'\036',
  (u_char)'\037',
  (u_char)'\040',
  (u_char)'\041',
  (u_char)'\042',
  (u_char)'\043',
  (u_char)'\044',
  (u_char)'\045',
  (u_char)'\046',
  (u_char)'\047',
  (u_char)'\050',
  (u_char)'\051',
  (u_char)'\052',
  (u_char)'\053',
  (u_char)'\054',
  (u_char)'\055',
  (u_char)'\056',
  (u_char)'\057',
  (u_char)'\060',
  (u_char)'\061',
  (u_char)'\062',
  (u_char)'\063',
  (u_char)'\064',
  (u_char)'\065',
  (u_char)'\066',
  (u_char)'\067',
  (u_char)'\070',
  (u_char)'\071',
  (u_char)'\072',
  (u_char)'\073',
  (u_char)'\074',
  (u_char)'\075',
  (u_char)'\076',
  (u_char)'\077',
  (u_char)'\100',
  (u_char)'\141',
  (u_char)'\142',
  (u_char)'\143',
  (u_char)'\144',
  (u_char)'\145',
  (u_char)'\146',
  (u_char)'\147',
  (u_char)'\150',
  (u_char)'\151',
  (u_char)'\152',
  (u_char)'\153',
  (u_char)'\154',
  (u_char)'\155',
  (u_char)'\156',
  (u_char)'\157',
  (u_char)'\160',
  (u_char)'\161',
  (u_char)'\162',
  (u_char)'\163',
  (u_char)'\164',
  (u_char)'\165',
  (u_char)'\166',
  (u_char)'\167',
  (u_char)'\170',
  (u_char)'\171',
  (u_char)'\172',
  (u_char)'\133',
  (u_char)'\134',
  (u_char)'\135',
  (u_char)'\136',
  (u_char)'\137',
  (u_char)'\140',
  (u_char)'\141',
  (u_char)'\142',
  (u_char)'\143',
  (u_char)'\144',
  (u_char)'\145',
  (u_char)'\146',
  (u_char)'\147',
  (u_char)'\150',
  (u_char)'\151',
  (u_char)'\152',
  (u_char)'\153',
  (u_char)'\154',
  (u_char)'\155',
  (u_char)'\156',
  (u_char)'\157',
  (u_char)'\160',
  (u_char)'\161',
  (u_char)'\162',
  (u_char)'\163',
  (u_char)'\164',
  (u_char)'\165',
  (u_char)'\166',
  (u_char)'\167',
  (u_char)'\170',
  (u_char)'\171',
  (u_char)'\172',
  (u_char)'\173',
  (u_char)'\174',
  (u_char)'\175',
  (u_char)'\176',
  (u_char)'\177',
  (u_char)'\200',
  (u_char)'\201',
  (u_char)'\202',
  (u_char)'\203',
  (u_char)'\204',
  (u_char)'\205',
  (u_char)'\206',
  (u_char)'\207',
  (u_char)'\210',
  (u_char)'\211',
  (u_char)'\212',
  (u_char)'\213',
  (u_char)'\214',
  (u_char)'\215',
  (u_char)'\216',
  (u_char)'\217',
  (u_char)'\220',
  (u_char)'\221',
  (u_char)'\222',
  (u_char)'\223',
  (u_char)'\224',
  (u_char)'\225',
  (u_char)'\226',
  (u_char)'\227',
  (u_char)'\230',
  (u_char)'\231',
  (u_char)'\232',
  (u_char)'\233',
  (u_char)'\234',
  (u_char)'\235',
  (u_char)'\236',
  (u_char)'\237',
  (u_char)'\240',
  (u_char)'\241',
  (u_char)'\242',
  (u_char)'\243',
  (u_char)'\244',
  (u_char)'\245',
  (u_char)'\246',
  (u_char)'\247',
  (u_char)'\250',
  (u_char)'\251',
  (u_char)'\252',
  (u_char)'\253',
  (u_char)'\254',
  (u_char)'\255',
  (u_char)'\256',
  (u_char)'\257',
  (u_char)'\260',
  (u_char)'\261',
  (u_char)'\262',
  (u_char)'\263',
  (u_char)'\264',
  (u_char)'\265',
  (u_char)'\266',
  (u_char)'\267',
  (u_char)'\270',
  (u_char)'\271',
  (u_char)'\272',
  (u_char)'\273',
  (u_char)'\274',
  (u_char)'\275',
  (u_char)'\276',
  (u_char)'\277',
  (u_char)'\300',
  (u_char)'\301',
  (u_char)'\302',
  (u_char)'\303',
  (u_char)'\304',
  (u_char)'\305',
  (u_char)'\306',
  (u_char)'\307',
  (u_char)'\310',
  (u_char)'\311',
  (u_char)'\312',
  (u_char)'\313',
  (u_char)'\314',
  (u_char)'\315',
  (u_char)'\316',
  (u_char)'\317',
  (u_char)'\320',
  (u_char)'\321',
  (u_char)'\322',
  (u_char)'\323',
  (u_char)'\324',
  (u_char)'\325',
  (u_char)'\326',
  (u_char)'\327',
  (u_char)'\330',
  (u_char)'\331',
  (u_char)'\332',
  (u_char)'\333',
  (u_char)'\334',
  (u_char)'\335',
  (u_char)'\336',
  (u_char)'\337',
  (u_char)'\340',
  (u_char)'\341',
  (u_char)'\342',
  (u_char)'\343',
  (u_char)'\344',
  (u_char)'\345',
  (u_char)'\346',
  (u_char)'\347',
  (u_char)'\350',
  (u_char)'\351',
  (u_char)'\352',
  (u_char)'\353',
  (u_char)'\354',
  (u_char)'\355',
  (u_char)'\356',
  (u_char)'\357',
  (u_char)'\360',
  (u_char)'\361',
  (u_char)'\362',
  (u_char)'\363',
  (u_char)'\364',
  (u_char)'\365',
  (u_char)'\366',
  (u_char)'\367',
  (u_char)'\370',
  (u_char)'\371',
  (u_char)'\372',
  (u_char)'\373',
  (u_char)'\374',
  (u_char)'\375',
  (u_char)'\376',
  (u_char)'\377',
};

monostatic int strcasecmp(const char* s1, const char* s2) {
  register const u_char *cm = charmap, *us1 = (const u_char*)s1,
                        *us2 = (const u_char*)s2;
  while (cm[*us1] == cm[*us2++])
    if (*us1++ == '\0') return (0);
  return cm[*us1] - cm[*--us2];
}

monostatic int strncasecmp(const char* s1, const char* s2, size_t n) {
  if (n) {
    register const u_char *cm = charmap, *us1 = (const u_char*)s1,
                          *us2 = (const u_char*)s2;
    do {
      if (cm[*us1] != cm[*us2++]) return cm[*us1] - cm[*--us2];
      if (*us1++ == '\0') break;
    } while (--n != 0);
  }
  return 0;
}
