$(
#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#
    Metamath source file for logic, set theory, numbers, and Hilbert space
#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#

set.mm - Version of 13-Dec-2004

                             PUBLIC DOMAIN

This file (specifically, the version of this file with the above date)
is placed in the public domain per the Creative Commons Public Domain
Dedication. http://creativecommons.org/licenses/publicdomain/

Norman Megill - email: nm(at)alum(dot)mit(dot)edu

=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
                          Recent label changes
=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

This is part of an ongoing project to improve naming consistency.  If
you are using set.mm as the base for your project, you should review the
following changes before updating to this version of set.mm.  If you
want to create an automated script, you can make can make global
substitutions into your database by processing the ones without "Notes"
in _reverse_ order and matching a space or beginning-of-line before the
label and a space or end-of-line after the label.  The ones with "Notes"
should be processed manually.  If you have suggestions for better names
let me know.

Date       Old       New         Notes
12-Dec-04  mpand     mpdan
12-Dec-04  ontr      ontr1
 6-Dec-04  on0eqel   on0eqelt
30-Nov-04  exp2      exp2t
30-Nov-04  1exp      1expt
30-Nov-04  0exp      0expt
30-Nov-04  exp1      exp1t
30-Nov-04  expp1     expp1t
18-Nov-04  divrclz   redivclz
18-Nov-04  divrclt   redivclt
18-Nov-04  subrclt   resubclt
18-Nov-04  negrclt   renegclt
18-Nov-04  mulrclt   remulclt
18-Nov-04  divrcl    redivcl
18-Nov-04  subrcl    resubcl
18-Nov-04  negrcl    renegcl
18-Nov-04  mulrcl    remulcl
18-Nov-04  addrcl    readdcl
18-Nov-04  ltdivmul  ---         obsolete; use ltmuldiv instead
18-Nov-04  ltdivmult ---         obsolete; use ltmuldivt instead (note: there
                                 is a new ltdivmult that is unrelated)
18-Nov-04  distr2t   adddirt
18-Nov-04  distr2    adddir
18-Nov-04  distr     adddi
18-Nov-04  subdistr  subdir
17-Nov-04  nnrect    nnrecgt0t
17-Nov-04  posdif    [--same--]  swapped biconditional and variable order
15-Nov-04  negdistt3 negdi3t
15-Nov-04  negdistt2 negdi2t
15-Nov-04  negdistt  negdit
15-Nov-04  negdist3  negdi3
15-Nov-04  negdist2  negdi2
11-Nov-04  reuunis   reuuni2
11-Nov-04  reuuni    reuuni1
 9-Nov-04  zlelt1    zleltp1t
 9-Nov-04  zltle1    zltp1let
 7-Sep-04  arch      [--same--]  removed quantifier, changed set var. to class
 2-Nov-04  dfom2     dfom3
 2-Nov-04  dfom3     dfom4
29-Oct-04  df-ded    df-if
29-Oct-04  dedeq1    ifeq1
29-Oct-04  dedeq2    ifeq2
29-Oct-04  dedeq12   ifeq12
29-Oct-04  dedbi     biif
29-Oct-04  dedlem1   iftrue
29-Oct-04  dedlem2   iffalse
29-Oct-04  dedex     ifex
29-Oct-04  cded      cif         changed math symbol "ded" to "if" everywhere!
20-Oct-04  oprabex   oprabex2
14-Oct-04  nn0lelt1  nn0leltp1t
14-Oct-04  nn0ltle1  nn0ltp1let
14-Oct-04  nnlelt1   nnleltp1t
14-Oct-04  nnltle1   nnltp1let
14-Oct-04  rnoel3    rabn0
14-Oct-04  noel3     abn0
14-Oct-04  noel2     n0i
14-Oct-04  peano5c   peano5nn
14-Oct-04  peano2c   peano2nn
12-Oct-04  supeu     supeui
12-Oct-04  supcl     supcli
12-Oct-04  supub     supubi
12-Oct-04  suplub    suplubi
12-Oct-04  supnub    supnubi
12-Oct-04  suprcl    suprcli
12-Oct-04  suprub    suprubi
12-Oct-04  pm4.12    bicon2
12-Oct-04  bicon4    bicon4i
12-Oct-04  bicon2    bicon2i
12-Oct-04  bicon1    bicon1i
12-Oct-04  pm2.11    exmid
11-Oct-04  sbf1      ---         obsolete; use sbf instead
11-Oct-04  ceqsexg   ceqex
 9-Oct-04  fvco2     fvco3
 8-Oct-04  indif0    difdisj
 8-Oct-04  biopab    biopabi
 8-Oct-04  bioprab   bioprabi
 7-Oct-04  ssii      sselii
 6-Oct-04  op2nd     op2ndb
 6-Oct-04  op1st     op1stb
 3-Oct-04  ind       nnind
 3-Oct-04  sylan5    sylan2
 3-Oct-04  sylan5b   sylan2b
 3-Oct-04  sylan5br  sylan2br
 3-Oct-04  sylan5d   sylan2d
 3-Oct-04  zind      uzind
30-Sep-04  fvop      funopfv
30-Sep-04  funopfv   funfvopi
30-Sep-04  pm4.21    bicom
30-Sep-04  bicom     bicomi
30-Sep-04  entr      entri
30-Sep-04  sstr2xxx  sstr
30-Sep-04  sstr      sstr2
30-Sep-04  sstr2     sstr2xxx
29-Sep-04  xp0xxx    xp0r
29-Sep-04  xp0r      xp0
29-Sep-04  xp0       xp0xxx
28-Sep-04  xpdom2    xpdom3
28-Sep-04  xpdom     xpdom2      changed variable names
26-Sep-04  xpindi    inxp
26-Sep-04  xpun1     xpundi
26-Sep-04  xpun2     xpundir
25-Sep-04  entr      entrt
23-Sep-04  ssfnres   ---         obsolete; use fnssres instead
23-Sep-04  resun     resundi
21-Sep-04  f11       [--same--]  changed o.p. membership to binary relation
21-Sep-04  unisuc    [--same--]  swapped arguments of = sign
21-Sep-04  onunisuc  [--same--]  swapped arguments of = sign
21-Sep-04  ssundif   [--same--]  swapped arguments of = sign
21-Sep-04  ssequn2   [--same--]  swapped arguments of = sign
21-Sep-04  sseqin2   [--same--]  swapped arguments of = sign
21-Sep-04  onelun    [--same--]  swapped arguments of = sign
21-Sep-04  dfss4     [--same--]  swapped arguments of = sign
21-Sep-04  ordunisuc ordunisssuc
21-Sep-04  ssfun     funss
15-Sep-04  19.6      alex
15-Sep-04  alex      alexeq
15-Sep-04  19.11     excom
15-Sep-04  19.11a    excomim
15-Sep-04  19.5      alcom
15-Sep-04  19.7      alnex
15-Sep-04  19.14     exnal
15-Sep-04  alnex     alinexa
15-Sep-04  exnal     exanali
15-Sep-04  dmsn      dmsnop
15-Sep-04  rnsn      rnsnop
13-Sep-04  ppnull    pwpw0
13-Sep-04  pwnull    pw0
13-Sep-04  zfnull2   zfnul
13-Sep-04  nullpss   0pss
13-Sep-04  nullss    0ss
13-Sep-04  nulleq    eq0
13-Sep-04  nnull     n0
13-Sep-04  nnullf    n0f
13-Sep-04  xpdisj    xpsndisj
13-Sep-04  subdist   subdi
13-Sep-04  ecoprdist ecoprdi
13-Sep-04  negdist   negdi
13-Sep-04  nndist    nndi
13-Sep-04  xpindist  inxp
11-Sep-04  ssd       sseld
11-Sep-04  ssi       sseli
11-Sep-04  vtoclab   elab2
11-Sep-04  vtoclabg  elab2g
11-Sep-04  elab2g    elab3g
 6-Sep-04  comm      com12
 4-Sep-04  opabval   fvopab3
 4-Sep-04  opabvalig fvopab3ig
 4-Sep-04  opabval2g fvopab4g
 4-Sep-04  opabval2  fvopab4
 3-Sep-04  undif2    difun2
 3-Sep-04  undif3    ssundifOLD
 1-Sep-04  dedlem2   [--same--]  swapped arguments of = sign
 1-Sep-04  dedlem1   [--same--]  swapped arguments of = sign
31-Aug-04  pm2.16d   con3d
31-Aug-04  pm2.03d   con2d
31-Aug-04  pm2.15d   con1d
31-Aug-04  pm2.16    con3
31-Aug-04  pm2.03    con2
31-Aug-04  pm2.15    con1
31-Aug-04  con3      con3i
31-Aug-04  con2      con2i
31-Aug-04  con1      con1i
29-Aug-04  oprabelrn elrnoprab
27-Aug-04  ssequn1   [--same--]  swapped arguments of = sign
27-Aug-04  df-ss     dfss
27-Aug-04  imdistanb ---         obsolete; use imdistan
27-Aug-04  imdistan  [--same--]  now includes converse
17-Aug-04  difun2    [--same--]  swapped arguments of = sign
17-Aug-04  undif1    [--same--]  swapped arguments of = sign and union
17-Aug-04  difin     [--same--]  swapped arguments of = sign
17-Aug-04  unindistr undir
17-Aug-04  unindist  undi
17-Aug-04  inundistr indir
17-Aug-04  inundist  indi
17-Aug-04  indist    inindi
16-Aug-04  ss2un     unss12      order of variables changed
15-Aug-04  funmo,dffunmof,dffunmo
                     [--same--]  ordered pair membership -> binary relation
15-Aug-04  dfrel2    [--same--]  swapped arguments of = sign
12-Aug-04  unxp      xpundir
11-Aug-04  elima2    elima3
11-Aug-04  reluni    [--same--]  restricted quantifier and added converse
 9-Aug-04  imun      imaun
 3-Aug-04  1id       om1
 3-Aug-04  oalim     [--same--]  conjoined antecedents; now uses indexed union
 3-Aug-04  divdiv23  divdiv23z
 3-Aug-04  divdiv23i divdiv23
 2-Aug-04  divrec,divrecz [--same--] swapped A and B
 2-Aug-04  zq        zqt
 2-Aug-04  qre       qret
 1-Aug-04  mulcant2  [--same--]  swapped conjunct in antecedent
 1-Aug-04  axrecex,axrrecex,divclt,divcan1t,divcan2t,recidt,divdistrt,divrclt
                     [--same--]  conjoined the two left-most antecedents
 1-Aug-04  peano1c   1nn
 1-Aug-04  1nn       1onn
28-Jul-04  sbco0     sbid2
28-Jul-04  sbid2     sbid2v
26-Jul-04  cardonval oncardval
15-Jun-04  ssin      [--same--]  swapped biconditional order
15-Jun-04  unss      [--same--]  swapped biconditional order
11-Jun-04  dfun2     df-un
11-Jun-04  df-un     dfun2
11-Jun-04  dfin2     df-in
11-Jun-04  df-in     dfin2
 3-Jun-04  ssintss   intss
 3-Jun-04  intss     intss1
30-May-04  r1clos    r1pwcl      now uses antecedent instead of hypothesis
30-May-04  r1powt    r1pw
28-May-04  sqvalt    ---         obsolete; use exp2
28-May-04  expntwo   exp2
28-May-04  expnone   ---         obsolete; use expp1t
28-May-04  expnsuc   ---         obsolete; use expp1t
28-May-04  expnzer   ---         obsolete; use exp0t
26-May-04  ontrt     ---         obsolete; use onelon instead
26-May-04  ddelim*   [--same--]  (*=wildcard) changed order of hypotheses
21-May-04  fvcnvb    f1ocnvfvb
21-May-04  fvcnv     f1ocnvfv
21-May-04  sbcb      sbcbi       now uses antecedent instead of hypothesis
21-May-04  sbci      sbcim
21-May-04  sbb       sbbi
21-May-04  sbo       sbor
21-May-04  sbi       sbim
21-May-04  sbim      sbimi
21-May-04  sba       sban
21-May-04  fvelrn    [--same--]  changed to restricted quantifier
>8-Feb-04  zfpowb    pwexb
>8-Feb-04  zfpowcl   pwex
>8-Feb-04  zfnull    0ex
>8-Feb-04  limelon   [--same--]  changed first -> to /\ in antecedent


=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
                              Bibliography
=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

Bibliographical references are made by bracketing an identifer in a
theorem's comment, such as [RussellWhitehead].  These refer to HTML tags
on the following web pages:

  Logic and set theory - see http://us.metamath.org/mpegif/mmset.html#bib
  Hilbert space - see http://us.metamath.org/mpegif/mmhil.html#ref


=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
     Quick "How To" use this file (under Windows 95/98/NT/2K/XP)
=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

1. Download the program metamath.exe per the instructions on the
   Metamath home page (http://metamath.org) and put it in the same
   directory as this file (set.mm).
2. In Windows Explorer, double-click on metamath.exe.
3. Type "read set.mm" and press Enter.
4. Type "help" for a list of help topics, and "help demo" for some
   command examples.


=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
                       Metamath syntax summary
=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

The HELP LANGUAGE command in the Metamath program will give you a quick
overview of Metamath.  Syntax summary:

          $c ... $. - Constant declaration
          $v ... $. - Variable declaration
          $d ... $. - Disjoint (distinct) variable restriction
  <label> $f ... $. - "Floating" (variable-type) hypothesis
  <label> $e ... $. - "Essential" (logical) hypothesis (i.e. theorem
                      assumption)
  <label> $a ... $. - Axiom or definition or syntax construction
  <label> $p ... $= ... $. - Theorem and its proof
          ${ ... $} - Block for scoping above statements (except $a, $p
                      which are forever active)
$)        $( ... $) $( - Comments (may not be nested); see HELP LANGUAGE
                         for markup features
          $[ ... $] - Include file

The above tokens (i.e. those beginning with "$") are the ONLY primitives
built into the Metamath language.  The only 'logic' Metamath uses in its
proof verification algorithm is the substitution of expressions for
variables while checking for distinct variable violations.  EVERYTHING
ELSE, including the axioms for logic, is defined in this database file.

Here is some more detail about the syntax.  A <token> may not contain
the "$" character but may contain any other non-whitespace printable
character.  A <label> may contain only alphanumeric characters and the
characters ".-_".  Tokens and labels are case-sensitive.

          $c <tokenlist> $. <tokenlist> is a (whitespace-separated) list of
                            distinct tokens unused in current scope.
          $v <tokenlist> $. <tokenlist> is a (whitespace-separated) list of
                            distinct tokens unused in current scope.
          $d <tokenlist> $. <tokenlist> is a (whitespace-separated) list of
                            distinct tokens previously declared with $v in
                            current scope.  It means that substitutions into
                            these tokens may not have variables in common.
  <label> $f <tokenlist> $. <tokenlist> is a list of 2 tokens, the first of
                            which must be previously declared with $c
                            in the current scope.
  <label> $e <tokenlist> $. <tokenlist> is a list of 2 or more tokens, the
                            first of which must be previously declared with $c
                            in the current scope.
  <label> $a <tokenlist> $. <tokenlist> is a list of 2 or more tokens, the
                            first of which must be previously declared with $c
                            in the current scope.
  <label> $p <tokenlist> $= <proof> $. <tokenlist> is a list of 2 or more
                            tokens, the first of which must be previously
                            declared with $c in the current scope.  <proof> is
                            either a whitespace-delimited sequence of previous
                            labels (created by SAVE PROOF <label> /NORMAL) or a
                            compressed proof (created by SAVE PROOF <label>
                            /COMPRESSED).  After using SAVE PROOF, use
                            WRITE SOURCE to save the database file to disk.
          ${ ... $} - Block for scoping the above statements (except $a, $p
                      which are forever active)
$)        $( <any text> $) $( Comment.  Note: <any text> may not contain
                             adjacent "$" and ")" characters.
          $[ <filename> $]   Insert contents of <filename> at this point.
                             If <filename> is current file or has been already
                             been inserted, it will not be inserted again.

Inside of comments, it is recommended that labels be preceded with a tilde (~)
and math symbol tokens be enclosed in grave accents (` `).  This way the
LaTeX and HTML rendition of comments will be accurate, and (future) tools
to globally change labels and math symbols will also change them in comments.
Note that `` inside of grave accents is interpreted as a single ` .
See HELP LANGUAGE for other markup features in comments.

The proofs in this file are in "compressed" format for storage efficiency. The
Metamath program reads the compressed format directly.  This format is
described in an Appendix of the Metamath book.  It is not intended to be
read by humans.  For viewing proofs you should use the various SHOW PROOF
commands described in the Metamath book (or the on-line HELP).


=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
                           Other notes
=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

1. It is recommended that you be familiar with chapters 2 and 4 of the
'Metamath' book to understand the Metamath language.  Chapters 2, 3 and
5 explain how to use the program.  Chapter 3 gives you an informal
overview of what this source file is all about.  Appendix A shows you
the standard mathematical symbols corresponding to the ASCII tokens in
this file.

The ASCII tokens may seem cryptic at first, even if you are familiar with set
theory, but a review of the definition summary in Chapter 3 should quickly
enable you to see the correspondence to standard mathematical notation. To
easily find the definition of a token, search for the first occurrences of the
token surrounded by spaces.  Some odd-looking ones include "-." for "not", and
"(_" for "is a subset of".  (HELP TEX tells you how to obtain a LaTeX
output to see the real mathematical symbols.)  Let me know if you have
better suggestions for naming ASCII tokens.

2. Logic and set theory provide a foundation for all of mathematics.  To learn
about them, you should study this source file in conjunction with one or more
of the references listed below.  The textbooks provide a motivation for what we
are doing, whereas Metamath lets you see in detail all hidden and implicit
steps. Most standard theorems are accompanied by citations.  Some closely
followed texts include the following:

  Axioms of propositional calculus - [Margaris].
  Axioms of predicate calculus - [Megill] (System S3' in the article
      referenced).
  Theorems of propositional calculus - [WhiteheadRussell].
  Theorems of pure predicate calculus - [Margaris].
  Theorems of equality and substitution - [Monk2], [Tarski], [Megill].
  Axioms of set theory - [BellMachover].
  Development of set theory - [TakeutiZaring].  (The first part of [Quine]
      has a good explanation of the powerful device of "virtual" or
      class abstractions, which is essential to our development.)
  Construction of real and complex numbers - [Gleason]
  Theorems about real numbers - [Apostol]

3. Convention:  All $a statements starting with "|-" have labels
starting with "ax-" (axioms) or "df-" (definitions).  "ax-" corresponds
to what is traditionally called an axiom.  "df-" introduces new symbols
or a new relationship among symbols that can be eliminated; they always
extend the definition of a wff or class.  Metamath blindly treats $a
statements as new facts but does not try to justify them; their
justification is the job of mathematicians and philosophers.  Generally
our philosophy is to make definitions easy to eliminate by simple symbol
substitutions.

4. Our method of definition, the axioms for predicate calculus, and the
development of substitution are somewhat different from those found in
standard texts.  The axioms were designed for direct derivation of
standard results without excessive use of metatheorems.  (See Theorem
9.7 of [Megill] for a rigorous justification.)  Typically we are
minimalist when introducing new definitions; they are introduced only
when a clear advantage becomes apparent for reducing the number of
symbols, shortening proofs, etc.  We generally avoid the introduction of
excessive definitions because each one requires associated theorems and
additional elimination steps in proofs.

5. For the most part, the notation conforms to modern conventions, with
variations due to our choice of the axiom system or to make proofs
shorter.  Listed below are some important conventions and how they
correspond to textbook language.  The notation is usually explained in
more detail when first introduced.

  Typically, Greek letters (ph = phi, ps = psi, ch = chi, etc.) are used for
      propositional (wff) variables; x,y,z,... for individual (i.e. set)
      variables; and A,B,C,... for class variables.
  "|-", meaning "It is provable that," is the first token of all assertions
      and hypotheses that aren't syntax constructions.  This is a standard
      convention in logic.  For us, it also prevents any ambiguity with
      statements that are syntax constructions, such as "wff -. ph".
  "$e |- ( ph -> A. x ph ) $." should be read "Assume variable x is
      (effectively) not free in wff phi."  Literally, this says "Assume it is
      provable that phi implies for all x phi."
  "|- ( -. A. x x = y -> ..." should be read "If x and y are distinct
      variables, then..."  This antecedent provides us with a technical
      device (called a "distinctor" in [Megill]) to avoid the need for the
      $d statement early in our development of predicate calculus, permitting
      unrestricted substitituions as conceptually simple as those in
      propositional calculus.  However, the $d eventually becomes a
      requirement, and after that this device is rarely used.
  "[ y / x ] ph" should be read "the wff that results when y is properly
      substituted for x in ph."
  "$d x y $." should be read "Assume x and y are distinct variables."
  "$d x ph $." should be read "Assume x does not occur in phi $."  Sometimes
      a theorem is proved with "$e |- ( ph -> A. x ph ) $." in place of
      "$d x ph $." when a more general result is desired; ~ ax-17 can be used
      to derive the $d version.  For an example of how to get from the $d
      version back to the $e version, see the proof of ~ euf from ~ df-eu .
  "$d x A $." should be read "Assume x is not a variable occurring in class
      A."
  "$d x A $.  $d x ps $.  $e |- ( x = A -> ( ph <-> ps ) ) $." is often used
      instead of explicit substitution, meaning "Assume psi results from the
      substitution of A for x in phi".
  "$e |- A e. V $." should be read "Assume class A is a set (i.e. exists)."
      This is a convenient convention used by [Quine].
  "$d x y $.  $e |- y e. A -> A. x y e. A $." should be read "Assume x is
      (effectively) not a free variable in class A."
  "`' R" should be read "converse of (relation) R" and is the same as
      the more standard notation R^{-1}.
  "( f ` x )" should be read "the value of function f at x" and is the same
      as the more familiar f(x).
  The Deduction Theorem of standard logic is never used.  Instead, in
      set theory we use other tricks to make a $e hypothesis become
      an antecedent.  See the comment for theorem dedth below.


A note for anyone attempting to find shorter proofs
---------------------------------------------------

I always like shorter proofs, and any shorter proof I accept will be
acknowledged and put into the next version of the Metamath site.

I will automatically accept shorter proofs conforming to these
guidelines:  (1) only existing, unmodified theorems in the database are
used (the order of theorems may be changed though), (2) the number of
characters in the compressed proof is less than before, (3) the last
step number in SHOW PROOF when the proof is compressed is less than
before, (4) the number of steps in the HTML file (SHOW STATEMENT xxx /
HTML) is less than before, (5) the size of the HTML file is less than
before, (6) no additional axioms are used, and (7) none of the special
cases listed below apply.  If not all of these are the case I may still
accept the proof on a case-by-case basis.

There are several theorems that purposely do not have shortest proofs:

The proof of id1 , ax9a , ax15 , snex , sbth , ordon , ltso and several
other theorems (in particular ones avoiding ax-reg and ax-ac ) could be
made shorter but are longer to illustrate specific points or to avoid
specific axioms.

The following proofs should not be MINIMIZE_WITH'ed in the Proof
Assistant, since they are for illustrative purposes or they involve areas
I want to rework in the future.

   " id1 " using " a2i "
   " id1 " using " id "
   " con3th " using " con3 "
   " meredith " through " ax3 " using anything
   " ax9a " using " ax-9 "
   " ax15 " using " ax-15 "
   " ddelimf2 " using " ddelimf "
   " nnm0r " or anything else now using " om0 ", using " om0x "
   " omex " using " inf3 "
   " ltso " using " ltsor "
   " pjpj " using " pjpj0 "
   " pjoml " using " omls "
   " pjococ " using " omlsi "
   " pjococ " using " ococ "
   " pjpjtht " using " pjtht "
   " pjpjth " using " pjth "

Also, in set theory don't use a8b , eqt2b , a13b , a14b (use set theory
versions cleq1 , cleq2 , eleq1 , eleq2 instead).
Use om0 instead of om0x .
Don't use eirr if efrirr , ordeirr , or oneirr can be used (to avoid the
Axiom of Regularity when it is not needed).

$)

$(
#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#
                           Propositional calculus
#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#
$)

  $( Declare the primitive constant symbols for propositional calculus. $)
  $c ( $.  $( Left parenthesis $)
  $c ) $.  $( Right parenthesis $)
  $c -> $. $( Right arrow (read:  'implies') $)
  $c -. $. $( Right handle (read:  'not') $)
  $c wff $. $( Well-formed formula symbol (read:  'the following symbol
               sequence is a wff') $)
  $c |- $. $( Turnstile (read:  'the following symbol sequence is provable' or
              'a proof exists for') $)

  $( Introduce some variable names we will use to represent well-formed
     formulas (wff's). $)
  $v ph $. $( Greek phi $)
  $v ps $.  $( Greek psi $)
  $v ch $.  $( Greek chi $)
  $v th $.  $( Greek theta $)
  $v ta $.  $( Greek tau $)

  $(
     Specify some variables that we will use to represent wff's.
     The fact that a variable represents a wff is relevant only to a theorem
     referring to that variable, so we may use $f hypotheses.  The symbol
     ` wff ` specifies that the variable that follows it represents a wff.
  $)

  $( Let variable ` ph ` be a wff. $)
  wph $f wff ph $.
  $( Let variable ` ps ` be a wff. $)
  wps $f wff ps $.
  $( Let variable ` ch ` be a wff. $)
  wch $f wff ch $.
  $( Let variable ` th ` be a wff. $)
  wth $f wff th $.
  $( Let variable ` ta ` be a wff. $)
  wta $f wff ta $.

  $(
     Recursively define wff's.
  $)

  $( If ` ph ` is a wff, so is ` -. ph ` or "not ` ph ` ".  Part of the
     recursive definition of a wff (well-formed formula).  In classical logic
     (which is our logic), a wff is interpreted as either true or false.
     So if ` ph ` is true, then ` -. ph ` is false; if ` ph ` is false, then
     ` -. ph ` is true.  Traditionally, Greek letters are used to represent
     wffs, and we follow this convention.  In propositional calculus, we define
     only wffs built up from other wffs, i.e. there is no starting or "atomic"
     wff.  Later, in predicate calculus, we will extend the basic wff
     definition by including atomic wffs ( ~ weq and ~ wel ). $)
  wn $a wff -. ph $.

  $( If ` ph ` and ` ps ` are wff's, so is ` ( ph -> ps ) ` or " ` ph ` implies
     ` ps ` ."  Part of the recursive definition of a wff.  The resulting wff
     is (interpreted as) false when ` ph ` is true and ` ps ` is false; it is
     true otherwise.  (Think of the truth table for an OR gate with input
     ` ph ` connected through an inverter.)  The left-hand wff is called the
     antecedent, and the right-hand wff is called the consequent.  In the case
     of ` ( ph -> ( ps -> ch ) ) ` , the middle ` ps ` may be informally called
     either an antecedent or part of the consequent depending on context. $)
  wi $a wff ( ph -> ps ) $.

$(
=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
        The axioms of propositional calculus
=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
$)

  $(
     Postulate the three axioms of classical propositional calculus.
  $)

  $( Axiom _Simp_.  Axiom A1 of [Margaris] p. 49.  One of the 3 axioms of
     propositional calculus.  The 3 axioms are also given as Definition 2.1
     of [Hamilton] p. 28.  This axiom is called _Simp_ or "the principle of
     simplification" in _Principia Mathematica_ (Theorem *2.02 of
     [WhiteheadRussell] p. 100) because "it enables us to pass from the joint
     assertion of ` ph ` and ` ps ` to the assertion of ` ph ` simply."

     Propositional calculus (axioms ~ ax-1 through ~ ax-3 and rule ~ ax-mp )
     can be thought of as asserting formulas that are universally "true" when
     their variables are replaced by any combination of "true" and "false".
     Propositional calculus was first formalized by Frege in 1879, using as
     his axioms (in addition to rule ~ ax-mp ) the wffs ~ ax-1 , ~ ax-2 ,
     ~ pm2.04 , ~ con3 , ~ nega , and ~ negb .  Around 1930, Lukasiewicz
     simplified the system by eliminating the third (which follows from the
     first two, as you can see by looking at the proof of ~ pm2.04 ) and
     replacing the last three with our ~ ax-3 .  (Thanks to Ted Ulrich
     for this information.)

     The theorems of propositional calculus are also called _tautologies_.
     Tautologies can be proved very simply using truth tables, based on the
     true/false interpretation of propositional calculus.  This is called the
     _semantic_ approach.  Our approach is called the _syntactic_ approach,
     in which everything is derived from axioms.  A metatheorem called the
     Completeness Theorem for Propositional Calculus shows that the two
     approaches are equivalent and even provides an algorithm for
     automatically generating syntactic proofs from a truth table.  Those
     proofs, however, tend to be long, and the much shorter proofs that we
     show here were found manually. $)
  ax-1 $a |- ( ph -> ( ps -> ph ) ) $.

  $( Axiom _Frege_.  Axiom A2 of [Margaris] p. 49.  One of the 3 axioms of
     propositional calculus.  It distributes an antecedent over two
     consequents.  This axiom was part of Frege's original system and is known
     as _Frege_ in the literature.  It is also proved as Theorem *2.77 of
     [WhiteheadRussell] p. 108.  $)
  ax-2 $a |- ( ( ph -> ( ps -> ch ) ) -> ( ( ph -> ps ) -> ( ph -> ch ) ) ) $.

  $( Axiom _Transp_.  Axiom A3 of [Margaris] p. 49.  One of the 3 axioms of
     propositional calculus.  It swaps or transposes the order of the
     consequents when negation is removed.  An informal example is that the
     statement "if there are no clouds in the sky, it is not raining" implies
     the statement "if it is raining, there are clouds in the sky."  This
     axiom is called _Transp_ or "the principle of transposition" in
     _Principia Mathematica_ (Theorem *2.17 of [WhiteheadRussell] p. 103).  $)
  ax-3 $a |- ( ( -. ph -> -. ps ) -> ( ps -> ph ) ) $.

  $(
     Postulate the modus ponens rule of inference.
  $)

  ${
    $( Minor premise for modus ponens. $)
    min $e |- ph $.
    $( Major premise for modus ponens. $)
    maj $e |- ( ph -> ps ) $.
    $( Rule of Modus Ponens. The postulated inference rule of propositional
       calculus.  See e.g. Rule 1 of [Hamilton] p. 73.  The rule says, "if
       ` ph ` is true, and ` ph ` implies ` ps ` , then ` ps ` must also be
       true."  This rule is sometimes called "detachment", since it detaches
       the minor premise from the major premise. $)
    ax-mp $a |- ps $.
  $}

  $(
     ~ ax-1 , ~ ax-2 , ~ ax-3 , and ~ ax-mp are the complete set of
     postulates for propositional calculus.  Some additional $a statements
     are used later to introduce defined terms.
  $)

$(
=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
        Logical implication
=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
$)

$( The results in this section make use of the first 2 axioms only.  In
   an implication, the wff before the arrow is called the 'antecedent'
   and the wff after the arrow is called the 'consequent.' $)

$( We will use the following descriptive terms very loosely:  A 'theorem'
   usually has no $e hypotheses.  An 'inference' has one or more $e hypotheses.
   A 'deduction' is an inference in which the hypotheses and the result
   share the same antecedent. $)

  ${
    $( Premise for ~ a1i . $)
    a1i.1 $e |- ph $.
    $( Inference derived from axiom ~ ax-1 .  See ~ a1d for an explanation of
       our informal use of the terms "inference" and "deduction". $)
    a1i $p |- ( ps -> ph ) $=
      ( wi ax-1 ax-mp ) ABADCABEF $.
      $( [5-Aug-93] $)
  $}

  ${
    $( Premise for ~ a2i . $)
    a2i.1 $e |- ( ph -> ( ps -> ch ) ) $.
    $( Inference derived from axiom ~ ax-2 . $)
    a2i $p |- ( ( ph -> ps ) -> ( ph -> ch ) ) $=
      ( wi ax-2 ax-mp ) ABCEEABEACEEDABCFG $.
      $( [5-Aug-93] $)
  $}

  $( Principle of identity.  Theorem *2.08 of [WhiteheadRussell] p. 101.
     For another version of the proof directly from axioms, see ~ id1 . $)
  id $p |- ( ph -> ph ) $=
    ( wi ax-1 a2i ax-mp ) AAABZBFAACAFAAFCDE $.
    $( [5-Aug-93] $)
