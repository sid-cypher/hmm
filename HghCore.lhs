A Haskell module for sound Ghilbert-style proofs
================================================

:Author: Marnix Klooster <marnix.klooster@gmail.com>

(This document is usually available in two formats: `XHTML <HghCore.xhtml>`__
and `literate Haskell <HghCore.lhs>`__.)

Introduction
------------

Here is the suggested interface for a small Haskell module that implements the
Ghilbert-based core proof language, which I suggested to Raph Levien (personal
e-mail, 30 May 2006).

This module could be the core of a Ghilbert verifier. ::

> module HghCore
>	(Expression(Var, App)
>	,InferenceRule, inferenceRule, ruleDVRs, ruleHypotheses, ruleConclusion
>	)
> where

We need some auxiliary modules::

> import Data.List(sort, nub)


Expressions and inference rules
-------------------------------

All mathematical expressions are constructed from variables and operators
(=constants)::

> data Expression = Var String | App String [Expression]
>	deriving (Eq, Show)

An ``InferenceRule`` represents how to get from source expressions (hypotheses)
to a target expression (the conclusion). ::

> data InferenceRule = InferenceRule
>	{ruleDVRs :: [(String, String)]
>	,ruleHypotheses :: [Expression]
>	,ruleConclusion :: Expression
>	}
>	deriving (Eq, Show)

Note that the constructor for this data type is not exported, so that we can
choose an optimal data structure for performance.  Therefore we need a
constructor function::

> inferenceRule :: [(String, String)] -> [Expression] -> Expression
>			-> InferenceRule
> inferenceRule dvrs hyps concl = InferenceRule (dvrsCanonical dvrs) hyps concl

For performance reasons, we keep the DVRs in an ``InferenceRule`` in canonical
order::

> dvrsCanonical :: [(String, String)] -> [(String, String)]
> dvrsCanonical = nub . sort . Prelude.map sortPair
>	where sortPair (p, q)
>		| p <= q = (p, q)
>		| True = (q, p)

This helps us in the implementation of equality.

Two ``InferenceRule`` objectss are equal (under ``Eq``) iff they have the same
DVRs (in any order, ignoring duplicates), the same hypotheses (in the same
order, including potential duplicates), and the same conclusion.

Because of the canonical representation of an ``InferenceRule``, the
implementation of ``Eq InferenceRule`` is as simple as "``deriving Eq``" (as is
done above).


Proofs
------

Now on to proofs::

< data Proof
<	= Hypothesis Expression
<	| RuleApp InferenceRule [Expression] [Proof]

TODO: Explain what the components of a RuleApp mean, and what it means for a
RuleApp to be consistent.

Design Issue. As written, a ``RuleApp`` requires knowledge of the order of the
hypotheses of an ``InferenceRule``. However, (meta-)logically
``InferenceRule`` values that only differ in their order of hypotheses are
equivalent.  Therefore we have four options.

 * We specify the order of the result of ``ruleHypotheses`` to be the same as
   the creation order.  We declare an ``InferenceRule`` with a different order
   of hypotheses to be different (under ``Eq``).  (We introduce a new
   equivalence ``(<==>)`` on ``InferenceRule``, and by extension on ``Proof``
   and ``Derivation`` (see below).) In a ``RuleApp`` the proofs must be in
   creation order.

 * We specify the order of the result of ``ruleHypotheses`` to be the same as
   the creation order.  Two ``InferenceRule`` objects can have different
   ``ruleHypothese`` results, and still be the same (under ``Eq``).  (This
   seems to be inconsistent.)  In a ``RuleApp`` the proofs must be in creation
   order.

 * We specify the order of the result of ``ruleHypotheses`` to be some
   arbitrary (but consistent) order.  In a ``RuleApp`` the proofs must be
   in this same order.

 * Instead of ``[Proof]``, a ``RuleApp`` uses ``[(Expression, Proof)]``, where
   each ``Expression`` is the hypothesis that is proven by the corresponding
   sub-``Proof``.  (Or we could even have it use a ``(Expression -> Proof)``,
   but that seems a bit silly.)

It seems each of these has its disadvantages.  It seems the first option is the
most consistent, while still making this module easy to use for a Ghilbert
verifier.  That's why I went with that option.  (End of Design Issue.)


Deriving inference rules: Derivations
-------------------------------------

Now we come to the heart of the matter: the ``Derivation``.  It is essential
that values of this type can only be constructed through ``interpretProof``::

< data Derivation = ToBeDefined2
< 
< interpretProof :: Proof -> Either String Derivation

This function basically implements the Ghilbert proof verification algorithm,
with the DVRs computed just like Hmm does this for Metamath proofs.

If the ``Proof`` is incorrect (i.e., if a ``RuleApp`` in it is inconsistent),
then the result will be ``Left "some error message"``, otherwise the result
will be a ``Right`` value with the resulting ``Derivation``.

For a correct ``Proof`` the result of this algorithm is a 'theorem', or in our
terminology, an ``InferenceRule``::

< targetRule :: Derivation -> InferenceRule

The resulting ``Derivation`` also knows what the assumptions of the ``Proof``
were, i.e., what inference rules were used in the ``Proof``::

< sourceRules :: Derivation -> [InferenceRule]

Two ``Derivation`` objects are equal (under ``Eq``) iff they map the same *set*
of ``InferenceRule`` to the same ``InferenceRule``.  This implies that a
``Derivation`` is independent of the ``Proof`` that was used to create it.


Open Issue. We could also make it possible to combine ``Derivation`` objects::

< combineDerivation :: Derivation -> Derivation -> Derivation

This takes the second derivation, removes from its ``sourceRules`` the
``targetRule`` of the first derivation, and adds to its sourceRules the
``sourceRules`` of the first derivation.  Perhaps it sounds difficult, but
basically this 'clicks together' derivations.

This ``combine`` operator is theoretically not necessary, since it is also
possible to have a similar operator at the ``Proof`` level, and then
``interpretProof`` creates the desired ``Derivation``.  However, this requires
one of the following:

 * Either a client of this module has to remember a ``Proof`` for each
   ``Derivation``;

 * Or this module makes a ``Proof`` part of each ``Derivation``.

Frankly I don't really like any of the three alternatives.  The best
alternative might be the first bullet above: implement the ``combine``
functionality outside of this core module, and keep ``Derivation`` and
``Proof`` separate.  But I haven't decided yet.  (End Open Issue.)


Weakening
---------

Until now we've not been complete, in the technical sense: it is not possible
to create every 'true' ``Derivation`` through ``interpretProof``.  The reason is
that ``interpretProof`` computes the "strongest" inference rule that is proven
by the given Proof.  To do proper theorem verification à la Ghilbert, we need
to be able to weaken a given derivation.

First, to determine the relationship between inference rules, we introduce ::

< (==>) :: InferenceRule -> InferenceRule -> Bool

Basically ``i ==> j`` ("``i`` is at least as strong as ``j``") iff ``i`` and
``j`` have the same conclusion, and if the DVRs and hypotheses of ``i`` are a
subset (under (<==>)) of those of ``j``.

Now we need the following two additional ways to create ``Derivations``::

< strengthenSourceRules :: InferenceRule -> Derivation -> Derivation
< weakenTargetRule :: InferenceRule -> Derivation -> Derivation

``strengthenSourceRule`` adds the given ``InferenceRule`` to the
``sourceRules``, and removes any that are weaker (i.e., implied by it under
(==>)).  ``weakenTargetRule`` replaces the ``targetRule`` by the given
``InferenceRule``, but only if the original rule implies the new one (otherwise
an error occurs).

