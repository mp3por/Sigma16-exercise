------------------------------------------------------------------------
-- Hydra hardware description language
-- Copyright John O'Donnell, see LICENSE and README
-- Box:  structural representation of circuits
------------------------------------------------------------------------

{- Representation of the Hydra language and transformations for
structural operations, including generation of netlists.  Experimental
module (later will become HDL.Hydra.Core.Box) -}

-- The Box module
--     Error handling
-- Syntax
--     Abstract syntax of Hydra language
--     Concrete syntax of Template Haskell
--     Show the Template Haskell code representation
-- Structural representation of signals
--     ClusterAccess: a data structure isomorphic to a cluster
--     ClusterShape: representing the organization of a cluster
--     ClusterContext: representing the location of a cluster
--     ClusterPiece: down from a cluster to its constituent pieces
--     Renameable clusters
--     Signal equality
--     Showing the signal and cluster representations
-- Structural representation of circuits
--     Traverse a circuit structure
--     Discover the components and signals in a circuit
--     Build structural primitive circuits
-- Simulating structural signals
-- Transformation of a circuit specification
--     Overview of the transformation
--     Transform a source module
--     Transform a top level declaration
--     Transform an input cluster
--     Transform an expression cluster
--     Helper functions

------------------------------------------------------------------------
-- The Box module
------------------------------------------------------------------------

{-# LANGUAGE
    TemplateHaskell,
    ExplicitForAll,
    Rank2Types,
    MultiParamTypeClasses,
    FlexibleContexts,
    FlexibleInstances,
    UndecidableInstances,
    FunctionalDependencies
  #-}

module Box where

import Control.Monad.Error
import Text.PrettyPrint
import Language.Haskell.TH
import HDL.Hydra.Core.Lib
import HDL.Hydra.Circuits.StandardCircuits


------------------------------------------------------------------------
--     Error handling
------------------------------------------------------------------------

{- Hydra is a small subset of Haskell.  When a Haskell construct which
is not valid Hydra is encountered, a HydraError is reported. -}

data HydraError
  = HyBadExp
  | ErrMultiClauses THname  -- multiple equations for a function
  | ErrBadDec
  | ErrBadPat Pat
  | HyBadEqu
  | HyBadPat
  | HyOtherError String
  deriving Show

instance Error HydraError where
  noMsg = HyOtherError "unknown error"
  strMsg s = HyOtherError s

type ErrQ = ErrorT HydraError Q

tryErrQ :: ErrQ Exp
tryErrQ =
  do let x = VarE (mkName "foo")
     y <- lift (newName "bar")
     lift (runIO (putStrLn "hello"))
     return x

------------------------------------------------------------------------
-- Syntax
------------------------------------------------------------------------

------------------------------------------------------------------------
--     Abstract syntax of Hydra language
------------------------------------------------------------------------

{- Name is ambiguous, THname isn't -}

type THname = Language.Haskell.TH.Name

{- The abstract syntax of the Hydra language is defined using
algebraic data types.

Hydra is implemented as a domain specific language embedded in
Haskell.  Hydra follows the syntax rules of Haskell, a general purpose
functional language.  For the concrete syntax, including the use of
spaces, indentation, and comments, see documentation on the Haskell
language.

Names are used for signals and circuits. A name is a String, and it
must follow the lexical rules of Haskell.

Expressions: Circuits and signals are denoted by expressions.

A circuit is a function with zero or more inputs and with one output.
The inputs and output are signal expressions.  A circuit may have no
inputs; a constant zero signal and a square wave generator are two
examples.

Expressions can denote either circuits or signals; the distinction
depends on the type.

A signal is defined by connecting it to the output of a circuit,
which is in turn connected to its input signals.  This is expressed by
writing an expression (called a circuit application, or CircApp)
consisting of the circuit's name followed by its input signals.  There
may be any number of input signals; the number depends on how many
input ports the circuit has.

Signals can be grouped together for convenience.  Grouping is purely a
shortcut for making circuit specifications shorter and more readable.
It is always possible to specify any circuit by writing out each
signal individually, but it's usually clearer to group related signals
together.  There are two grouping notations: tuples and words.

Tuples use round parentheses and the elements are separated by commas.
The elements of a tuple can have different types: for example, if
a,b,c are bit signals, then (a,(b,c)) is a pair (2-tuple) whose first
element is a bit signal, but whose second element is a pair of bits.

Words use square brackets and the elements are separated by commas.
All the elements must have the same type.  For example, [x0,x1,x2] is
a word of singleton bits and [(x0,y0), (x1,y1), (x2,y2)] is a word of
pairs, but [zero, (a,b)] is wrong because the first element is a bit
and the second element is a pair of bits.

Words and tuples may be nested: you can have a word of tuples, or a
tuple of words, or more deeply nested structures. For example, an
adder might output a tuple with two elements, one of which is a carry
bit and the other is a word of sum bits.

A signal group expression describes the value of a signal or a group
of signals.  There are several forms of signal expressions: a name, a
group constructor, and a circuit application.

CONSIDER PARTIAL APPLICATIONS.  circ x y could be (circ a) as a
circuit with fanout a.  If it's a partial application, it's a circuit;
if it's a full application, it's a signal expression.  Need to access
the type to decide...  But could just use context: without making a
distinction between circuits and signals; the typechecker will locate
errors in this anyway.  After all, a circuit with one output and no
inputs would look syntactically like a signal anyway.

-}

------------------------------------------------------------------------
--     Concrete syntax of Template Haskell
------------------------------------------------------------------------

{- Concrete syntax of Template Haskell

Name
  mkName :: String -> Name

Expressions
  VarE Name     :: Exp
  AppE Exp Exp  :: Exp
  TupE [Exp]    :: Exp
  ListE [Exp]   :: Exp
  _             :: Exp but unused in Hydra

Declaration
  FunD Name [Clause]  :: Dec       {f p1 p2 = b where decs}
  ValD Pat Body [Dec] :: Dec       {p = b where decs}

Clause [Pat] Body [Dec] :: Clause

NormalB Exp :: Body
  A Body can be a NormalB with just an Exp, or it can have a guard.
  Hydra doesn't allow the guards.

A circuit specification

FunD circuit_name [oneclause]
The clause is Clause [Pat] Body [Dec]
The Body is NormalB exp
The Decs are of the form ValD Pat Body []

FunD cname [Clause pats body [ValD p b [], ValD p b []]]

Variable
  VarE Test1.x

***
circ x = x

FunD circ
  [Clause [VarP x] (NormalB (VarE x)) []]

***
-}

------------------------------------------------------------------------
--     Show the Template Haskell code representation
------------------------------------------------------------------------

prDec :: Dec -> String
prDec = pprint

------------------------------------------------------------------------
-- Structural representation of signals
------------------------------------------------------------------------

{- Signal representation: if x :: StrucSig a, then x is a base signal
where a is the behavioral type.  Base means that x is just one base
value, such as a bit, but not a word or tuple.  However, x could have
a base type of Stream Int, so it might actually correspond to several
wires in a hardware realisation; it's just that there is only one
signal as far as Hydra is concerned. -}

{- Structural signals.  A signal in a structural circuit specification
is represented as StrucSig a, where a is the base signal
representation.  The base signal, of type a, could be a stream used
for simulation, so that the simulation can be carried out in the
context of the hierarchical structure of the circuit. The StrucSig
contains the base signal (:: a) for simulation, as well as a pointer
(:: ClusterContext a) that says where this signal occurs within a
cluster.  Every signal is a member of a cluster, even if that cluster
is only a singleton.  Thus the ClusterContext pointer enables
functions to trace back from the StrucSig to find where it is
defined. -}

data StrucSig a
  = StrucSigBase a
  | StrucSigRenamed (StrucSig a) (ClusterContext a)

------------------------------------------------------------------------
--     ClusterAccess: a data structure isomorphic to a cluster
------------------------------------------------------------------------

{- It's common to group several signals together in order to clarify a
circuit specification.  For example, a word might be described as a
single object, rather than as 32 individual wires.  This is a simple
but effective form of abstraction, and it can occur at many levels.

Hydra uses lists and tuples to group together signals, and they can be
nested.  Any such grouping is called a Cluster.  The type variable a
is the underlying non-structural signal type (e.g. it can be used for
simulation). A cluster may have a name, or it may be anonymous.

There are several related terms used in Hydra:

* A cluster is an argument to a function, or the result of a function.
  A cluster might have a type like [(a,a)] where a is the underlying
  base signal type.

* A cluster shape is a data structure of type ClusterShape that
  describes the structure of a cluster.

* A ClusterAccess is a data structure of type ClusterAccess a that
  provides access to the components of a cluster.  This data structure
  contains a pointer giving the location of the cluster
  (ClusterContext a), as well as the contents of the cluster.  A
  cluster may be a singleton, word, or tuple.  The purpose of a
  singleton is to allow giving a name and location to a signal.  Aside
  from these "cluster as further clusters" values, a cluster may
  simply be a structural signal.

Signal clusters, a is the base signal type.

For now, assume each group is a singleton signal.  Later, will need
to allow tuples and words, and have a representation of a path from a
group to an element of the group.  Perhaps groups should be treated as
signals, although that would mean that logic gates would not simply
have Signal a as the context, but something like BitSignal a.

A signal might be in a cluster, but that cluster could somewhere be
embedded in a larger, cluster, and the representation of this signal
might not know it.  Examples needed.

A signal points to the cluster that contains it, which might just be a
singleton but could be a tuple, word, tuple of words, etc.  The
cluster is a data structure that contains pointers to all its
components (and sub components, etc. down to the level of singletons).
A cluster also indicates where it is, via a box and a cluster location
ClusterContext which says where in the box this cluster belongs.

From a signal, you can work up through clusters, one level at a time.
From a cluster, you can traverse down, one level at a time, and find
all the signals.

A singleton doesn't necessarily indicate one physical wire.  There
could be several base signal types, for example a = Stream Bool and b
= Stream Int, and a singleton signal of type b could correspond to 16
signals of type a.  -}

data ClusterAccess a
  = CluSig  (Maybe CluName) (ClusterContext a) (StrucSig a)
  | CluSgl  (Maybe CluName) (ClusterContext a) (ClusterAccess a)
  | CluWord (Maybe CluName) (ClusterContext a) [ClusterAccess a]
  | CluTup  (Maybe CluName) (ClusterContext a) [ClusterAccess a]

{- Cluster names.  A CluName is a string defined in a pattern; thus
the equation x = ... defines a singleton cluster with name "x". -}

type CluName = Language.Haskell.TH.Name

------------------------------------------------------------------------
--     ClusterShape: representing the organization of a cluster
------------------------------------------------------------------------

data ClusterShape
  = ShapeName String
  | ShapeTuple [ClusterShape]
  | ShapeWord [ClusterShape]
  | ShapeApp ClusterShape ClusterShape
  deriving Show

-- Build a data structure representing the shape of a pattern

patToClusterShape :: Pat -> ErrQ ClusterShape
patToClusterShape (VarP n) = return (ShapeName (show n))
patToClusterShape (TupP xs) =
  do ys <- mapM patToClusterShape xs
     return (ShapeTuple ys)
patToClusterShape (ListP xs) =
  do ys <- mapM patToClusterShape xs
     return (ShapeWord ys)
patToClusterShape p = throwError (ErrBadPat p)

-- Build a data structure representing the shape of an expression

expToClusterShape :: Exp -> ErrQ ClusterShape
expToClusterShape (VarE n) = return (ShapeName (show n))
expToClusterShape (TupE xs) =
  do ys <- mapM expToClusterShape xs
     return (ShapeTuple ys)
expToClusterShape (ListE xs) =
  do ys <- mapM expToClusterShape xs
     return (ShapeWord ys)
expToClusterShape (AppE f x) =
  do f' <- expToClusterShape f
     x' <- expToClusterShape x
     return (ShapeApp f' x')
expToClusterShape p = throwError (HyOtherError "bad exp for shape")

-- Given a shape, construct a TH expression that will, when evaluated,
-- construct that shape.  This is needed because the transformed
-- function definition must build shapes.

shapeToExp :: ClusterShape -> ErrQ Exp
shapeToExp (ShapeName n) =
  return  (AppE
             (ConE (mkName "ShapeName"))
             (LitE (StringL n)))
shapeToExp (ShapeTuple xs) =
  do exps <- mapM shapeToExp xs
     let expListExp = ListE exps
     return (AppE (ConE (mkName "ShapeTuple")) expListExp)
shapeToExp (ShapeWord xs) =
  do exps <- mapM shapeToExp xs
     let expListExp = ListE exps
     return (AppE (ConE (mkName "ShapeWord")) expListExp)
shapeToExp (ShapeApp f x) =
  do f' <- shapeToExp f
     x' <- shapeToExp x
     return (AppE f' x')

-- Print a shape

ppPat :: ClusterShape -> IO ()
ppPat (ShapeName name) = putStrLn ("  pat name = " ++ show name)
ppPat (ShapeTuple ps) =
  do putStrLn "pat tuple = "
     mapM ppPat ps
     return ()
ppPat (ShapeWord ps) =
  do putStrLn "pat word = "
     mapM ppPat ps
     return ()

------------------------------------------------------------------------
--     ClusterContext: representing the location of a cluster
------------------------------------------------------------------------

{- The location of a cluster is represented by a cluster-up pointer,
of type ClusterContext a.  By following this pointer, the context of
the cluster is found.  There are two cases: the cluster might be a
component of a larger cluster, or it might be a top level cluster
which is located somewhere within a black box.  Every cluster must
fall into one of these two cases; random clusters just floating around
somewhere are not allowed. -}

data ClusterContext a
  = ClusterContextPrim
  | ClusterContextSig (ClusterAccess a)
  | ClusterContextSgl (ClusterAccess a)
  | ClusterContextWord (ClusterAccess a) Int
  | ClusterContextTup (ClusterAccess a) Int
  | ClusterContextInport (CircBox a) Int
  | ClusterContextOutport (CircBox a)
  | ClusterContextEqu (CircBox a) [Int]

{- Addressing the internal boxes and signals.  Suppose the context is
a particular box, and it contains the following equation, which is
equation number i:

  x = and2 a (or4 b (inv c) d (buf e))

The location of an internal box can be specified as a list of Ints.
Then the internal boxes and their locations are

  and2 ~ [i]
  or4  ~ [i, 1]
  inv  ~ [i, 1, 1]
  buf  ~ [i, 1, 3]

Each internal signal belongs to a cluster, and each cluster is
identified as one of (1) and input cluster to the main box, identified
by the input cluster number, or (2) the output of an internal box,
identified by the internal box location.

The transformed code needs to rename all the clusters that appear on
the right hand side of an equation. -}

------------------------------------------------------------------------
--     ClusterPiece: down from a cluster to its constituent pieces
------------------------------------------------------------------------

{- Find the list of paths to the component signals in a cluster -}

ppClusterPieces :: [ClusterPiece a] -> Doc
ppClusterPieces ds = vcat (map ppClusterPiece ds)

data ClusterPiece a
  = CluSigDown (ClusterAccess a) (StrucSig a)
  | CluSglDown (ClusterAccess a) (ClusterPiece a)
  | CluWordDown (ClusterAccess a) Int (ClusterPiece a)
  | CluTupDown (ClusterAccess a) Int (ClusterPiece a)

ppClusterPiece :: ClusterPiece a -> Doc
ppClusterPiece (CluSigDown c x) = text "Sig"
ppClusterPiece (CluSglDown c x) = text "Sgl" <+> char '.' <+> ppClusterPiece x
ppClusterPiece (CluWordDown c i x) =
  text "Word[" <> int i <> text "]" <+> char '.' <+> ppClusterPiece x
ppClusterPiece (CluTupDown c i x) =
  text "Tup(" <> int i <> text ")" <+> char '.' <+> ppClusterPiece x

sigsFromClu :: ClusterAccess a -> [ClusterPiece a]
sigsFromClu c@(CluSig n u x) = [CluSigDown c x]
sigsFromClu c@(CluSgl n u x) =
  [CluSglDown c dy | dy <- sigsFromClu x]
sigsFromClu c@(CluWord n u xs) =
  concat
    [[CluWordDown c i d | d <- sigsFromClu x]
       | (i,x) <- zip [0..] xs]
sigsFromClu c@(CluTup n u xs) =
  concat
    [[CluTupDown c i d | d <- sigsFromClu x]
       | (i,x) <- zip [0..] xs]

------------------------------------------------------------------------
--     Renameable clusters
------------------------------------------------------------------------

{-- a is the base signal type, b is the cluster type -}

class Renameable a b | b -> a where
  rename :: b -> ClusterShape -> ClusterContext a -> (ClusterAccess a, b)
  flattenCluster :: b -> [StrucSig a]

{- The type ClusterShape describes the shape of a cluster; it
specifies a name, tuple, or word (and a tuple or word may contain
further ClusterShapes).  Suppose we have a cluster x, which is a
tuple/word/singleton based on signal type a.  We also have a pattern p
:: ClusterShape which gives the structure of the cluster x, and a u ::
ClusterContext a which gives the context of x.  Then rename pat x u
returns a pair: the first element is the shape of the cluster,
expressed in the type ClusterAccess a, and the second element is a
renamed cluster.  -}

instance Renameable a (StrucSig a) where
  rename x shape u =
    case shape of
      ShapeName n ->
        let cl = CluSig (Just (mkName n)) u x
            x' = StrucSigRenamed x (ClusterContextSig cl)
        in (cl,x')
      _ -> error "rename expected ShapeName"
  flattenCluster x = [x]

instance (Renameable a b, Renameable a c) => Renameable a (b,c) where
  rename (x,y) shape u =
    case shape of
      ShapeTuple [px,py] ->
        let (clx,x') = rename x px ux
            (cly,y') = rename y py uy
            ux = ClusterContextTup cl 0
            uy = ClusterContextTup cl 1
            cl = CluTup Nothing u [clx,cly]
         in (cl,(x',y'))
      _ -> error "rename expected ShapeTuple [px,py]"
  flattenCluster (x,y) = flattenCluster x ++ flattenCluster y

instance Renameable a b => Renameable a [b] where
  rename xs p u =
    case p of
      ShapeWord ps ->
        let zs = zipWith3 rename xs ps us
            us = [ClusterContextWord cl i | i <- [0 .. length xs - 1]]
            cl = CluWord Nothing u (map fst zs)
            xs' = map snd zs
        in (cl,xs')
      _ -> error "rename expected ShapeWord ps"
  flattenCluster xs = concat (map flattenCluster xs)

------------------------------------------------------------------------
--     Signal equality
------------------------------------------------------------------------

{- Predicate to determine whether two values denote the same
signal. Each signal has a representation specific to that box.  For
example, a signal defined by an equation has a representation that
says it's defined by equation n withing box b.  If a signal is
actually produced by some external box, and is an input to this box,
then it is renamed so it has a representation as an input port
relative to this box.  -}

------------------------------------------------------------------------
--     Showing the signal and cluster representations
------------------------------------------------------------------------

describeSigFull :: StrucSig a -> String
describeSigFull (StrucSigBase x) = "StrucSigBase"
describeSigFull (StrucSigRenamed x u) =
  "(StrucSigRenamed " ++ describeClusterContext u ++ describeSigFull x ++ ")"

-- Just show the current (most recent) renaming of a signal
describeSigShort :: StrucSig a -> String
describeSigShort (StrucSigBase x) = "(signal base)"
describeSigShort (StrucSigRenamed x u) =
  "(signal " ++ describeClusterContextShort u ++ ")"

describeCluShort :: ClusterAccess a -> String
describeCluShort (CluSig n u x) = describeCluName n
describeCluShort (CluSgl n u c) = describeCluName n
describeCluShort (CluWord n u c) =
  "CluWord[" ++ show (length c) ++ "]"  ++ describeCluName n
describeCluShort (CluTup n u c) =
  "CluTup(" ++ show (length c) ++ ")"  ++ describeCluName n

describeClusterAccess :: ClusterAccess a -> String
describeClusterAccess (CluSig n u x) = describeCluName n
describeClusterAccess (CluSgl n u c) = "CluSgl" ++ describeCluName n
describeClusterAccess (CluWord n u cs) =
  "[" ++ sepWithCommas (map describeClusterAccess cs) ++ "]"
--  "CluWord[" ++ show (length c) ++ "]"  ++ describeCluName n
describeClusterAccess (CluTup n u cs) =
  "[" ++ sepWithCommas (map describeClusterAccess cs) ++ "]"
--  "CluTup(" ++ show (length c) ++ ")"  ++ describeCluName n

sepWithCommas :: [String] -> String
sepWithCommas [] = []
sepWithCommas [x] = x
sepWithCommas (x:xs) = x ++ "," ++ sepWithCommas xs

describeCluName :: Maybe CluName -> String
describeCluName Nothing = []
describeCluName (Just n) = show n

describeClusterContextShort :: ClusterContext a -> String
describeClusterContextShort ClusterContextPrim = "(prim)"
describeClusterContextShort (ClusterContextSig c) = describeCluShort c
describeClusterContextShort (ClusterContextSgl c) = describeCluShort c
describeClusterContextShort (ClusterContextWord c i)
  = "Word[" ++ show i ++ "]"
describeClusterContextShort (ClusterContextTup c i)
  = "Tup[" ++ show i ++ "]"
describeClusterContextShort (ClusterContextInport b i)
  = "ClusterContextInport[" ++ show i ++ "]"
describeClusterContextShort (ClusterContextOutport b)
  = "ClusterContextOutport"
describeClusterContextShort (ClusterContextEqu b is)
  = "ClusterContextEqu[" ++ show is ++ "]"

describeClusterContext :: ClusterContext a -> String
describeClusterContext ClusterContextPrim = "ClusterContextPrim"
describeClusterContext (ClusterContextSig c)
  = "ClusterContextSig" ++ describeClusterAccess c
describeClusterContext (ClusterContextSgl c) = "ClusterContextSgl"
describeClusterContext (ClusterContextWord c i)
  = "ClusterContextWord[" ++ show i ++ "]"
describeClusterContext (ClusterContextTup c i)
  = "ClusterContextTup[" ++ show i ++ "]"
describeClusterContext (ClusterContextInport b i)
  = "ClusterContextInport[" ++ show i ++ "]"
describeClusterContext (ClusterContextOutport b)
  = "ClusterContextOutport"
describeClusterContext (ClusterContextEqu b is)
  = "ClusterContextEqu[" ++ show is ++ "]"

describeBox :: CircBox a -> String
describeBox (CircBox n ins lhss out) =
  do "Circuit " ++ n
     ++ "\n  " ++ show (length ins) ++ " input clusters:"
     ++ concat (map describeClusterAccess ins)
     ++ "\n  " ++ "output cluster:"
     ++ describeClusterAccess out
     ++ "\n  " ++ show (length lhss) ++ " equations:"
     ++ concat (map describeClusterAccess lhss)

------------------------------------------------------------------------
-- Structural representation of circuits
------------------------------------------------------------------------

data CircBox a
  = CircBox String [ClusterAccess a] [ClusterAccess a] (ClusterAccess a)

{-
data BoxPath a
  = TopBox
  | SubBox (BoxPath a) (Box a) Int

-- mkInportSig :: StrucSig a -> Box a -> Int -> StrucSig a
-- mkInportSig x b i = BoxInput x b i

data Role a
  = BoxInput (StrucSig a) (Box a) Int
  | BoxOutput (Box a) Int
  | BoxInternal (Box a) Int

-}

{- When a signal appears in a box it has a role as a circuit input, or
as an internal signal which is the output of an interior box.  The
role refers to the role of the signal within the box; thus if the
signal is one of the outputs, its role would still refer either to its
membership in the set of inputs or the set of internal signals.  The
actual outputs of a circuit have a role as an output of the box, which
is layered on top of its role within the box. -}

------------------------------------------------------------------------
--     Traverse a circuit structure
------------------------------------------------------------------------

getBoxfromSig :: StrucSig a -> CircBox a
getBoxfromSig (StrucSigBase x) = error "Cannot find box in StrucSigBase"
getBoxfromSig (StrucSigRenamed x u) = getBoxfromClusterContext u

getBoxfromClusterContext :: ClusterContext a -> CircBox a
getBoxfromClusterContext ClusterContextPrim = error "getbox - prim"
getBoxfromClusterContext (ClusterContextSig c) = getBoxfromClu c
getBoxfromClusterContext (ClusterContextSgl c) = getBoxfromClu c
getBoxfromClusterContext (ClusterContextWord c i) = getBoxfromClu c
getBoxfromClusterContext (ClusterContextTup c i) = getBoxfromClu c
getBoxfromClusterContext (ClusterContextInport b i) = b
getBoxfromClusterContext (ClusterContextOutport b) = b
getBoxfromClusterContext (ClusterContextEqu b is) = b

getBoxfromClu :: ClusterAccess a -> CircBox a
getBoxfromClu (CluSig n u x) = getBoxfromClusterContext u
getBoxfromClu (CluSgl n u x) = getBoxfromClusterContext u
getBoxfromClu (CluWord n u xs) = getBoxfromClusterContext u
getBoxfromClu (CluTup n u xs) = getBoxfromClusterContext u

------------------------------------------------------------------------
--     Discover the components and signals in a circuit
------------------------------------------------------------------------

boxOuts :: CircBox a -> ClusterAccess a
boxOuts (CircBox n inps equs outs) = outs

traverse :: Renameable a b => b -> IO ()
traverse cl =
  do let outs = flattenCluster cl
     putStrLn (describeSigShort (outs!!0))
     putStrLn (describeSigShort (outs!!1))
     putStrLn (describeSigShort (outs!!2))
     putStrLn (describeSigShort (outs!!3))
     let topbox = getBoxfromSig (head outs)
     putStrLn (describeBox topbox)
     let boxoutclu = boxOuts topbox
     let outsigdowns = sigsFromClu boxoutclu
     putStrLn (render (ppClusterPieces outsigdowns))
     putStrLn "traverse finished"

------------------------------------------------------------------------
--     Build structural primitive circuits
------------------------------------------------------------------------

mkStrucFcn11
  :: Signal a
  => String
  -> (a->a)
  -> (StrucSig a -> StrucSig a)

mkStrucFcn11 circName circFcn inputs0 = outputs
  where
--
-- Inputs (cluster 0)
    inputs0pat = ShapeName (circName ++ "_in")
    (inclu0,inputs0') = rename inputs0 inputs0pat (ClusterContextInport box 0)
    x = inputs0'
-- Internal primitive behavior
    x' = getsigval x
    y = circFcn x'
    y' = StrucSigBase y
-- Outputs
    outputs0 = y'
    outpat = ShapeName (circName ++ "_out")
    (outclu,outputs) = rename outputs0 outpat (ClusterContextOutport box)
-- Box
    box = CircBox circName [inclu0] [] outclu

mkStrucFcn21
  :: Signal a
  => String
  -> (a->a->a)
  -> (StrucSig a -> StrucSig a -> StrucSig a)

mkStrucFcn21 circName circFcn inputs0 inputs1 = outputs
  where
--
-- Inputs (cluster 0)
    inputs0pat = ShapeName (circName ++ "_in0")
    (inclu0,inputs0') = rename inputs0 inputs0pat (ClusterContextInport box 0)
    x = inputs0'
-- Inputs (cluster 1)
    inputs1pat = ShapeName (circName ++ "_in1")
    (inclu1,inputs1') = rename inputs1 inputs1pat (ClusterContextInport box 1)
    y = inputs1'
-- Internal primitive behavior
    x' = getsigval x
    y' = getsigval y
    z = circFcn x' y'
    z' = StrucSigBase z
-- Outputs
    outputs0 = z'
    outpat = ShapeName (circName ++ "_out")
    (outclu,outputs) = rename outputs0 outpat (ClusterContextOutport box)
-- Box
    box = CircBox circName [inclu0,inclu1] [] outclu

------------------------------------------------------------------------
-- Simulating structural signals
------------------------------------------------------------------------

getsigval :: StrucSig a -> a
getsigval (StrucSigBase x) = x
getsigval (StrucSigRenamed x u) = getsigval x

------------------------------------------------------------------------
-- Transformation of a circuit specification
------------------------------------------------------------------------

------------------------------------------------------------------------
--     Overview of the transformation
------------------------------------------------------------------------

{- An argument to a circuit function may be a cluster of signals.  It
would be good to be able to generate a netlist with a cluster treated
as one signal (sometimes drawn as a thick or double line in schematic
diagrams) or as a collection of bit signals.  The tricky point here is
that in looking at a circuit, we just can't tell if a name refers to a
singleton bit without consulting the type, and it would not be
straightforward to examine the type in the syntactic transformation.

One nonworkable approach would be to replace a cluster such as
[a,b,c,d] with a data structure like Word [a,b,c,d].  This would
interfere with normal pattern matching equations.

The solution used in Hydra is to make each signal or cluster of signals

Basic transformation: each expression or pattern is a singleton.

Each box has a path, which enables that box to be found by starting at
the unique TopBox and traversing the path.  Similarly, each signal has
a path, which enables the signal to be found within a box. 

possible tools for  for printing the structures...
-- Data.PrettyPrint
-- package Groom
-- pretty-show

 -}

------------------------------------------------------------------------
--     Transform a source module
------------------------------------------------------------------------

{- Transform a set of top level declarations.  Typically this would be
an entire circuit specification module. -}

transformModule :: Q [Dec] -> Q [Dec]

transformModule topdecs =
  do starline
     xs <- topdecs
     runIO (putStrLn ("Transforming top level declarations: " ++
              show (length xs) ++ " top level declarations"))
     ys <- doit transformTopLevelDec xs
     starline
     return ys

------------------------------------------------------------------------
--     Transform a top level declaration
------------------------------------------------------------------------

-- Interface to the transformation; runs the worker function in the
-- Error monad and emits either the transformed code (if successful)
-- or the original code (if an error occurred).

transformTopLevelDec :: Dec -> Q Dec
transformTopLevelDec srcdec =
  do dashline
     runIO (putStrLn "Transforming a top level declaration")
     runIO (putStrLn "Original code:")
     runIO (putStrLn (prDec srcdec))
     runIO (putStrLn "Representation of original code:")
     runIO (putStrLn "(skipped)")
--     runIO (putStrLn (show srcdec))
     try <- runErrorT (trDecWorker srcdec)
     case try of
       Left err ->
         do runIO (putStrLn ("Error: " ++ show err))
            runIO (putStrLn "Invalid source, skipping transformation")
            return srcdec
       Right transformed ->
         do runIO (putStrLn "Transformation successful, result code:")
            runIO (putStrLn (prDec transformed))
            runIO (putStrLn "Representation of transformed code (raw):")
            runIO (putStrLn "(skipped)")
--            runIO (putStrLn (show transformed))
            return transformed

-- trDecWorker is a worker function that tries to transform a top
-- level declaration.  There are several cases, corresponding to the
-- various constructors of the Dec type.

trDecWorker :: Dec -> ErrQ Dec


-- body is NormalB Exp (the alternative to NormalB is a guarded body)

-- A circuit definition equation
trDecWorker src@(FunD circName clauses) =
  do case clauses of
       [Clause ps (NormalB e) eqs] ->
         do
-- Local names
            boxname <- lift (newName "box")
--            outportname <- lift (newName "outport")
-- Inports
            (ps',ces, pseqs) <- transformInps boxname 0 ps

-- Outport
            (outportname,outcluaccess,outeqs) <- transformOute boxname e

-- Box
            let internals = ListE []
            let inpsexps = ListE (map VarE ces)
            let boxexpr =
                  AppE
                    (AppE
                       (AppE
                         (AppE (ConE (mkName "CircBox"))
                               (LitE (StringL (show circName))))
                         inpsexps)
                       internals)
                    (VarE outcluaccess)
            let boxequ = mkequation (VarP boxname) boxexpr

-- Circuit definition
            let e' = outportname
            let eqs' = eqs ++ pseqs ++ outeqs ++ [boxequ]
            return (FunD circName [Clause ps' (NormalB (VarE e')) eqs'])

-- Badly formed source circuit: multiple clauses
       _ -> throwError (ErrMultiClauses circName)

-- All other cases correspond to a top level Dec that isn't part of
--Hydra.

trDecWorker badsrc =
  throwError ErrBadDec

{- WORKING HERE ???????????????? build box definition equation
box equation looks like this:
   boxname = CircBox [inportnames] [internals] outportname

mkBoxEqu :: Exp -> [ClusterAccess a] -> [ClusterAccess a]
  -> ClusterAccess a -> ErrQ [Dec]
mkBoxEqu boxname inps internals outp =
  lift [d| boxfoo = CircBox $(return boxname) $inps $internals $outp |]


NEED TO TURN ClusterAccess into an Exp ?.... what a pain

mkBoxEqu :: THname -> [ClusterAccess a] -> [ClusterAccess a]
  -> ClusterAccess a -> Dec
mkBoxEqu boxname inps internals outp =
  let rhs = AppE (AppE (AppE (ConE (mkName "CircBox")) inps) internals) outp
  in ValD (VarP boxname) (NormalB rhs) []

-}

------------------------------------------------------------------------
--     Transform an input cluster
------------------------------------------------------------------------

-- i is used to number the inport clusters 0, 1, 2, ...
   -- ce is expr for the cluster

transformInps :: THname -> Int -> [Pat] -> ErrQ ([Pat],[THname],[Dec])
transformInps boxname i [] = return ([],[],[])
transformInps boxname i (p:qs) =
  do (p',ce, peqs) <- transformInp boxname i p
     lift (runIO (putStrLn ("transformInps... ")))
--     lift (runIO (putStrLn (show p' ++ show peqs)))
     (qs',ces,qeqs) <- transformInps boxname (i+1) qs
     return (p':qs', ce:ces, peqs ++ qeqs)

{- Example: suppose the actual input pattern is [a,b,c,d], e.g. circ
 [a,b,c,d] = ...  Then this will be transformed to circ incluname with
 the following equations:

    [a,b,c,d] = incluname
    inputs0pat = ShapeWord [ShapeName (mkName "a"), ShapeName (mkName "b"),
                        ShapeName (mkName "c"),ShapeName (mkName "d")]
    (inclu0,inputs0') = rename inputs0 inputs0pat (ClusterContextInport box 0)
-}

--  Build representation of the application (ClusterContextInport box i)

mkUpInpExp :: Q Exp -> Q Exp -> ErrQ Exp
mkUpInpExp boxname i = lift [| ClusterContextInport $boxname $i |]

mkUpOutExp :: Q Exp -> ErrQ Exp
mkUpOutExp boxname = lift [| ClusterContextOutport $boxname |]

{-
mkUpInpExpclunky :: THname -> Int -> ErrQ Exp
mkUpInpExpclunky boxname i =
  do f1 <- lift (conE (mkName "ClusterContextInport"))
     bexp <- lift (varE boxname)
     f2 <- lift (appE (return f1) (return bexp))
     liti <- lift (litE (IntPrimL (fromIntegral i)))
     f3 <- lift (appE (return f2) (return liti))
     return f3
-}

mkRenameExp :: Q Exp -> Q Exp -> Q Exp -> ErrQ Exp
mkRenameExp a d up = lift [| rename $a $d $up |]

--  Example: if the input pattern is [a,b] then the transformed code
-- has inportName as the input pattern, and an equation { [a,b] =
-- inportName }.

-- Returns (1) a name to be used for the function argument; (2) a name
-- that will be bound to the expression that builds the input
-- ClusterAccess (this will be needed to construct the box), and (3) a
-- list of equations that must be included in the body of the
-- transformed function.

transformInp :: THname -> Int -> Pat -> ErrQ (Pat,THname,[Dec])
transformInp boxname i p = do

-- Names for values used to represent the input port
     let namePrefix = "inport_" ++ show i
     inportName <- lift (newName namePrefix)
     inportShape <- lift (newName (namePrefix ++ "_shape"))
     inportRenamed <- lift (newName (namePrefix ++ "_renamed"))
     inportCluAccess <- lift (newName (namePrefix ++ "_cluaccess"))

-- Equation defining the given inport pattern
     let inclueq = mkequation p (VarE inportRenamed)

-- Equation defining the shape of the inport cluster
     patRep <- patToClusterShape p
     patRepExp <- shapeToExp patRep
     let patEqu = mkequation (VarP inportShape) patRepExp

-- Equation defining the renamed cluster
     inpnum <- lift (litE (IntegerL (fromIntegral i)))
     up <- mkUpInpExp (return (VarE boxname)) (return inpnum)
     renExp <- mkRenameExp
                 (return (VarE inportName))
                 (return (VarE inportShape))
                 (return up)
     let renameEqu = mkequation
           (TupP [VarP inportCluAccess, VarP inportRenamed])
           renExp

     let equations = [inclueq, patEqu, renameEqu]
--     return (VarP inportName, inportRenamed, equations)
     return (VarP inportName, inportCluAccess, equations)

------------------------------------------------------------------------
--     Transform an expression cluster
------------------------------------------------------------------------

{- Renaming expressions (for outport and internal components).
Consider a circuit f x = e where ...  The output expression e uses
names that will be renamed to live within the box. Start by copying
transformInp to transformOutp, make necessary changes, then see
whether they can be handled by one more abstract function...  Don't
need i (inport number) Transform to

  f x = (foo,bar)

  f x = outport
    where  ...
          outport = e'
          (outaccess, outclu) = rename e

An expression like (inv x, y) uses names x and y that will be in the
local box.

An application like (and2 a b) denotes the output of an internal box,
and this needs to be renamed so the signal(s) are named in the current
box.

Need to add renameExp as a method to class Renameable; and change name
of rename to renamePat -}

transformOute :: THname -> Exp -> ErrQ (THname,THname,[Dec])
transformOute boxname e =
  do

-- Names for values used to represent the out port
     let namePrefix = "outport"
     outportName <- lift (newName namePrefix)
     outportShape <- lift (newName (namePrefix ++ "_Shape"))
     outportRenamed <- lift (newName (namePrefix ++ "_Renamed"))
     outportCluAccess <- lift (newName (namePrefix ++ "_CluAccess"))

-- Equation defining the given outport pattern
     let outclueq = mkequation (VarP outportName) (VarE outportRenamed)

-- Equation defining the shape of the outport cluster
     expRep <- expToClusterShape e
     lift (runIO (putStrLn ("transformOutE: shape = " ++ show expRep)))
     expRepExp <- shapeToExp expRep
     let expEqu = mkequation (VarP outportShape) expRepExp

-- Equation defining the renamed cluster
     up <- mkUpOutExp (return (VarE boxname))
     renExp <- mkRenameExp
--                 (return (VarE outportName))
                 (return e)   -- we are renaming the expression
                 (return (VarE outportShape))
                 (return up)
     let renameEqu = mkequation
           (TupP [VarP outportCluAccess, VarP outportRenamed])
           renExp

     let equations = [outclueq, expEqu, renameEqu]
     return (outportName, outportCluAccess, equations)

------------------------------------------------------------------------
--     Helper functions
------------------------------------------------------------------------

-- What's this called in the standard libraries???

doit :: Monad m => (a -> m a) -> [a] -> m [a]
doit f [] = return []
doit f (x:xs) =
  do y <- f x
     ys <- doit f xs
     return (y:ys)

-- Construct the TH representation of an equation, which has the form
-- (ValD lhspat (NormalB rhsexpr) [])

mkequation :: Pat -> Exp -> Dec
mkequation p e = ValD p (NormalB e) []

-- Tools for formatting the trace output from the transformation

charline c = runIO (putStrLn (take 72 (repeat c)))
dashline = charline '-'
starline = charline '*'
