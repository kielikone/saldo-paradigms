module TypesSw where

import General
import Dictionary
import Data.Functor.Invariant

-- parameter types for Swedish morphology

-- enumerated parameter types

data Genus = Utr | Neutr | GPl | GDPl | Pend | MascGen | FemGen | Human | PNeutr
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data Numerus = Sg | Pl
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data Species = Indef | Def
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data Casus = Nom | Gen
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data Sex = NoMasc | Masc
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data Modus = Ind | Conj
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data Vox = Act | SForm
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data Grade = Posit | Compar | Superl
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data Person = P1 | P2 | P3
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance Param Genus where 
    values = enum
    prValue g  = case g of
                     Utr     -> "u"
                     Neutr   -> "n"
                     GPl     -> "p"
                     GDPl    -> "d"
                     Pend    -> "v"
                     MascGen -> "m"
                     Human   -> "h"
                     FemGen  -> "f"
                     PNeutr  -> "w"

instance Param Numerus where 
    values = enum
    prValue Sg = "sg"
    prValue Pl = "pl"
instance Param Species where 
    values = enum
    prValue Indef = "indef"
    prValue Def   = "def"
instance Param Casus   where 
    values = enum
    prValue Nom = "nom"
    prValue Gen = "gen"
instance Param Sex     where 
    values = enum
    prValue NoMasc = "no_masc"
    prValue Masc   = "masc"
instance Param Modus   where 
    values = enum
    prValue Ind = "ind"
    prValue Conj = "konj"
instance Param Vox     where 
    values = enum
    prValue Act   = "aktiv"
    prValue SForm = "s-form"
instance Param Grade   where 
    values = enum
    prValue Posit = "pos"
    prValue Compar = "komp"
    prValue Superl = "super"
instance Param Person  where 
    values = enum
    prValue P1 = "p1"
    prValue P2 = "p2"
    prValue P3 = "p3"
--- would we need something more in Haskell?

-- real parameter types: mostly hierarchical

-- substantives (= common nouns)

type Substantive = SubstForm -> Str

data SubstForm = SF Numerus Species Casus | 
                 Composite |
		 Deriv
                 deriving (Eq, Ord, Show, Read)

instance Param SubstForm 
  where values = [SF a b c | a <- values, b <- values, c <- values]
                   ++ [Composite] -- Deriv removed
	prValue (SF a b c) = unwords [prValue a, prValue b, prValue c]
        prValue Composite  = "comp"
	prValue Deriv      = "deriv"

--data SubstFormA = SFA Casus
--  deriving (Eq, Ord, Show, Read)

--instance Param SubstFormA where
-- values = [SFA c | c <- values]
-- prValue (SFA c) = prValue c

data SubstM = SFM SubstForm
  deriving (Eq, Ord, Show, Read)

instance Param SubstM where
 values = [SFM t | t <- values]
 prValue (SFM t) = prValue t

-- adjectives

type Adjective = AdjForm -> Str

data GenNum = ASgUtr   |
              ASgNeutr |
              APl
  deriving (Eq, Ord, Show, Read,Enum,Bounded)

data SexNum = AxSg Sex | AxPl
  deriving (Eq, Ord, Show, Read)

data AdjFormPos =
    Strong GenNum
  | Weak   SexNum
  deriving (Eq, Ord, Show, Read)

data AdjFormSuper = SupStrong | SupWeak Sex
  deriving (Eq, Ord, Show, Read)

data AdjFormGrad =
    Pos    AdjFormPos
  | Comp  
  | Super  AdjFormSuper
  deriving (Eq, Ord, Show, Read)

data AdjForm = AF AdjFormGrad Casus
  deriving (Eq, Ord, Show, Read)

data AdjM = AdjM AdjForm
  deriving (Eq, Ord, Show, Read)

instance Param AdjM where
 values = [AdjM t | t <- values]
 prValue (AdjM t) = prValue t

data AdjMInv = AdjMInv
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance Param AdjMInv where
    values              = enum
    prValue (AdjMInv) = "invar"

instance Param GenNum where
  values = enum
  prValue (ASgUtr)   = "sg" ++ " " ++ prValue Utr
  prValue (ASgNeutr) = "sg" ++ " " ++ prValue Neutr
  prValue APl        = "pl"

instance Param SexNum where
  values = map AxSg values ++ [AxPl] -- needed for pronouns
  prValue (AxSg sex) = "sg" ++ " " ++ prValue sex
  prValue (AxPl)      = "pl"

instance Param AdjFormPos where 
  values = map Strong values ++ map Weak values
  prValue (Strong gn) = "indef " ++ prValue gn
  prValue (Weak   sn) = "def " ++ prValue sn

instance Param AdjFormSuper where 
    values = SupStrong : [SupWeak s | s <- values]
    prValue (SupStrong) = "indef"
    prValue (SupWeak s) = unwords ["def", prValue s]

instance Param AdjFormGrad where 
  values = map Pos values ++ [Comp] ++ map Super values
  prValue (Pos adj) = "pos " ++ prValue adj
  prValue (Comp)    = "komp"
  prValue (Super adj) = "super " ++ prValue adj

instance Param AdjForm where 
  values = [AF a c | a <- values, c <- values]
  prValue (AF a c) = unwords [prValue a, prValue c]

-- verbs

type Verb = VerbForm -> Str

data VFin = 
   Pres Modus Vox
 | Pret Modus Vox
 | Imper          --- no passive
  deriving (Eq, Ord, Show, Read)

data VInf =
   Inf Vox
 | Sup Vox
 | PtPres Casus
 | PtPret AdjFormPos Casus
  deriving (Eq, Ord, Show, Read)

data VerbForm = 
   VF VFin
 | VI VInf
  deriving (Eq, Ord, Show, Read)

instance Param VFin where
  values = map (uncurry Pres) mvs ++ map (uncurry Pret) mvs ++ [Imper] where
    mvs = [(m,v) | m <- values, v<- values]
  prValue (Pres m v) = unwords ["pres",prValue m, prValue v]
  prValue (Pret m v) = unwords ["pret",prValue m, prValue v]
  prValue (Imper) = "imper"

instance Param VInf where
  values = map Inf values ++ map Sup values ++ map PtPres values ++ 
           [PtPret a c | a <- values, c <- values]
  prValue (Inf v) = unwords ["inf", prValue v]
  prValue (Sup v) = unwords ["sup", prValue v]
  prValue (PtPres c)     = unwords ["pres_part",prValue c]
  prValue (PtPret adj c) = unwords ["pret_part",prValue adj, prValue c]

instance Param VerbForm where
  values = map VF values ++ map VI values
  value0 = VI (Inf Act) -- to show the infinitive as dictionary form
  prValue (VF f) = prValue f
  prValue (VI f) = prValue f

data VerbFormM = VM VerbForm
  deriving (Eq, Ord, Show, Read)

instance Param VerbFormM where
  values = map VM values 
  value0 = VM value0 
  prValue (VM vf) = prValue vf

type VerbM = VerbFormM -> Str

-- price to pay for hierarchical types: if we want to define non-passive verbs
passive_forms :: [VerbForm]
passive_forms = 
  map VF (concat [[Pres m SForm, Pret m SForm] | m <- values]) ++ 
  map VI [Inf SForm, Sup SForm] ++ part_pret_forms ++ conj_forms

active_forms :: [VerbForm]
active_forms =
  map VF (concat [[Pres m Act, Pret m Act] | m <- values]) ++ 
  map VI [Inf Act, Sup Act] ++ part_pret_forms ++ conj_forms

part_pret_forms :: [VerbForm]
part_pret_forms = [VI (PtPret a c) | a <- values, c <- values]

part_pres_forms :: [VerbForm]
part_pres_forms = [VI (PtPres c) | c <- values] 

part_forms :: [VerbForm]
part_forms = part_pres_forms ++ part_pret_forms 

conj_forms :: [VerbForm]
conj_forms = [VF (Pres Conj v) | v <- values] ++
            [VF (Pret Conj v) | v <- values]

positive_forms :: [AdjForm]
positive_forms = [AF (Pos a) c | a <- values, c <- values]

-- adverbs

type Adverb = AdverbForm -> Str

data AdverbForm = AdverbForm Grade
  deriving (Eq, Ord, Show, Read)

instance Param AdverbForm where 
			  values = [AdverbForm g | g <- values]
			  prValue (AdverbForm g) = prValue g

-- invariant adverbs

type AdverbInv = AdverbInvForm -> Str

data AdverbInvForm = AdverbInvForm
 deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance Param AdverbInvForm where 
			     values = enum
			     prValue _ = "invar"

type AdverbMInv = AdverbMInvForm -> Str

data AdverbMInvForm = AdverbMInvForm
 deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance Param AdverbMInvForm where 
			     values = enum
			     prValue _ = "invar"


-- invariant adjectives

type AdjInv = AdjInvForm -> Str

data AdjInvForm = AdjInvForm
 deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance Param AdjInvForm where 
			     values = enum
			     prValue _ = "invar"

data AdjCompInvForm = AdjCompInvForm
 deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance Param AdjCompInvForm where 
			     values = enum
			     prValue _ = "komp"

-- invariant interrogative adverbs

type InterrogInv = InterrogInvForm -> Str

data InterrogInvForm = InterrogInvForm
 deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance Param InterrogInvForm where 
			     values = enum
			     prValue _ = "invar"


-------------------------------
-- closed classes -------------
-------------------------------

-- pronouns

type PronPN  = PronCasus -> Str
type PronAdj = AdjPronForm -> Str

data PronCasus = PNom | PAcc | PGen GenNum
  deriving (Eq, Ord, Show, Read)

instance Param PronCasus where
  values = PNom : PAcc : map PGen values
  prValue (PNom) = "nom"
  prValue (PAcc) = "ack"
  prValue (PGen gn) = unwords ["poss", prValue gn]

data AdjPronForm = AP GenNum Casus
  deriving (Eq, Ord, Show, Read)

instance Param AdjPronForm where
  values = [AP g c | g <- values, c <- values]
  prValue (AP gn c) = unwords [prValue gn, prValue c]

type PronInv = PronInvForm -> Str

data PronInvForm = PronInvForm
 deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance Param PronInvForm where 
			     values = enum
			     prValue _ = "invar"

data PronMInvForm = PronMInvForm
 deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance Param PronMInvForm where 
			     values = enum
			     prValue _ = "invar"

type Number = NumForm -> Str

data NumOrd = Ordinal Sex | Numeral | NumeralN
  deriving (Eq, Ord, Show, Read)

instance Param NumOrd where
    values = (Numeral:NumeralN:[Ordinal s | s <- values])
    prValue (Ordinal m) = unwords ["ord", prValue m]
    prValue Numeral     = unwords ["num", prValue Utr]
    prValue NumeralN    = unwords ["num", prValue Neutr]

data NumForm = NumF Casus NumOrd
 deriving (Eq, Ord, Show, Read)

instance Param NumForm where 
  values = [NumF c o | c <- values, o <- values]
  prValue (NumF c o) = unwords [prValue c, prValue o]

-- invariant
data InterjForm = InterjForm 
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

type Interjection = InterjForm -> Str

instance Param InterjForm where 
  values = enum
  prValue _ = "invar"

-- articles

type Article = ArticleForm -> Str

data ArticleForm = ArticleForm GenNum
 deriving(Eq,Ord,Show,Read)

instance Param ArticleForm where 
   values = [ArticleForm g | g <- values]
   prValue (ArticleForm g) = prValue g

-- auxiliary verbs

data AuxVerbForm = AuxInf | AuxPres | AuxPret | AuxSup 
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance Param AuxVerbForm where 
    values = enum
    prValue (AuxInf)  = "inf"
    prValue (AuxPres) = "pres"
    prValue (AuxPret) = "pret"
    prValue (AuxSup)  = "sup"

type AuxVerb = AuxVerbForm -> Str

-- Prepositions

type Preposition = PrepForm -> Str

data PrepForm = PrepForm
 deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance Param PrepForm where 
			values = enum
			prValue _ = "invar"

-- Conjunction

type Conjunction = ConjForm -> Str

data ConjForm = ConjForm
 deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance Param ConjForm where 
			values = enum
			prValue _ = "invar"

-- Subjunction

type Subjunction = SubForm -> Str

data SubForm = SubForm
 deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance Param SubForm where 
		       values = enum
		       prValue _ = "invar"

-- Particles

type Particle = PartForm -> Str

data PartForm = PartForm
 deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance Param PartForm where 
			values = enum
			prValue _ = "invar"
		   
-- Infinitive mark
type InfMark = InfMarkForm -> Str

data InfMarkForm = InfMarkForm
 deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance Param InfMarkForm where 
    values = enum
    prValue _ = "invar"

-- Proper Noun
type PN = PNForm -> Str

data PNForm = PNForm Casus
 deriving (Eq, Ord, Show, Read)

instance Param PNForm where 
		      values = [PNForm c | c <- values]
		      prValue (PNForm c) = prValue c

data PNAForm = PNAForm Casus
 deriving (Eq, Ord, Show, Read)

instance Param PNAForm where 
		      values = [PNAForm c | c <- values]
		      prValue (PNAForm c) = prValue c

data PNMForm = PNMForm Casus
 deriving (Eq, Ord, Show, Read)

instance Param PNMForm where 
		      values = [PNMForm c | c <- values]
		      prValue (PNMForm c) = prValue c

-- Abbreviations
data ABAForm = ABAForm
 deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance Param ABAForm where 
    values = enum
    prValue _ = "invar"

-- Dictionary instances
-- Part of Speech Declarations.
-- (Dict instances)
instance Dict SubstForm       where 
 category     _  = "nn"
 attrException _ = [(Composite,1), (Deriv,2)]

instance Dict SubstM  where 
 category     _  = "nnm"
 attrException _ = [(SFM Composite,1), (SFM Deriv,2)]

-- instance Dict SubstFormA      where category _ = "nna"
instance Dict AdjForm         where category _ = "av"
instance Dict AdjM            where category _ = "avm"
instance Dict AdjMInv         where category _ = "avm"

instance Dict VerbForm        where 
    category _ = "vb"
    dictword f = case unStr (f (VI (Inf Act))) of
                   (x:_) -> x
                   _     -> getDictWord f
instance Dict VerbFormM       where category _ = "vbm"
instance Dict AdverbForm      where category _ = "ab"
instance Dict AdverbInvForm   where category _ = "ab"
instance Dict AdjInvForm      where category _ = "av"
instance Dict AdjCompInvForm  where category _ = "av"
instance Dict PronCasus       where category _  = "pn"
instance Dict InterjForm      where category _  = "in"
instance Dict ArticleForm     where category _  = "al"
instance Dict AuxVerbForm     where category _  = "vb"
instance Dict PrepForm        where category _  = "pr"
instance Dict ConjForm        where category _  = "kn"
instance Dict SubForm         where category _  = "sn"
instance Dict PartForm        where category _  = "pp"
instance Dict InfMarkForm     where category _  = "ie"
instance Dict ABAForm         where category _  = "aba"
instance Dict AdverbMInvForm  where category _  = "abm"
instance Dict PNForm          where category _  = "pm"
instance Dict PNAForm         where category _  = "pma"
instance Dict PNMForm         where category _  = "pmm"
instance Dict AdjPronForm     where category _  = "pn"
instance Dict PronInvForm     where category _  = "pn"
instance Dict PronMInvForm     where category _ = "pnm"
instance Dict NumForm         where category _  = "nl"
instance Dict InterrogInvForm where category _  = "pn" -- ??

