{-# LANGUAGE GADTs, ScopedTypeVariables #-}
module Language.Glyph.Hoopl.Syntax
       ( module X
       , Stmt (..)
       , StmtView (..)
       , VarInit (..)
       , Expr (..)
       , ExprView (..)
       , prettyGraph
       ) where

import Compiler.Hoopl

import Data.Foldable (Foldable, toList)
import Data.Maybe

import Language.Glyph.Ident
import Language.Glyph.Syntax as X (Lit (..), Name, ident)

import Text.PrettyPrint.Free hiding (encloseSep, tupled)

data Stmt a e x where
  Stmt :: a -> StmtView a x -> Stmt a O x
  Label :: Label -> Stmt a C O
  Goto :: Label -> Stmt a O C
  Catch :: Maybe Name -> Label -> Stmt a C O
  ReturnVoid :: Stmt a O C

instance Show (Stmt a e x) where
  show = show . pretty

data StmtView a x where
  ExprS :: Expr a -> MaybeC x (Label, Label) -> StmtView a x
  VarDeclS :: Name -> VarInit a x -> StmtView a x
  FunDeclS :: Name -> [Name] -> Graph (Stmt a) O C -> StmtView a O
  ReturnS :: Expr a -> Maybe Label -> StmtView a C
  IfS :: Expr a -> Label -> Label -> Maybe Label -> StmtView a C
  ThrowS :: Expr a -> Maybe Label -> StmtView a C

data VarInit a x where
  UndefinedO :: VarInit a O
  ExprO :: Expr a -> VarInit a O
  ExprC :: Expr a -> Label -> Label -> VarInit a C

data Expr a = Expr a (ExprView a)

data ExprView a where
  LitE :: Lit -> ExprView a
  NotE :: Expr a -> ExprView a
  VarE :: Name -> ExprView a
  FunE :: Ident -> [Name] -> Graph (Stmt a) O C -> ExprView a
  ApplyE :: Expr a -> [Expr a] -> ExprView a
  AssignE :: Name -> Expr a -> ExprView a


instance Show (StmtView a x) where
  show = show . pretty

prettyGraph :: Graph (Stmt a) e x -> Doc e'
prettyGraph = go
  where
    go :: Graph (Stmt a) e x -> Doc e'
    go GNil =
      empty
    go (GUnit unit) =
      vcat $ block unit []
    go (GMany entry blocks exit) =
      vcat $
      open (flip block []) entry ++
      body blocks ++
      open (flip block []) exit
    
    open :: (a -> [Doc e]) -> MaybeO z a -> [Doc e]
    open _ NothingO = []
    open p (JustO x) = p x
    
    body :: LabelMap (Block (Stmt a) C C) -> [Doc e]
    body blocks =
      concatMap (flip block []) . mapElems $ blocks
    
    block :: forall a e x e' .
             Block (Stmt a) e x ->
             IndexedCO x [Doc e'] [Doc e'] ->
             IndexedCO e [Doc e'] [Doc e']
    block = foldBlockNodesB f
      where
        f :: forall entry exit . Stmt a entry exit -> [Doc e'] -> [Doc e']
        f stmt docs = pretty stmt : docs

instance Pretty (Stmt a e x) where
  pretty = go
    where
      go :: Stmt a e x -> Doc e'
      go (Stmt _ x) =
        pretty x
      go (Label label) =
        prettyLabel label <> colon
      go (Goto label) =
        text "goto" <+> prettyLabel label <> semi
      go (Catch Nothing _label) =
        text "catch" <+> parens (text "...") <> colon
      go (Catch (Just name) _label) =
        text "catch" <+> parens (pretty name) <> colon
      go ReturnVoid =
        text "return" <+> pretty VoidL <> semi

instance Pretty (StmtView a x) where
  pretty = go
    where
      go :: StmtView a x -> Doc e
      go (ExprS expr NothingC) =
        pretty expr <> semi
      go (ExprS expr (JustC (nextLabel, _catchLabel))) =
        pretty expr <> semi
        `above`
        goto nextLabel
      go (VarDeclS name UndefinedO) =
        varDecl name
      go (VarDeclS name (ExprO expr)) =
        varDef name expr
      go (VarDeclS name (ExprC expr nextLabel _catchLabel)) =
        varDef name expr
        `above`
        goto nextLabel
      go (FunDeclS name params graph) =
        text "fn" <+> pretty name <> tupled (map pretty params) <+> lbrace <>
        (enclose linebreak linebreak . indent 2 . prettyGraph $ graph) <>
        rbrace
      go (ReturnS expr _maybeCatchLabel) =
        text "return" <+> pretty expr <> semi 
      go (IfS expr then' else' _maybeCatchLabel) =
        text "if" <+> parens (pretty expr) <+>
        prettyLabel then' <+>
        prettyLabel else' <>
        semi
      go (ThrowS expr _maybeCatchLabel) =
        text "throw" <+> pretty expr <> semi
      
      varDef :: Name -> Expr a -> Doc e
      varDef name expr =
        var name <+> char '=' <+> pretty expr <> semi
      
      varDecl :: Name -> Doc e
      varDecl name =
        var name <> semi
      
      var :: Name -> Doc e
      var name =
        text "var" <+> pretty name

      goto :: Label -> Doc e
      goto label =
        text "goto" <+> prettyLabel label <> semi

instance Pretty (Expr a) where
  pretty (Expr _ x) = pretty x

instance Pretty (ExprView a) where
  pretty = go
    where
      go (LitE lit) =
        pretty lit
      go (NotE expr) =
        char '!' <> pretty expr
      go (VarE name) =
        pretty name
      go (FunE _ params graph) =
        text "fn" <+> tupled (map pretty params) <+> lbrace <>
        (enclose linebreak linebreak . indent 2 . prettyGraph $ graph) <>
        rbrace
      go (ApplyE expr exprs) =
        pretty expr <> tupled (map pretty exprs)
      go (AssignE name expr) =
        pretty name <+> char '=' <+> pretty expr

prettyLabel :: Label -> Doc e
prettyLabel = text . show

tupled :: Foldable f => f (Doc e) -> Doc e
tupled = encloseSep lparen rparen (comma <> space)

encloseSep :: Foldable f => Doc e -> Doc e -> Doc e -> f (Doc e) -> Doc e
encloseSep left right sp ds0 =
  case toList ds0 of
    [] -> left <> right
    [d] -> left <> d <> right
    ds -> left <> align (cat (zipWith (<>) (init ds) (repeat sp) ++ [last ds <> right]))

instance NonLocal (Stmt a) where
  entryLabel = go
    where
      go (Label label) = label
      go (Catch _maybeName label) = label
  
  
  successors = stmtSuccessors
    where
      stmtSuccessors = go
        where
          go (Stmt _ x) = stmtViewSuccessors x
          go (Goto label) = [label]
          go ReturnVoid = []
      
      stmtViewSuccessors = go
        where
          go :: StmtView a C -> [Label]
          go (ExprS _ (JustC (nextLabel, catchLabel))) =
            [nextLabel, catchLabel]
          go (VarDeclS _ (ExprC _ nextLabel catchLabel)) =
            [nextLabel, catchLabel]
          go (ReturnS _ maybeCatchLabel') =
            maybeToList maybeCatchLabel'
          go (IfS _ thenLabel elseLabel maybeCatchLabel') =
            [thenLabel, elseLabel] ++ maybeToList maybeCatchLabel'
          go (ThrowS _ maybeCatchLabel') =
            maybeToList maybeCatchLabel'

instance HooplNode (Stmt a) where
  mkBranchNode = Goto
  mkLabelNode = Label
