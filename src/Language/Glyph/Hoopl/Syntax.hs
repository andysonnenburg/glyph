{-# LANGUAGE GADTs, ScopedTypeVariables #-}
module Language.Glyph.Hoopl.Syntax
       ( module X
       , Stmt (..)
       , StmtView (..)
       , ExprView (..)
       , ExprIdent
       , prettyGraph
       ) where

import Compiler.Hoopl

import Data.Foldable (Foldable, toList)
import Data.Maybe

import Language.Glyph.Ident
import Language.Glyph.Syntax as X (Lit (..),
                                   Name,
                                   NameView,
                                   ident,
                                   prettyNameView)

import Text.PrettyPrint.Free hiding (encloseSep, tupled)

data Stmt a e x where
  Stmt :: a -> StmtView a x -> Stmt a O x
  Expr :: a -> ExprIdent -> ExprView a -> MaybeC x (Label, Label) -> Stmt a O x
  Label :: Label -> Stmt a C O
  Goto :: Label -> Stmt a O C
  Catch :: Ident -> Label -> Stmt a C O
  ReturnVoid :: Stmt a O C

instance Show (Stmt a e x) where
  show = show . pretty

data StmtView a x where
  ExprS :: ExprIdent -> StmtView a O
  VarDeclS :: Name -> Maybe ExprIdent -> StmtView a O
  FunDeclS :: Name -> [Name] -> Graph (Stmt a) O C -> StmtView a O
  ReturnS :: ExprIdent -> StmtView a C
  IfS :: ExprIdent -> Label -> Label -> StmtView a C
  ThrowS :: ExprIdent -> Maybe Label -> StmtView a C

data ExprView a where
  LitE :: Lit -> ExprView a
  NotE :: ExprIdent -> ExprView a
  VarE :: Ident -> Maybe NameView -> ExprView a
  FunE :: Ident -> [Name] -> Graph (Stmt a) O C -> ExprView a
  ApplyE :: ExprIdent -> [ExprIdent] -> ExprView a
  AssignE :: Name -> ExprIdent -> ExprView a

type ExprIdent = Ident

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
      go (Expr _ x expr (JustC (nextLabel, catchLabel))) =
        prettyExpr x expr <+>
        text "to" <+> prettyLabel nextLabel <+>
        text "unwind" <+> prettyLabel catchLabel <>
        semi
      go (Expr _ x expr NothingC) =
        prettyExpr x expr <> semi
      go (Label label) =
        prettyLabel label <> colon
      go (Goto label) =
        prettyGoto label
      go (Catch x label) =
        prettyCatch (prettyIdent x) label
      go ReturnVoid =
        text "return" <+> pretty VoidL <> semi
      
      prettyCatch :: Doc e' -> Label -> Doc e'
      prettyCatch nameDoc label =
        prettyLabel label <> colon <+> text "catch" <+> parens nameDoc
      
      prettyExpr :: ExprIdent -> ExprView a -> Doc e'
      prettyExpr x expr =
        text "let" <+> prettyIdent x <+> char '=' <+> pretty expr

instance Pretty (StmtView a x) where
  pretty = go
    where
      go :: StmtView a x -> Doc e
      go (ExprS expr) =
        prettyIdent expr <> semi
      go (VarDeclS name Nothing) =
        varDecl name
      go (VarDeclS name (Just x)) =
        varDef name x
      go (FunDeclS name params graph) =
        text "fn" <+> pretty name <> tupled (map pretty params) <+> lbrace <>
        (enclose linebreak linebreak . indent 2 . prettyGraph $ graph) <>
        rbrace
      go (ReturnS expr) =
        text "return" <+> prettyIdent expr <> semi 
      go (IfS expr then' else') =
        text "if" <+> parens (prettyIdent expr) <+>
        prettyLabel then' <+>
        prettyLabel else' <>
        semi
      go (ThrowS expr _maybeCatchLabel) =
        text "throw" <+> prettyIdent expr <> semi
      
      varDef :: Name -> ExprIdent -> Doc e
      varDef name x =
        var name <+> char '=' <+> prettyIdent x  <> semi
      
      varDecl :: Name -> Doc e
      varDecl name =
        var name <> semi
      
      var :: Name -> Doc e
      var name =
        text "var" <+> pretty name

prettyGoto :: Label -> Doc e
prettyGoto label =
  text "goto" <+> prettyLabel label <> semi

instance Pretty (ExprView a) where
  pretty = go
    where
      go (LitE lit) =
        pretty lit
      go (NotE x) =
        char '!' <> prettyIdent x
      go (VarE _ (Just nameView)) =
        prettyNameView nameView
      go (VarE x Nothing) =
        prettyIdent x
      go (FunE _ params graph) =
        text "fn" <+> tupled (map pretty params) <+> lbrace <>
        (enclose linebreak linebreak . indent 2 . prettyGraph $ graph) <>
        rbrace
      go (ApplyE expr exprs) =
        prettyIdent expr <> tupled (map prettyIdent exprs)
      go (AssignE name x) =
        pretty name <+> char '=' <+> prettyIdent x

prettyIdent :: Ident -> Doc e
prettyIdent =
  text . show

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
          go (Expr _ _ _ (JustC (next, catch'))) = [next, catch']
          go (Goto label) = [label]
          go ReturnVoid = []
      
      stmtViewSuccessors = go
        where
          go :: StmtView a C -> [Label]
          go (ReturnS _) =
            []
          go (IfS _ thenLabel elseLabel) =
            [thenLabel, elseLabel]
          go (ThrowS _ maybeCatchLabel') =
            maybeToList maybeCatchLabel'

instance HooplNode (Stmt a) where
  mkBranchNode = Goto
  mkLabelNode = Label
