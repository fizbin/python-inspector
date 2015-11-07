{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}

import Control.Applicative
import Control.Lens.Lens
import Control.Lens.Traversal
import Data.Foldable
import qualified Data.Set as S
import Language.Python.Common.AST
import Language.Python.Common.SrcLocation

deriving instance Foldable Ident
deriving instance Foldable Argument
deriving instance Foldable CompIf
deriving instance Foldable CompIter
deriving instance Foldable CompFor
deriving instance Foldable DictMappingPair
deriving instance Foldable ComprehensionExpr
deriving instance Foldable Comprehension
deriving instance Foldable Op
deriving instance Foldable ParamTuple
deriving instance Foldable Parameter
deriving instance Foldable Slice
deriving instance Foldable YieldArg
deriving instance Foldable Expr
deriving instance Foldable AssignOp
deriving instance Foldable Decorator
deriving instance Foldable FromItem
deriving instance Foldable FromItems
deriving instance Foldable ExceptClause
deriving instance Foldable Handler
deriving instance Foldable ImportItem
deriving instance Foldable ImportRelative
deriving instance Foldable RaiseExpr
deriving instance Foldable Statement

deriving instance Traversable Ident
deriving instance Traversable Argument
deriving instance Traversable CompIf
deriving instance Traversable CompIter
deriving instance Traversable CompFor
deriving instance Traversable DictMappingPair
deriving instance Traversable ComprehensionExpr
deriving instance Traversable Comprehension
deriving instance Traversable Op
deriving instance Traversable ParamTuple
deriving instance Traversable Parameter
deriving instance Traversable Slice
deriving instance Traversable YieldArg
deriving instance Traversable Expr
deriving instance Traversable AssignOp
deriving instance Traversable Decorator
deriving instance Traversable FromItem
deriving instance Traversable FromItems
deriving instance Traversable ExceptClause
deriving instance Traversable Handler
deriving instance Traversable ImportItem
deriving instance Traversable ImportRelative
deriving instance Traversable RaiseExpr
deriving instance Traversable Statement

main :: IO ()
main = return ()

class HasExprs s where
  myExprs :: Traversal' (s t) (Expr t)

lensIdent :: Lens (Ident a) (Ident b) a b
lensIdent f (Ident {ident_string=is, ident_annot=ia}) = Ident is `fmap` f ia

instance HasExprs Argument where
  myExprs f (ArgKeyword ident expr aannot) =
    ArgKeyword <$> pure ident <*> f expr <*> pure aannot
  myExprs f (ArgVarArgsKeyword expr aannot) =
    flip ArgVarArgsKeyword aannot <$> f expr
  myExprs f (ArgVarArgsPos expr aannot) =
    flip ArgVarArgsPos aannot <$> f expr
  myExprs f (ArgExpr expr aannot) =
    flip ArgExpr aannot <$> f expr

instance HasExprs Slice where
  myExprs f (SliceProper l u s a) =
    SliceProper <$> traverse f l <*> traverse f u <*> (traverse . traverse) f s <*> pure a
  myExprs f (SliceExpr e a) = flip SliceExpr a <$> f e
  myExprs _ x@(SliceEllipsis _) = pure x

instance HasExprs Parameter where
  myExprs f (Param name py_annot d a) = Param name <$> traverse f py_annot <*>
                                        traverse f d <*> pure a
  myExprs f (VarArgsPos name py_annot a) = VarArgsPos name <$> traverse f py_annot <*> pure a
  myExprs f (VarArgsKeyword name py_annot a) = VarArgsKeyword name <$>
                                               traverse f py_annot <*> pure a
  myExprs _ x@(EndPositional _) = pure x
  myExprs f (UnPackTuple tuple d a) = UnPackTuple tuple <$> traverse f d <*> pure a

traverseParamTupleIdent :: Traversal' (ParamTuple a) (Ident a)
traverseParamTupleIdent f (ParamTupleName n a) = ParamTupleName <$> f n <*> pure a
traverseParamTupleIdent f (ParamTuple t a) =
  ParamTuple <$> (traverse . traverseParamTupleIdent) f t <*> pure a

traverseParameterIdent :: Traversal' (Parameter a) (Ident a)
traverseParameterIdent f (Param name py_annot d a) = Param <$> f name <*> pure py_annot
                                                     <*> pure d <*> pure a
traverseParameterIdent f (VarArgsPos name py_annot a) = VarArgsPos <$> f name
                                                        <*> pure py_annot <*> pure a
traverseParameterIdent f (VarArgsKeyword name py_annot a) = VarArgsKeyword <$> f name
                                                            <*> pure py_annot <*> pure a
traverseParameterIdent _ x@(EndPositional _) = pure x
traverseParameterIdent f (UnPackTuple tuple d a) =
  UnPackTuple <$> traverseParamTupleIdent f tuple <*> pure d <*> pure a

traverseAnnotatedTail :: (Annotated t, Functor t) => Lens (t a) (t b) a b
traverseAnnotatedTail f x = (\q -> fmap (const q) x) `fmap` f (annot x)

-- Then I need to write a HasExpr instance for Statement,
-- and a Plated instance for Statement and Expr

data FuncVarEnv = FuncVarEnv {_fveUsed :: S.Set String, _fveBound :: S.Set String}

tx :: Statement SrcSpan -> Statement (SrcSpan, Maybe FuncVarEnv)
tx = undefined


