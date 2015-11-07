{-# LANGUAGE ScopedTypeVariables #-}

import Control.Applicative
import Control.Lens.Lens
import Control.Lens.Traversal
import qualified Data.Set as S
import Language.Python.Common.AST
import Language.Python.Common.SrcLocation

main :: IO ()
main = return ()

lensIdent :: Lens (Ident a) (Ident b) a b
lensIdent f (Ident {ident_string=is, ident_annot=ia}) = Ident is `fmap` f ia

traverseArgument :: Traversal (Argument a) (Argument b) a b
traverseArgument f (ArgKeyword ident expr aannot) = ArgKeyword <$> lensIdent f ident
                                                    <*> traverseExpr f  expr
                                                    <*> f aannot
traverseArgument f (ArgVarArgsKeyword expr aannot) = ArgVarArgsKeyword <$>
                                                     traverseExpr f expr
                                                     <*> f aannot
traverseArgument f (ArgVarArgsPos expr aannot) = ArgVarArgsPos <$>
                                                 traverseExpr f expr
                                                 <*> f aannot
traverseArgument f (ArgExpr expr aannot) = ArgExpr <$>
                                           traverseExpr f expr
                                           <*> f aannot

traverseSlice :: Traversal (Slice a) (Slice b) a b
traverseSlice f (SliceProper l u s a) =
  SliceProper <$> (traverse . traverseExpr) f l
  <*> (traverse . traverseExpr) f u
  <*> (traverse . traverse . traverseExpr) f s
  <*> f a
traverseSlice f (SliceExpr e a) = SliceExpr <$> traverseExpr f e <*> f a
traverseSlice f (SliceEllipsis a) = SliceEllipsis <$> f a

traverseAnnotatedTail :: (Annotated t, Functor t) => Lens (t a) (t b) a b
traverseAnnotatedTail f x = (\q -> fmap (const q) x) `fmap` f (annot x)

traverseExpr :: Traversal (Expr a) (Expr b) a b
traverseExpr f (Var{var_ident=vident, expr_annot=eannot }) =
  Var <$> lensIdent f vident <*> f eannot
traverseExpr f (Call {call_fun=cfun, call_args=cargs, expr_annot=eannot}) =
  Call <$> traverseExpr f cfun <*> (traverse . traverseArgument) f cargs <*> f eannot
traverseExpr f (Subscript subscriptee' subscript_expr' expr_annot') =
  Subscript <$> traverseExpr f subscriptee' <*> traverseExpr f subscript_expr'
  <*> f expr_annot'
traverseExpr f (SlicedExpr slicee' slices' expr_annot') =
  SlicedExpr <$> traverseExpr f slicee' <*> (traverse . traverseSlice) f slices'
  <*> f expr_annot'
traverseExpr f (CondExpr ce_true_branch' ce_condition' ce_false_branch' expr_annot') =
  CondExpr <$> traverseExpr f ce_true_branch' <*> traverseExpr f ce_condition'
  <*> traverseExpr f ce_false_branch' <*> f expr_annot'
traverseExpr f (BinaryOp operator' left_op_arg' right_op_arg' expr_annot') =
  BinaryOp <$> traverseAnnotatedTail f operator'
  <*> traverseExpr f left_op_arg' <*> traverseExpr f right_op_arg' <*> f expr_annot'
traverseExpr f (UnaryOp operator' op_arg' expr_annot') =
  UnaryOp <$> traverseAnnotatedTail f operator' <*> traverseExpr f op_arg' <*> f expr_annot'
traverseExpr f (Dot dot_expr' dot_attribute' expr_annot') =
  Dot <$> traverseExpr f dot_expr' <*> lensIdent f dot_attribute' <*> f expr_annot'

{- Expr constructors. See https://hackage.haskell.org/package/language-python-0.5.2/docs/Language-Python-Common-AST.html#t:Expr

   | UnaryOp { operator :: Op annot, op_arg :: Expr annot, expr_annot :: annot }
   | Dot { dot_expr :: Expr annot, dot_attribute :: Ident annot, expr_annot :: annot }
   | Lambda { lambda_args :: [Parameter annot], lambda_body :: Expr annot, expr_annot :: annot }
   | Tuple { tuple_exprs :: [Expr annot], expr_annot :: annot }
   | Yield { yield_arg :: Maybe (YieldArg annot) , expr_annot :: annot}
   | Generator { gen_comprehension :: Comprehension annot, expr_annot :: annot }
   | ListComp { list_comprehension :: Comprehension annot, expr_annot :: annot }
   | List { list_exprs :: [Expr annot], expr_annot :: annot }
   | Dictionary { dict_mappings :: [DictMappingPair annot], expr_annot :: annot }
   | DictComp { dict_comprehension :: Comprehension annot, expr_annot :: annot }
   | Set { set_exprs :: [Expr annot], expr_annot :: annot } 
   | SetComp { set_comprehension :: Comprehension annot, expr_annot :: annot }
   | Starred { starred_expr :: Expr annot, expr_annot :: annot }
   | Paren { paren_expr :: Expr annot, expr_annot :: annot }
   | StringConversion { backquoted_expr :: Expr annot, expr_anot :: annot }
-}
-- default for Expr constructors that don't contain recursive types with "annot"
traverseExpr f x = traverseAnnotatedTail f x

data FuncVarEnv = FuncVarEnv {_fveUsed :: S.Set String, _fveBound :: S.Set String}

tx :: Statement SrcSpan -> Statement (SrcSpan, Maybe FuncVarEnv)
tx = undefined


t = FromImport {
  from_module = ImportRelative {
     import_relative_dots = 0,
     import_relative_module = Just [
       Ident {
          ident_string = "__future__",
          ident_annot = SpanCoLinear {
            span_filename = "Ontology.py",
            span_row = 6,
            span_start_column = 6,
            span_end_column = 15}}]
     , import_relative_annot = SpanCoLinear {
       span_filename = "Ontology.py",
       span_row = 6,
       span_start_column = 6,
       span_end_column = 15}}
  , from_items = FromItems {
     from_items_items = [
        FromItem {
           from_item_name = Ident {
              ident_string = "absolute_import",
              ident_annot = SpanCoLinear {
                span_filename = "Ontology.py",
                span_row = 6, span_start_column = 24, span_end_column = 38}}
           , from_as_name = Nothing
           , from_item_annot = SpanCoLinear {
             span_filename = "Ontology.py",
             span_row = 6, span_start_column = 24, span_end_column = 38}}
        ]
     , from_items_annot = SpanCoLinear {
        span_filename = "Ontology.py",
        span_row = 6, span_start_column = 24, span_end_column = 38}}
  , stmt_annot = SpanCoLinear {
     span_filename = "Ontology.py",
     span_row = 6, span_start_column = 1, span_end_column = 38}}

