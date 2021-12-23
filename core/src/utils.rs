extern crate proc_macro;
// TODO Make macro to minimise code duplication here.

type UnwrapResult<'a, T> = Result<&'a T, &'static str>;
type UnwrapResultMut<'a, T> = Result<&'a mut T, &'static str>;

pub trait UnwrapReturnType {
    fn type_(&self) -> UnwrapResult<syn::Type>;
}
impl UnwrapReturnType for syn::ReturnType {
    fn type_(&self) -> UnwrapResult<syn::Type> {
        match self {
            Self::Type(_, typed_) => Ok(&**typed_),
            _ => Err("called `ReturnType::type_()` on a non `Type` value"),
        }
    }
}

pub trait UnwrapType {
    fn path(&self) -> UnwrapResult<syn::TypePath>;
}
impl UnwrapType for syn::Type {
    fn path(&self) -> UnwrapResult<syn::TypePath> {
        match self {
            Self::Path(path) => Ok(path),
            _ => Err("called `Type::path()` on a non `Path` value"),
        }
    }
}
pub trait UnwrapLit {
    fn float(&self) -> UnwrapResult<syn::LitFloat>;
}
impl UnwrapLit for syn::Lit {
    fn float(&self) -> UnwrapResult<syn::LitFloat> {
        match self {
            Self::Float(float) => Ok(float),
            _ => Err("called `Lit::float()` on a non `Float` value"),
        }
    }
}
pub trait UnwrapTokenTree {
    fn ident(&self) -> UnwrapResult<proc_macro::Ident>;
    fn literal(&self) -> UnwrapResult<proc_macro::Literal>;
}
impl UnwrapTokenTree for proc_macro::TokenTree {
    fn ident(&self) -> UnwrapResult<proc_macro::Ident> {
        match self {
            Self::Ident(local) => Ok(local),
            _ => Err("called `TokenTree::ident()` on a non `Ident` value"),
        }
    }
    fn literal(&self) -> UnwrapResult<proc_macro::Literal> {
        match self {
            Self::Literal(lit) => Ok(lit),
            _ => Err("called `TokenTree::literal()` on a non `Literal` value"),
        }
    }
}

pub trait UnwrapStmt {
    fn local(&self) -> UnwrapResult<syn::Local>;
    fn local_mut(&mut self) -> UnwrapResultMut<syn::Local>;
    fn semi(&self) -> UnwrapResult<syn::Expr>;
    fn semi_mut(&mut self) -> UnwrapResultMut<syn::Expr>;
}
impl UnwrapStmt for syn::Stmt {
    fn local(&self) -> UnwrapResult<syn::Local> {
        match self {
            Self::Local(local) => Ok(local),
            _ => Err("called `Stmt::local()` on a non `Local` value"),
        }
    }
    fn local_mut(&mut self) -> UnwrapResultMut<syn::Local> {
        match self {
            Self::Local(local) => Ok(local),
            _ => Err("called `Stmt::local_mut()` on a non `Local` value"),
        }
    }
    fn semi(&self) -> UnwrapResult<syn::Expr> {
        match self {
            Self::Semi(expr, _) => Ok(expr),
            _ => Err("called `Stmt::semi()` on a non `Semi` value"),
        }
    }
    fn semi_mut(&mut self) -> UnwrapResultMut<syn::Expr> {
        match self {
            Self::Semi(expr, _) => Ok(expr),
            _ => Err("called `Stmt::semi_mut()` on a non `Semi` value"),
        }
    }
}
pub trait IsStmt {
    fn is_local(&self) -> bool;
    fn is_semi(&self) -> bool;
}
impl IsStmt for syn::Stmt {
    fn is_local(&self) -> bool {
        matches!(self, Self::Local(_))
    }
    fn is_semi(&self) -> bool {
        matches!(self, Self::Semi(_, _))
    }
}
pub trait UnwrapPat {
    fn ident_mut(&mut self) -> UnwrapResultMut<syn::PatIdent>;
    fn ident(&self) -> UnwrapResult<syn::PatIdent>;
    fn tuple_mut(&mut self) -> UnwrapResultMut<syn::PatTuple>;
    fn tuple(&self) -> UnwrapResult<syn::PatTuple>;
}
impl UnwrapPat for syn::Pat {
    fn ident_mut(&mut self) -> UnwrapResultMut<syn::PatIdent> {
        match self {
            Self::Ident(ident) => Ok(ident),
            _ => Err("called `Pat::ident()` on a non `Ident` value"),
        }
    }
    fn ident(&self) -> UnwrapResult<syn::PatIdent> {
        match self {
            Self::Ident(ident) => Ok(ident),
            _ => Err("called `Pat::ident()` on a non `Ident` value"),
        }
    }
    fn tuple_mut(&mut self) -> UnwrapResultMut<syn::PatTuple> {
        match self {
            Self::Tuple(tuple) => Ok(tuple),
            _ => Err("called `Pat::tuple_mut()` on a non `Tuple` value"),
        }
    }
    fn tuple(&self) -> UnwrapResult<syn::PatTuple> {
        match self {
            Self::Tuple(tuple) => Ok(tuple),
            _ => Err("called `Pat::tuple()` on a non `Tuple` value"),
        }
    }
}
pub trait IsExpr {
    fn is_binary(&self) -> bool;
    fn is_path(&self) -> bool;
    fn is_return(&self) -> bool;
    fn is_call(&self) -> bool;
}
impl IsExpr for syn::Expr {
    fn is_binary(&self) -> bool {
        matches!(self, Self::Binary(_))
    }
    fn is_path(&self) -> bool {
        matches!(self, Self::Path(_))
    }
    fn is_return(&self) -> bool {
        matches!(self, Self::Return(_))
    }
    fn is_call(&self) -> bool {
        matches!(self, Self::Call(_))
    }
}
pub trait UnwrapExpr {
    fn binary(&self) -> UnwrapResult<syn::ExprBinary>;
    fn binary_mut(&mut self) -> UnwrapResultMut<syn::ExprBinary>;
    fn block(&self) -> UnwrapResult<syn::ExprBlock>;
    fn block_mut(&mut self) -> UnwrapResultMut<syn::ExprBlock>;
    fn path(&self) -> UnwrapResult<syn::ExprPath>;
    fn return_(&self) -> UnwrapResult<syn::ExprReturn>;
    fn return_mut(&mut self) -> UnwrapResultMut<syn::ExprReturn>;
    fn call(&self) -> UnwrapResult<syn::ExprCall>;
    fn call_mut(&mut self) -> UnwrapResultMut<syn::ExprCall>;
    fn method_call(&self) -> UnwrapResult<syn::ExprMethodCall>;
    fn method_call_mut(&mut self) -> UnwrapResultMut<syn::ExprMethodCall>;
}
impl UnwrapExpr for syn::Expr {
    fn binary(&self) -> UnwrapResult<syn::ExprBinary> {
        match self {
            Self::Binary(b) => Ok(b),
            _ => Err("called `Expr::binary()` on a non `Binary` value"),
        }
    }
    fn binary_mut(&mut self) -> UnwrapResultMut<syn::ExprBinary> {
        match self {
            Self::Binary(b) => Ok(b),
            _ => Err("called `Expr::binary_mut()` on a non `Binary` value"),
        }
    }
    fn block(&self) -> UnwrapResult<syn::ExprBlock> {
        match self {
            Self::Block(b) => Ok(b),
            _ => Err("called `Expr::block()` on a non `Block` value"),
        }
    }
    fn block_mut(&mut self) -> UnwrapResultMut<syn::ExprBlock> {
        match self {
            Self::Block(b) => Ok(b),
            _ => Err("called `Expr::block_mut()` on a non `Block` value"),
        }
    }
    fn path(&self) -> UnwrapResult<syn::ExprPath> {
        match self {
            Self::Path(b) => Ok(b),
            _ => Err("called `Expr::path()` on a non `Path` value"),
        }
    }
    fn return_(&self) -> UnwrapResult<syn::ExprReturn> {
        match self {
            Self::Return(b) => Ok(b),
            _ => Err("called `Expr::return_()` on a non `Return` value"),
        }
    }
    fn return_mut(&mut self) -> UnwrapResultMut<syn::ExprReturn> {
        match self {
            Self::Return(b) => Ok(b),
            _ => Err("called `Expr::return_mut()` on a non `Return` value"),
        }
    }
    fn call(&self) -> UnwrapResult<syn::ExprCall> {
        match self {
            Self::Call(b) => Ok(b),
            _ => Err("called `Expr::call()` on a non `Call` value"),
        }
    }
    fn call_mut(&mut self) -> UnwrapResultMut<syn::ExprCall> {
        match self {
            Self::Call(b) => Ok(b),
            _ => Err("called `Expr::call_mut()` on a non `Call` value"),
        }
    }
    fn method_call(&self) -> UnwrapResult<syn::ExprMethodCall> {
        match self {
            Self::MethodCall(b) => Ok(b),
            _ => Err("called `Expr::method_call()` on a non `MethodCall` value"),
        }
    }
    fn method_call_mut(&mut self) -> UnwrapResultMut<syn::ExprMethodCall> {
        match self {
            Self::MethodCall(b) => Ok(b),
            _ => Err("called `Expr::method_call_mut()` on a non `MethodCall` value"),
        }
    }
}
pub trait UnwrapMember {
    fn named(&self) -> UnwrapResult<syn::Ident>;
}
impl UnwrapMember for syn::Member {
    fn named(&self) -> UnwrapResult<syn::Ident> {
        match self {
            Self::Named(i) => Ok(i),
            Self::Unnamed(_) => Err("called `Member::named()` on a non `Named` value"),
        }
    }
}
pub trait UnwrapFnArg {
    fn typed(&self) -> UnwrapResult<syn::PatType>;
}
impl UnwrapFnArg for syn::FnArg {
    fn typed(&self) -> UnwrapResult<syn::PatType> {
        match self {
            Self::Typed(i) => Ok(i),
            Self::Receiver(_) => Err("called `PatType::typed()` on a non `Typed` value"),
        }
    }
}
