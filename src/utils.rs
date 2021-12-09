pub trait UnwrapStmt {
    fn local(&self) -> &syn::Local;
    fn local_mut(&mut self) -> &mut syn::Local;
}
impl UnwrapStmt for syn::Stmt {
    fn local(&self) -> &syn::Local {
        match self {
            Self::Local(local) => local,
            _ => panic!("called `Stmt::local()` on a non `Local` value"),
        }
    }
    fn local_mut(&mut self) -> &mut syn::Local {
        match self {
            Self::Local(local) => local,
            _ => panic!("called `Stmt::local()` on a non `Local` value"),
        }
    }
}
pub trait UnwrapPat {
    fn ident_mut(&mut self) -> &mut syn::PatIdent;
    fn ident(&self) -> &syn::PatIdent;
    fn tuple_mut(&mut self) -> &mut syn::PatTuple;
    fn tuple(&self) -> &syn::PatTuple;
}
impl UnwrapPat for syn::Pat {
    fn ident_mut(&mut self) -> &mut syn::PatIdent {
        match self {
            Self::Ident(ident) => ident,
            _ => panic!("called `Pat::ident()` on a non `Ident` value"),
        }
    }
    fn ident(&self) -> &syn::PatIdent {
        match self {
            Self::Ident(ident) => ident,
            _ => panic!("called `Pat::ident()` on a non `Ident` value"),
        }
    }
    fn tuple_mut(&mut self) -> &mut syn::PatTuple {
        match self {
            Self::Tuple(tuple) => tuple,
            _ => panic!("called `Pat::tuple_mut()` on a non `Tuple` value"),
        }
    }
    fn tuple(&self) -> &syn::PatTuple {
        match self {
            Self::Tuple(tuple) => tuple,
            _ => panic!("called `Pat::tuple()` on a non `Tuple` value"),
        }
    }
}
pub trait IsExpr {
    fn is_binary(&self) -> bool;
}
impl IsExpr for syn::Expr {
    fn is_binary(&self) -> bool {
        matches!(self, Self::Binary(_))
    }
}
pub trait UnwrapExpr {
    fn binary(&self) -> &syn::ExprBinary;
    fn binary_mut(&mut self) -> &mut syn::ExprBinary;
}
impl UnwrapExpr for syn::Expr {
    fn binary(&self) -> &syn::ExprBinary {
        match self {
            Self::Binary(b) => b,
            _ => panic!("called `Expr::binary()` on a non `Binary` value"),
        }
    }
    fn binary_mut(&mut self) -> &mut syn::ExprBinary {
        match self {
            Self::Binary(b) => b,
            _ => panic!("called `Expr::binary()` on a non `Binary` value"),
        }
    }
}
pub trait UnwrapMember {
    fn named(&self) -> &syn::Ident;
}
impl UnwrapMember for syn::Member {
    fn named(&self) -> &syn::Ident {
        match self {
            Self::Named(i) => i,
            Self::Unnamed(_) => panic!("called `Member::named()` on a non `Named` value"),
        }
    }
}
pub trait UnwrapFnArg {
    fn typed(&self) -> &syn::PatType;
}
impl UnwrapFnArg for syn::FnArg {
    fn typed(&self) -> &syn::PatType {
        match self {
            Self::Typed(i) => i,
            Self::Receiver(_) => panic!("called `PatType::typed()` on a non `Typed` value"),
        }
    }
}
