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
}
pub trait IsExpr {
    fn is_binary(&self) -> bool;
}
impl IsExpr for syn::Expr {
    fn is_binary(&self) -> bool {
        match self {
            Self::Binary(_) => true,
            _ => false,
        }
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
