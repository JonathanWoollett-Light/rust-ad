pub trait UnwrapLit {
    fn float(&self) -> &syn::LitFloat;
}
impl UnwrapLit for syn::Lit {
    fn float(&self) -> &syn::LitFloat {
        match self {
            Self::Float(float) => float,
            _ => panic!("called `Lit::float()` on a non `Float` value"),
        }
    }
}
pub trait UnwrapTokenTree {
    fn ident(&self) -> &proc_macro::Ident;
    fn literal(&self) -> &proc_macro::Literal;
}
impl UnwrapTokenTree for proc_macro::TokenTree {
    fn ident(&self) -> &proc_macro::Ident {
        match self {
            Self::Ident(local) => local,
            _ => panic!("called `TokenTree::ident()` on a non `Ident` value"),
        }
    }
    fn literal(&self) -> &proc_macro::Literal {
        match self {
            Self::Literal(lit) => lit,
            _ => panic!("called `TokenTree::literal()` on a non `Literal` value"),
        }
    }
}

pub trait UnwrapStmt {
    fn local(&self) -> &syn::Local;
    fn local_mut(&mut self) -> &mut syn::Local;
    fn semi(&self) -> &syn::Expr;
    fn semi_mut(&mut self) -> &mut syn::Expr;
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
            _ => panic!("called `Stmt::local_mut()` on a non `Local` value"),
        }
    }
    fn semi(&self) -> &syn::Expr {
        match self {
            Self::Semi(expr, _) => expr,
            _ => panic!("called `Stmt::semi()` on a non `Semi` value"),
        }
    }
    fn semi_mut(&mut self) -> &mut syn::Expr {
        match self {
            Self::Semi(expr, _) => expr,
            _ => panic!("called `Stmt::semi_mut()` on a non `Semi` value"),
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
    fn is_path(&self) -> bool;
    fn is_return(&self) -> bool;
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
}
pub trait UnwrapExpr {
    fn binary(&self) -> &syn::ExprBinary;
    fn binary_mut(&mut self) -> &mut syn::ExprBinary;
    fn block(&self) -> &syn::ExprBlock;
    fn block_mut(&mut self) -> &mut syn::ExprBlock;
    fn path(&self) -> &syn::ExprPath;
    fn return_(&self) -> &syn::ExprReturn;
    fn return_mut(&mut self) -> &mut syn::ExprReturn;
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
            _ => panic!("called `Expr::binary_mut()` on a non `Binary` value"),
        }
    }
    fn block(&self) -> &syn::ExprBlock {
        match self {
            Self::Block(b) => b,
            _ => panic!("called `Expr::block()` on a non `Block` value"),
        }
    }
    fn block_mut(&mut self) -> &mut syn::ExprBlock {
        match self {
            Self::Block(b) => b,
            _ => panic!("called `Expr::block_mut()` on a non `Block` value"),
        }
    }
    fn path(&self) -> &syn::ExprPath {
        match self {
            Self::Path(b) => b,
            _ => panic!("called `Expr::path()` on a non `Path` value"),
        }
    }
    fn return_(&self) -> &syn::ExprReturn {
        match self {
            Self::Return(b) => b,
            _ => panic!("called `Expr::return_()` on a non `Return` value"),
        }
    }
    fn return_mut(&mut self) -> &mut syn::ExprReturn {
        match self {
            Self::Return(b) => b,
            _ => panic!("called `Expr::return_mut()` on a non `Return` value"),
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
