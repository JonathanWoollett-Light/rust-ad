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
pub trait IsStmt {
    fn is_local(&self) -> bool;
}
impl IsStmt for syn::Stmt {
    fn is_local(&self) -> bool {
        matches!(self, Self::Local(_))
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
    fn block(&self) -> &syn::ExprBlock;
    fn block_mut(&mut self) -> &mut syn::ExprBlock;
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

// fn join_assign(this: &mut proc_macro::Span, other: proc_macro::Span) {
//     *this = this.join(other).unwrap();
// }

// trait Span {
//     // Returns span formed from `join` of all items in `self`.
//     fn span(&self) -> proc_macro::Span;
// }
// impl Span for syn::Stmt {
//     fn span(&self) -> proc_macro::Span {
//         match self {
//             Self::Local(l) => l.span(),
//             Self::Item(i) => i.span(),
//             Self::Expr(e) => e.span(),
//             Self::Semi(s) => s.span()
//         }
//     }
// }
// impl Span for syn::Local {
//     fn span(&self) -> proc_macro::Span {
//         let mut base = &mut self.attrs[0].span();
//         for i in 1..self.attrs.len() {
//             join_assign(base,self.attrs[i].span());
//         }
//         join_assign(base,self.let_token.span());
//         join_assign(base,self.pat.span());
//         join_assign(base,self.init.span());
//         join_assign(base,self.semi_token.span());
//     }
// }
// impl Span for syn::Attribute {
//     fn span(&self) -> proc_macro::Span {
//         let mut base = &mut self.attrs[0].span();
//         for i in 1..self.attrs.len() {
//             join_assign(base,self.attrs[i].span());
//         }
//         join_assign(base,self.let_token.span());
//         join_assign(base,self.pat.span());
//         join_assign(base,self.init.span());
//         join_assign(base,self.semi_token.span());
//     }
// }
