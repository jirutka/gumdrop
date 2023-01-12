use proc_macro2::Span;
use syn::parse::Error;
use syn::spanned::Spanned;
use syn::{parse_str, AttrStyle, Attribute, Ident, Lit, Meta, NestedMeta, Path, PathArguments};

#[cfg(feature = "default_expr")]
use syn::Expr;

#[derive(Default)]
pub struct AttrOpts {
    pub long: Option<String>,
    pub short: Option<char>,
    pub multi: Option<Ident>,
    pub free: bool,
    pub count: bool,
    pub help_flag: bool,
    pub no_help_flag: bool,
    pub no_short: bool,
    pub no_long: bool,
    pub no_multi: bool,
    pub required: bool,
    pub not_required: bool,
    pub doc: Option<String>,
    pub help: Option<String>,
    pub meta: Option<String>,
    pub parse: Option<ParseFn>,
    pub default: Option<String>,
    #[cfg(feature = "default_expr")]
    pub default_expr: Option<Expr>,

    pub command: bool,
}

impl AttrOpts {
    pub fn set_defaults(&mut self, defaults: &DefaultOpts) {
        if !self.help_flag && defaults.no_help_flag {
            self.no_help_flag = true;
        }
        if self.short.is_none() && defaults.no_short {
            self.no_short = true;
        }
        if self.long.is_none() && defaults.no_long {
            self.no_long = true;
        }
        if self.multi.is_none() && defaults.no_multi {
            self.no_multi = true;
        }

        if self.not_required {
            self.required = false;
        } else if defaults.required {
            self.required = true;
        }
    }

    pub fn parse(span: Span, attrs: &[Attribute]) -> Result<AttrOpts, Error> {
        let mut opts = AttrOpts::default();

        for attr in attrs {
            if is_outer(attr.style) {
                if path_eq(&attr.path, "doc") {
                    let meta = attr.parse_meta()?;

                    if let Meta::NameValue(nv) = meta {
                        let doc = lit_str(&nv.lit)?;

                        if opts.doc.is_none() {
                            opts.doc = Some(doc.trim_start().to_owned());
                        }
                    }
                } else if path_eq(&attr.path, "options") {
                    let meta = attr.parse_meta()?;

                    match meta {
                        Meta::Path(path) => {
                            return Err(Error::new(
                                path.span(),
                                "`#[options]` is not a valid attribute",
                            ))
                        }
                        Meta::NameValue(nv) => {
                            return Err(Error::new(
                                nv.path.span(),
                                "`#[options = ...]` is not a valid attribute",
                            ))
                        }
                        Meta::List(items) => {
                            for item in &items.nested {
                                opts.parse_item(item)?;
                            }
                        }
                    }
                }
            }
        }

        opts.check(span)?;

        Ok(opts)
    }

    fn parse_item(&mut self, item: &NestedMeta) -> Result<(), Error> {
        match item {
            NestedMeta::Lit(lit) => return Err(unexpected_meta_item(lit.span())),
            NestedMeta::Meta(item) => match item {
                Meta::Path(path) => match path.get_ident() {
                    Some(ident) => match ident.to_string().as_str() {
                        "free" => self.free = true,
                        "command" => self.command = true,
                        "count" => self.count = true,
                        "help_flag" => self.help_flag = true,
                        "no_help_flag" => self.no_help_flag = true,
                        "no_short" => self.no_short = true,
                        "no_long" => self.no_long = true,
                        "no_multi" => self.no_multi = true,
                        "required" => self.required = true,
                        "not_required" => self.not_required = true,
                        _ => return Err(unexpected_meta_item(path.span())),
                    },
                    None => return Err(unexpected_meta_item(path.span())),
                },
                Meta::List(list) => match list.path.get_ident() {
                    Some(ident) if *ident == "parse" => {
                        if list.nested.len() != 1 {
                            return Err(unexpected_meta_item(list.path.span()));
                        }

                        self.parse = Some(ParseFn::parse(&list.nested[0])?);
                    }
                    _ => return Err(unexpected_meta_item(list.path.span())),
                },
                Meta::NameValue(nv) => match nv.path.get_ident() {
                    Some(ident) => match ident.to_string().as_str() {
                        "default" => self.default = Some(lit_str(&nv.lit)?),
                        #[cfg(feature = "default_expr")]
                        "default_expr" => {
                            let expr = parse_str(&lit_str(&nv.lit)?)?;
                            self.default_expr = Some(expr);
                        }
                        #[cfg(not(feature = "default_expr"))]
                        "default_expr" => {
                            return Err(Error::new(
                                nv.path.span(),
                                "compile gumdrop with the `default_expr` \
                                    feature to enable this attribute",
                            ));
                        }
                        "long" => self.long = Some(lit_str(&nv.lit)?),
                        "short" => self.short = Some(lit_char(&nv.lit)?),
                        "help" => self.help = Some(lit_str(&nv.lit)?),
                        "meta" => self.meta = Some(lit_str(&nv.lit)?),
                        "multi" => {
                            let name = parse_str(&lit_str(&nv.lit)?)?;
                            self.multi = Some(name);
                        }
                        _ => return Err(unexpected_meta_item(nv.path.span())),
                    },
                    None => return Err(unexpected_meta_item(nv.path.span())),
                },
            },
        }

        Ok(())
    }

    fn check(&self, span: Span) -> Result<(), Error> {
        macro_rules! err {
            ( $($tt:tt)* ) => { {
                return Err(Error::new(span, $($tt)*));
            } }
        }

        if self.command {
            for (name, is_used) in [
                ("free", self.free),
                ("default", self.default.is_some()),
                ("multi", self.multi.is_some()),
                ("long", self.long.is_some()),
                ("short", self.short.is_some()),
                ("count", self.count),
                ("help_flag", self.help_flag),
                ("no_help_flag", self.no_help_flag),
                ("no_short", self.no_short),
                ("no_long", self.no_long),
                ("no_multi", self.no_multi),
                ("help", self.help.is_some()),
                ("meta", self.meta.is_some()),
            ] {
                if is_used {
                    err!(format!("`command` and `{}` are mutually exclusive", name));
                }
            }
        }

        if self.free {
            for (name, is_used) in [
                ("default", self.default.is_some()),
                ("long", self.long.is_some()),
                ("short", self.short.is_some()),
                ("count", self.count),
                ("help_flag", self.help_flag),
                ("no_help_flag", self.no_help_flag),
                ("no_short", self.no_short),
                ("no_long", self.no_long),
                ("meta", self.meta.is_some()),
            ] {
                if is_used {
                    err!(format!("`free` and `{}` are mutually exclusive", name));
                }
            }
        }

        if self.multi.is_some() && self.no_multi {
            err!("`multi` and `no_multi` are mutually exclusive");
        }

        if self.help_flag && self.no_help_flag {
            err!("`help_flag` and `no_help_flag` are mutually exclusive");
        }

        if self.no_short && self.short.is_some() {
            err!("`no_short` and `short` are mutually exclusive");
        }

        if self.no_long && self.long.is_some() {
            err!("`no_long` and `long` are mutually exclusive");
        }

        if self.required && self.not_required {
            err!("`required` and `not_required` are mutually exclusive");
        }

        if self.parse.is_some() && self.count {
            err!("`count` and `parse` are mutually exclusive");
        }

        #[cfg(feature = "default_expr")]
        {
            if self.default.is_some() && self.default_expr.is_some() {
                err!("`default` and `default_expr` are mutually exclusive");
            }
        }

        Ok(())
    }
}

#[derive(Default)]
pub struct CmdOpts {
    pub name: Option<String>,
    pub doc: Option<String>,
    pub help: Option<String>,
}

impl CmdOpts {
    pub fn parse(attrs: &[Attribute]) -> Result<CmdOpts, Error> {
        let mut opts = CmdOpts::default();

        for attr in attrs {
            if is_outer(attr.style) {
                if path_eq(&attr.path, "doc") {
                    let meta = attr.parse_meta()?;

                    if let Meta::NameValue(nv) = meta {
                        let doc = lit_str(&nv.lit)?;

                        if opts.doc.is_none() {
                            opts.doc = Some(doc.trim_start().to_owned());
                        }
                    }
                } else if path_eq(&attr.path, "options") {
                    let meta = attr.parse_meta()?;

                    match meta {
                        Meta::Path(path) => {
                            return Err(Error::new(
                                path.span(),
                                "`#[options]` is not a valid attribute",
                            ))
                        }
                        Meta::NameValue(nv) => {
                            return Err(Error::new(
                                nv.path.span(),
                                "`#[options = ...]` is not a valid attribute",
                            ))
                        }
                        Meta::List(items) => {
                            for item in &items.nested {
                                opts.parse_item(item)?;
                            }
                        }
                    }
                }
            }
        }

        Ok(opts)
    }

    fn parse_item(&mut self, item: &NestedMeta) -> Result<(), Error> {
        match item {
            NestedMeta::Lit(lit) => return Err(unexpected_meta_item(lit.span())),
            NestedMeta::Meta(item) => match item {
                Meta::Path(path) => return Err(unexpected_meta_item(path.span())),
                Meta::List(list) => return Err(unexpected_meta_item(list.path.span())),
                Meta::NameValue(nv) => match nv.path.get_ident() {
                    Some(ident) => match ident.to_string().as_str() {
                        "name" => self.name = Some(lit_str(&nv.lit)?),
                        "help" => self.help = Some(lit_str(&nv.lit)?),
                        _ => return Err(unexpected_meta_item(nv.path.span())),
                    },
                    None => return Err(unexpected_meta_item(nv.path.span())),
                },
            },
        }

        Ok(())
    }
}

#[derive(Default)]
pub struct DefaultOpts {
    pub no_help_flag: bool,
    pub no_long: bool,
    pub no_multi: bool,
    pub no_short: bool,
    pub required: bool,
    pub doc: Option<String>,
    pub help: Option<String>,
}

impl DefaultOpts {
    pub fn parse(attrs: &[Attribute]) -> Result<DefaultOpts, Error> {
        let mut opts = DefaultOpts::default();

        for attr in attrs {
            if is_outer(attr.style) {
                if path_eq(&attr.path, "doc") {
                    let meta = attr.parse_meta()?;

                    if let Meta::NameValue(nv) = meta {
                        let doc = lit_str(&nv.lit)?;

                        if let Some(text) = opts.doc.as_mut() {
                            text.push('\n');
                            text.push_str(doc.trim_start());
                        } else {
                            opts.doc = Some(doc.trim_start().to_owned());
                        }
                    }
                } else if path_eq(&attr.path, "options") {
                    let meta = attr.parse_meta()?;

                    match meta {
                        Meta::Path(path) => {
                            return Err(Error::new(
                                path.span(),
                                "`#[options]` is not a valid attribute",
                            ))
                        }
                        Meta::NameValue(nv) => {
                            return Err(Error::new(
                                nv.path.span(),
                                "`#[options = ...]` is not a valid attribute",
                            ))
                        }
                        Meta::List(items) => {
                            for item in &items.nested {
                                opts.parse_item(item)?;
                            }
                        }
                    }
                }
            }
        }

        Ok(opts)
    }

    fn parse_item(&mut self, item: &NestedMeta) -> Result<(), Error> {
        match item {
            NestedMeta::Lit(lit) => return Err(unexpected_meta_item(lit.span())),
            NestedMeta::Meta(item) => match item {
                Meta::Path(path) => match path.get_ident() {
                    Some(ident) => match ident.to_string().as_str() {
                        "no_help_flag" => self.no_help_flag = true,
                        "no_short" => self.no_short = true,
                        "no_long" => self.no_long = true,
                        "no_multi" => self.no_multi = true,
                        "required" => self.required = true,
                        _ => return Err(unexpected_meta_item(ident.span())),
                    },
                    None => return Err(unexpected_meta_item(path.span())),
                },
                Meta::NameValue(nv) => match nv.path.get_ident() {
                    Some(ident) if *ident == "help" => self.help = Some(lit_str(&nv.lit)?),
                    _ => return Err(unexpected_meta_item(nv.path.span())),
                },
                Meta::List(list) => return Err(unexpected_meta_item(list.path.span())),
            },
        }

        Ok(())
    }
}

#[derive(Clone, Default)]
pub enum ParseFn {
    #[default]
    Default,
    FromStr(Option<Path>),
    TryFromStr(Path),
    FromOsStr(Option<Path>),
    TryFromOsStr(Path),
}

impl ParseFn {
    pub fn parse(item: &NestedMeta) -> Result<ParseFn, Error> {
        let result = match item {
            NestedMeta::Meta(Meta::Path(path)) => match path.get_ident() {
                Some(ident) => match ident.to_string().as_str() {
                    "from_str" => ParseFn::FromStr(None),
                    "try_from_str" => ParseFn::Default,
                    "from_os_str" => ParseFn::FromOsStr(None),
                    _ => return Err(unexpected_meta_item(ident.span())),
                },
                None => return Err(unexpected_meta_item(path.span())),
            },
            NestedMeta::Meta(Meta::NameValue(nv)) => match nv.path.get_ident() {
                Some(ident) => match ident.to_string().as_str() {
                    "from_str" => {
                        let path = parse_str(&lit_str(&nv.lit)?)?;
                        ParseFn::FromStr(Some(path))
                    }
                    "try_from_str" => {
                        let path = parse_str(&lit_str(&nv.lit)?)?;
                        ParseFn::TryFromStr(path)
                    }
                    "from_os_str" => {
                        let path = parse_str(&lit_str(&nv.lit)?)?;
                        ParseFn::FromOsStr(Some(path))
                    }
                    "try_from_os_str" => {
                        let path = parse_str(&lit_str(&nv.lit)?)?;
                        ParseFn::TryFromOsStr(path)
                    }
                    _ => return Err(unexpected_meta_item(nv.path.span())),
                },
                None => return Err(unexpected_meta_item(nv.path.span())),
            },
            NestedMeta::Lit(_) | NestedMeta::Meta(Meta::List(_)) => {
                return Err(unexpected_meta_item(item.span()))
            }
        };

        Ok(result)
    }
}

fn is_outer(style: AttrStyle) -> bool {
    matches!(style, AttrStyle::Outer)
}

fn lit_str(lit: &Lit) -> Result<String, Error> {
    match lit {
        Lit::Str(s) => Ok(s.value()),
        _ => Err(Error::new(lit.span(), "expected string literal")),
    }
}

fn lit_char(lit: &Lit) -> Result<char, Error> {
    match lit {
        Lit::Char(ch) => Ok(ch.value()),
        // Character literals in attributes are not necessarily allowed
        Lit::Str(s) => {
            let s = s.value();
            let mut chars = s.chars();

            let first = chars.next();
            let second = chars.next();

            match (first, second) {
                (Some(ch), None) => Ok(ch),
                _ => Err(Error::new(
                    lit.span(),
                    "expected one-character string literal",
                )),
            }
        }
        _ => Err(Error::new(lit.span(), "expected character literal")),
    }
}

fn path_eq(path: &Path, s: &str) -> bool {
    path.segments.len() == 1 && {
        let seg = path.segments.first().unwrap();

        match seg.arguments {
            PathArguments::None => seg.ident == s,
            _ => false,
        }
    }
}

fn unexpected_meta_item(span: Span) -> Error {
    Error::new(span, "unexpected meta item")
}
