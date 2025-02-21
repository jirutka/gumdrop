//! Provides `derive(Options)` for `gumdrop` crate
//!
//! # `derive(Options)`
//!
//! `derive(Options)` generates an implementation of the trait `Options`,
//! creating an option for each field of the decorated `struct`.
//!
//! See the `gumdrop` [documentation](https://docs.rs/gumdrop/) for an example
//! of its usage.
//!
//! ## `options` attribute
//!
//! Behavior of `derive(Options)` can be controlled by adding `#[options(...)]`
//! attributes to one or more fields within a decorated struct.
//!
//! Supported items for `struct` fields are:
//!
//! * `command` indicates that a field represents a subcommand. The field must
//!   be of type `Option<T>` where `T` is a type implementing `Options`.
//!   Typically, this type is an `enum` containing subcommand option types.
//! * `help_flag` marks an option as a help flag. The field must be `bool` type.
//!   Options named `help` will automatically receive this option.
//! * `no_help_flag` prevents an option from being considered a help flag.
//! * `count` marks a field as a counter value. The field will be incremented
//!   each time the option appears in the arguments, i.e. `field += 1;`
//! * `free` marks a field as a positional argument field. Non-option arguments
//!   will be used to fill all `free` fields, in declared sequence.
//!   If the final `free` field is of type `Vec<T>`, it will contain all
//!   remaining free arguments.
//! * `short = "?"` sets the short option name to the given character
//! * `no_short` prevents a short option from being assigned to the field
//! * `long = "..."` sets the long option name to the given string
//! * `no_long` prevents a long option from being assigned to the field
//! * `default` provides a default value for the option field.
//!   The value of this field is parsed in the same way as argument values.
//! * `default_expr` provides a default value for the option field.
//!   The value of this field is parsed at compile time as a Rust expression
//!   and is evaluated before any argument values are processed.
//!   The `default_expr` feature must be enabled to use this attribute.
//! * `required` will cause an error if the option is not present,
//!   unless at least one `help_flag` option is also present.
//! * `multi = "..."` will allow parsing an option multiple times,
//!   adding each parsed value to the field using the named method.
//!   This behavior is automatically applied to `Vec<T>` fields, unless the
//!   `no_multi` option is present.
//! * `no_multi` will inhibit automatically marking `Vec<T>` fields as `multi`
//! * `not_required` will cancel a type-level `required` flag (see below).
//! * `help = "..."` sets help text returned from the `Options::usage` method;
//!   field doc comment may also be provided to set the help text.
//!   If both are present, the `help` attribute value is used.
//! * `meta = "..."` sets the meta variable displayed in usage for options
//!   which accept an argument
//! * `parse(...)` uses a named function to parse a value from a string.
//!   Valid parsing function types are:
//!     * `parse(from_str = "...")` for `fn(&str) -> T`
//!     * `parse(try_from_str = "...")` for
//!       `fn(&str) -> Result<T, E> where E: Display`
//!     * `parse(from_str)` uses `std::convert::From::<&str>::from`
//!     * `parse(try_from_str)` uses `std::str::FromStr::from_str` (this is the
//!       default when no parsing function is specified)
//!     * `parse(from_os_str = "...")` for `fn(&OsStr) -> T`
//!     * `parse(try_from_os_str = "...")` for
//!       `fn(&OsStr) -> Result<T, E> where E: Display`
//!     * `parse(from_os_str)` uses `std::convert::From::<&OsStr>::from`
//!
//! Additionally, the following flags may be set at the type level to establish
//! default values for all contained fields: `no_help_flag`, `no_long`,
//! `no_short`, and `required`.
//!
//! Supported items for `enum` variants are:
//!
//! * `name = "..."` sets the user-facing command name.
//!   If this option is not present, one is automatically generated from the variant name.
//! * `help = "..."` sets the help string for the command;
//!   variant doc comment may also be provided to set the help text.
//!   If both are present, the `help` attribute value is used.
//!
//! The `help` attribute (or a type-level doc comment) can be used to provide
//! some introductory text which will precede option help text in the usage
//! string.

#![recursion_limit = "1024"]

mod help;
mod parse_attrs;

use std::iter::repeat;

use proc_macro::TokenStream;
use proc_macro2::{Span, TokenStream as TokenStream2};
use quote::quote;
use syn::parse::Error;
use syn::{
    Data, DataEnum, DataStruct, DeriveInput, Fields, GenericArgument, Ident, PathArguments, Type,
};

use crate::parse_attrs::*;
use crate::help::*;

/// Derives the `gumdrop::Options` trait for `struct` and `enum` items.
///
/// `#[options(...)]` attributes can be used to control behavior of generated trait
/// implementation. See [crate-level documentation](index.html) for the full list of
/// supported options.
#[proc_macro_derive(Options, attributes(options))]
pub fn derive_options(input: TokenStream) -> TokenStream {
    let ast: DeriveInput = match syn::parse(input) {
        Ok(ast) => ast,
        Err(e) => {
            return e.to_compile_error().into();
        }
    };

    let span = ast.ident.span();

    let result = match &ast.data {
        Data::Enum(data) => derive_options_enum(&ast, data),
        Data::Struct(DataStruct {
            fields: Fields::Unit,
            ..
        }) => Err(Error::new(
            span,
            "cannot derive Options for unit struct types",
        )),
        Data::Struct(DataStruct {
            fields: Fields::Unnamed(..),
            ..
        }) => Err(Error::new(
            span,
            "cannot derive Options for tuple struct types",
        )),
        Data::Struct(DataStruct { fields, .. }) => derive_options_struct(&ast, fields),
        Data::Union(_) => Err(Error::new(span, "cannot derive Options for union types")),
    };

    match result {
        Ok(tokens) => tokens.into(),
        Err(e) => e.to_compile_error().into(),
    }
}

fn derive_options_enum(ast: &DeriveInput, data: &DataEnum) -> Result<TokenStream2, Error> {
    let name = &ast.ident;
    let mut commands = Vec::new();
    let mut var_ty = Vec::new();

    for var in &data.variants {
        let span = var.ident.span();

        let ty = match &var.fields {
            Fields::Unit | Fields::Named(_) => {
                return Err(Error::new(
                    span,
                    "command variants must be unary tuple variants",
                ))
            }
            Fields::Unnamed(fields) if fields.unnamed.len() != 1 => {
                return Err(Error::new(
                    span,
                    "command variants must be unary tuple variants",
                ))
            }
            Fields::Unnamed(fields) => &fields.unnamed.first().unwrap().ty,
        };

        let opts = CmdOpts::parse(&var.attrs)?;

        let var_name = &var.ident;

        var_ty.push(ty);

        commands.push(Cmd {
            name: opts
                .name
                .unwrap_or_else(|| make_command_name(&var_name.to_string())),
            help: opts.help.or(opts.doc),
            variant_name: var_name,
            ty,
        });
    }

    let mut command = Vec::new();
    let mut handle_cmd = Vec::new();
    let mut help_req_impl = Vec::new();
    let mut variant = Vec::new();
    let usage = make_cmd_usage(&commands);

    for cmd in commands {
        command.push(cmd.name);

        let var_name = cmd.variant_name;
        let ty = &cmd.ty;

        variant.push(var_name);

        handle_cmd.push(quote! {
            #name::#var_name(<#ty as ::gumdrop::Options>::parse(_parser)?)
        });

        help_req_impl.push(quote! {
            #name::#var_name(cmd) => { ::gumdrop::Options::help_requested(cmd) }
        });
    }

    // Borrow re-used items
    let command = &command;

    let (impl_generics, ty_generics, where_clause) = ast.generics.split_for_impl();

    let command_impl = {
        let name = repeat(name);

        quote! {
            match self {
                #( #name::#variant(cmd) => ::gumdrop::Options::command(cmd), )*
            }
        }
    };

    let command_name_impl = {
        let name = repeat(name);

        quote! {
            match self {
                #( #name::#variant(_) => ::std::option::Option::Some(#command), )*
            }
        }
    };

    let self_usage_impl = {
        let name = repeat(name);

        quote! {
            match self {
                #( #name::#variant(sub) => ::gumdrop::Options::self_usage(sub), )*
            }
        }
    };

    let self_command_list_impl = {
        let name = repeat(name);

        quote! {
            match self {
                #( #name::#variant(sub) => ::gumdrop::Options::self_command_list(sub), )*
            }
        }
    };

    Ok(quote! {
        impl #impl_generics ::gumdrop::Options for #name #ty_generics #where_clause {
            fn parse<__S: ::std::convert::AsRef<::std::ffi::OsStr>>(
                    _parser: &mut ::gumdrop::Parser<__S>)
                    -> ::std::result::Result<Self, ::gumdrop::Error> {
                let _arg = _parser.next_arg()
                    .ok_or_else(::gumdrop::Error::missing_command)?;

                Self::parse_command(::gumdrop::to_str(_arg)?, _parser)
            }

            fn command(&self) -> ::std::option::Option<&dyn ::gumdrop::Options> {
                #command_impl
            }

            fn command_name(&self) -> ::std::option::Option<&'static str> {
                #command_name_impl
            }

            fn help_requested(&self) -> bool {
                match self {
                    #( #help_req_impl )*
                }
            }

            fn parse_command<__S: ::std::convert::AsRef<::std::ffi::OsStr>>(name: &str,
                    _parser: &mut ::gumdrop::Parser<__S>)
                    -> ::std::result::Result<Self, ::gumdrop::Error> {
                let cmd = match name {
                    #( #command => { #handle_cmd } )*
                    _ => return ::std::result::Result::Err(
                        ::gumdrop::Error::unrecognized_command(name))
                };

                ::std::result::Result::Ok(cmd)
            }

            fn usage() -> &'static str {
                #usage
            }

            fn self_usage(&self) -> &'static str {
                #self_usage_impl
            }

            fn command_list() -> ::std::option::Option<&'static str> {
                ::std::option::Option::Some(<Self as ::gumdrop::Options>::usage())
            }

            fn self_command_list(&self) -> ::std::option::Option<&'static str> {
                #self_command_list_impl
            }

            fn command_usage(name: &str) -> ::std::option::Option<&'static str> {
                match name {
                    #( #command => ::std::option::Option::Some(
                        <#var_ty as ::gumdrop::Options>::usage()), )*
                    _ => ::std::option::Option::None
                }
            }
        }
    })
}

fn derive_options_struct(ast: &DeriveInput, fields: &Fields) -> Result<TokenStream2, Error> {
    let mut pattern = Vec::new();
    let mut handle_opt = Vec::new();
    let mut short_names = Vec::new();
    let mut long_names = Vec::new();
    let mut free: Vec<FreeOpt> = Vec::new();
    let mut required = Vec::new();
    let mut required_err = Vec::new();
    let mut command = None;
    let mut command_ty = None;
    let mut command_required = false;
    let mut help_flag = Vec::new();
    let mut options = Vec::new();
    let mut field_name = Vec::new();
    let mut default = Vec::new();

    let default_expr = quote! { ::std::default::Default::default() };
    let default_opts = DefaultOpts::parse(&ast.attrs)?;

    for field in fields {
        let span = field.ident.as_ref().unwrap().span();

        let mut opts = AttrOpts::parse(span, &field.attrs)?;
        opts.set_defaults(&default_opts);

        let ident = field.ident.as_ref().unwrap();

        field_name.push(ident);

        if let Some(expr) = &opts.default {
            default.push(
                opts.parse
                    .as_ref()
                    .unwrap_or(&ParseFn::Default)
                    .make_parse_default_action(ident, expr),
            );
        } else {
            #[cfg(not(feature = "default_expr"))]
            default.push(default_expr.clone());

            #[cfg(feature = "default_expr")]
            {
                if let Some(expr) = &opts.default_expr {
                    default.push(quote! { #expr });
                } else {
                    default.push(default_expr.clone());
                }
            }
        }

        if opts.command {
            if command.is_some() {
                return Err(Error::new(span, "duplicate declaration of `command` field"));
            }
            if !free.is_empty() {
                return Err(Error::new(
                    span,
                    "`command` and `free` options are mutually exclusive",
                ));
            }

            command = Some(ident);
            command_ty = Some(first_ty_param(&field.ty).unwrap_or(&field.ty));
            command_required = opts.required;

            if opts.required {
                required.push(ident);
                required_err.push(quote! {
                ::gumdrop::Error::missing_required_command() });
            }

            continue;
        }

        if opts.free {
            if command.is_some() {
                return Err(Error::new(
                    span,
                    "`command` and `free` options are mutually exclusive",
                ));
            }

            if let Some(last) = free.last() {
                if last.action.is_push() {
                    return Err(Error::new(
                        span,
                        "only the final `free` option may be of type `Vec<T>`",
                    ));
                }
            }

            if opts.required {
                required.push(ident);
                required_err.push(quote! {
                ::gumdrop::Error::missing_required_free() });
            }

            free.push(FreeOpt {
                field: ident,
                action: FreeAction::infer(&field.ty, &opts),
                parse: opts.parse.unwrap_or_default(),
                required: opts.required,
                help: opts.help.or(opts.doc),
            });

            continue;
        }

        if opts.long.is_none() && !opts.no_long {
            opts.long = Some(make_long_name(&ident.to_string()));
        }

        if let Some(long) = &opts.long {
            validate_long_name(span, long, &long_names)?;
            long_names.push(long.clone());
        }

        if let Some(short) = opts.short {
            validate_short_name(span, short, &short_names)?;
            short_names.push(short);
        }

        if opts.help_flag
            || (!opts.no_help_flag && opts.long.as_ref().map(|s| &s[..]) == Some("help"))
        {
            help_flag.push(ident);
        }

        let action = if opts.count {
            Action::Count
        } else {
            Action::infer(&field.ty, &opts)
        };

        if action.takes_arg() {
            if opts.meta.is_none() {
                opts.meta = Some(make_meta(&ident.to_string(), &action));
            }
        } else if opts.meta.is_some() {
            return Err(Error::new(span, "`meta` value is invalid for this field"));
        }

        options.push(Opt {
            field: ident,
            action,
            long: opts.long,
            short: opts.short,
            no_short: opts.no_short,
            required: opts.required,
            meta: opts.meta,
            help: opts.help.or(opts.doc),
            default: opts.default,
        });
    }

    // Assign short names after checking all options.
    // Thus, manual short names will take priority over automatic ones.
    for opt in &mut options {
        if opt.short.is_none() && !opt.no_short {
            let short = make_short_name(&opt.field.to_string(), &short_names);

            if let Some(short) = short {
                short_names.push(short);
            }

            opt.short = short;
        }
    }

    for opt in &options {
        if opt.required {
            required.push(opt.field);
            let display = opt.display_form();
            required_err.push(quote! {
            ::gumdrop::Error::missing_required(#display) });
        }

        let pat = match (&opt.long, opt.short) {
            (Some(long), Some(short)) => quote! {
                ::gumdrop::Opt::Long(#long) | ::gumdrop::Opt::Short(#short)
            },
            (Some(long), None) => quote! {
                ::gumdrop::Opt::Long(#long)
            },
            (None, Some(short)) => quote! {
                ::gumdrop::Opt::Short(#short)
            },
            (None, None) => {
                return Err(Error::new(
                    opt.field.span(),
                    "option has no long or short flags",
                ));
            }
        };

        pattern.push(pat);
        handle_opt.push(opt.make_action());

        if let Some(long) = &opt.long {
            let (pat, handle) = if let Some(n) = opt.action.tuple_len() {
                (
                    quote! { ::gumdrop::Opt::LongWithArg(_long, _) if _long == #long },
                    quote! { return ::std::result::Result::Err(
                    ::gumdrop::Error::unexpected_single_argument(&_opt, #n)) },
                )
            } else if opt.action.takes_arg() {
                let mut handle = quote! { let _arg: &::std::ffi::OsStr = _arg; };
                handle.extend(opt.make_action_arg());
                (
                    quote! { ::gumdrop::Opt::LongWithArg(_long, _arg) if _long == #long },
                    handle,
                )
            } else {
                (
                    quote! { ::gumdrop::Opt::LongWithArg(_long, _) if _long == #long },
                    quote! { return ::std::result::Result::Err(
                    ::gumdrop::Error::unexpected_argument(&_opt)) },
                )
            };

            pattern.push(pat);
            handle_opt.push(handle);
        }
    }

    let name = &ast.ident;
    let opts_help = default_opts.help.or(default_opts.doc);
    let usage = make_usage(&opts_help, &free, &options);

    let handle_free = if !free.is_empty() {
        let catch_all = if free.last().unwrap().action.is_push() {
            let last = free.pop().unwrap();

            let free = last.field;
            let name = free.to_string();
            let meth = match &last.action {
                FreeAction::Push(meth) => meth,
                _ => unreachable!(),
            };

            let parse = last.parse.make_parse_action(Some(&name[..]));
            let mark_used = last.mark_used();

            quote! {
                #mark_used
                let _arg = _free;
                _result.#free.#meth(#parse);
            }
        } else {
            quote! {
                return ::std::result::Result::Err(
                    ::gumdrop::Error::unexpected_free(_free))
            }
        };

        let num = 0..free.len();
        let action = free
            .iter()
            .map(|free| {
                let field = free.field;
                let name = field.to_string();

                let mark_used = free.mark_used();
                let parse = free.parse.make_parse_action(Some(&name[..]));

                let assign = match &free.action {
                    FreeAction::Push(meth) => quote! {
                        let _arg = _free;
                        _result.#field.#meth(#parse);
                    },
                    FreeAction::SetField => quote! {
                        let _arg = _free;
                        _result.#field = #parse;
                    },
                    FreeAction::SetOption => quote! {
                        let _arg = _free;
                        _result.#field = ::std::option::Option::Some(#parse);
                    },
                };

                quote! {
                    #mark_used
                    #assign
                }
            })
            .collect::<Vec<_>>();

        quote! {
            match _free_counter {
                #( #num => {
                    _free_counter += 1;
                    #action
                } )*
                _ => { #catch_all }
            }
        }
    } else if let Some(ident) = command {
        let mark_used = if command_required {
            quote! { _used.#ident = true; }
        } else {
            quote! {}
        };

        quote! {
            #mark_used
            _result.#ident = ::std::option::Option::Some(
                ::gumdrop::Options::parse_command(::gumdrop::to_str(_free)?, _parser)?);
            break;
        }
    } else {
        quote! {
            return ::std::result::Result::Err(
                ::gumdrop::Error::unexpected_free(_free));
        }
    };

    let command_impl = match &command {
        None => quote! { ::std::option::Option::None },
        Some(field) => quote! {
            ::std::option::Option::map(
                ::std::option::Option::as_ref(&self.#field),
                |sub| sub as _)
        },
    };

    let command_name_impl = match &command {
        None => quote! { ::std::option::Option::None },
        Some(field) => quote! {
            ::std::option::Option::and_then(
                ::std::option::Option::as_ref(&self.#field),
                ::gumdrop::Options::command_name)
        },
    };

    let command_list = match command_ty {
        Some(ty) => quote! {
            ::std::option::Option::Some(
                <#ty as ::gumdrop::Options>::usage())
        },
        None => quote! {
            ::std::option::Option::None
        },
    };

    let command_usage = match command_ty {
        Some(ty) => quote! {
            <#ty as ::gumdrop::Options>::command_usage(_name)
        },
        None => quote! {
            ::std::option::Option::None
        },
    };

    let help_requested_impl = match (&help_flag, &command) {
        (flags, None) => quote! {
            fn help_requested(&self) -> bool {
                false #( || self.#flags )*
            }
        },
        (flags, Some(cmd)) => quote! {
            fn help_requested(&self) -> bool {
                #( self.#flags || )*
                ::std::option::Option::map_or(
                    ::std::option::Option::as_ref(&self.#cmd),
                    false, ::gumdrop::Options::help_requested)
            }
        },
    };

    let self_usage_impl = match &command {
        None => quote! { <Self as ::gumdrop::Options>::usage() },
        Some(field) => quote! {
            ::std::option::Option::map_or_else(
                ::std::option::Option::as_ref(&self.#field),
                <Self as ::gumdrop::Options>::usage,
                ::gumdrop::Options::self_usage)
        },
    };

    let self_command_list_impl = match &command {
        None => quote! { <Self as ::gumdrop::Options>::command_list() },
        Some(field) => quote! {
            ::std::option::Option::map_or_else(
                ::std::option::Option::as_ref(&self.#field),
                <Self as ::gumdrop::Options>::command_list,
                ::gumdrop::Options::self_command_list)
        },
    };

    let required = &required;

    let (impl_generics, ty_generics, where_clause) = ast.generics.split_for_impl();

    Ok(quote! {
        impl #impl_generics ::gumdrop::Options for #name #ty_generics #where_clause {
            fn parse<__S: ::std::convert::AsRef<::std::ffi::OsStr>>(
                    _parser: &mut ::gumdrop::Parser<__S>)
                    -> ::std::result::Result<Self, ::gumdrop::Error> {
                #[derive(Default)]
                struct _Used {
                    #( #required: bool , )*
                }

                let mut _result = #name{
                    #( #field_name: #default ),*
                };
                let mut _free_counter = 0usize;
                let mut _used = _Used::default();

                while let ::std::option::Option::Some(_opt) = _parser.next_opt() {
                    let _opt = _opt?;
                    match &_opt {
                        #( #pattern => { #handle_opt } )*
                        ::gumdrop::Opt::Free(_free) => {
                            #handle_free
                        }
                        _ => {
                            return ::std::result::Result::Err(
                                ::gumdrop::Error::unrecognized_option(&_opt));
                        }
                    }
                }

                if true #( && !_result.#help_flag )* {
                    #( if !_used.#required {
                        return ::std::result::Result::Err(#required_err);
                    } )*
                }

                ::std::result::Result::Ok(_result)
            }

            fn command(&self) -> ::std::option::Option<&dyn ::gumdrop::Options> {
                #command_impl
            }

            fn command_name(&self) -> ::std::option::Option<&'static str> {
                #command_name_impl
            }

            #help_requested_impl

            fn parse_command<__S: ::std::convert::AsRef<::std::ffi::OsStr>>(name: &str,
                    _parser: &mut ::gumdrop::Parser<__S>)
                    -> ::std::result::Result<Self, ::gumdrop::Error> {
                ::std::result::Result::Err(
                    ::gumdrop::Error::unrecognized_command(name))
            }

            fn usage() -> &'static str {
                #usage
            }

            fn self_usage(&self) -> &'static str {
                #self_usage_impl
            }

            fn command_list() -> ::std::option::Option<&'static str> {
                #command_list
            }

            fn command_usage(_name: &str) -> ::std::option::Option<&'static str> {
                #command_usage
            }

            fn self_command_list(&self) -> ::std::option::Option<&'static str> {
                #self_command_list_impl
            }
        }
    })
}

enum Action {
    /// Increase count
    Count,
    /// Push an argument to a `multi` field using the given method
    Push(Ident, ParseMethod),
    /// Set field
    SetField(ParseMethod),
    /// Set `Option<T>` field
    SetOption(ParseMethod),
    /// Set field to `true`
    Switch,
}

struct Cmd<'a> {
    name: String,
    help: Option<String>,
    variant_name: &'a Ident,
    ty: &'a Type,
}

enum FreeAction {
    Push(Ident),
    SetField,
    SetOption,
}

struct FreeOpt<'a> {
    field: &'a Ident,
    action: FreeAction,
    parse: ParseFn,
    required: bool,
    help: Option<String>,
}

struct Opt<'a> {
    field: &'a Ident,
    action: Action,
    long: Option<String>,
    short: Option<char>,
    no_short: bool,
    required: bool,
    help: Option<String>,
    meta: Option<String>,
    default: Option<String>,
    // NOTE: `default_expr` is not contained here
    // because it is not displayed to the user in usage text
}

struct ParseMethod {
    parse_fn: ParseFn,
    tuple_len: Option<usize>,
}

impl Action {
    fn infer(ty: &Type, opts: &AttrOpts) -> Action {
        match ty {
            Type::Path(path) => {
                let path = path.path.segments.last().unwrap();
                let param = first_ty_param(ty);

                match &path.ident.to_string()[..] {
                    "bool" if opts.parse.is_none() => Action::Switch,
                    "Vec" if !opts.no_multi && param.is_some() => {
                        let tuple_len = tuple_len(param.unwrap());

                        Action::Push(
                            Ident::new("push", Span::call_site()),
                            ParseMethod {
                                parse_fn: opts.parse.clone().unwrap_or_default(),
                                tuple_len,
                            },
                        )
                    }
                    "Option" if param.is_some() => {
                        let tuple_len = tuple_len(param.unwrap());

                        Action::SetOption(ParseMethod {
                            parse_fn: opts.parse.clone().unwrap_or_default(),
                            tuple_len,
                        })
                    }
                    _ => {
                        if let Some(meth) = &opts.multi {
                            let tuple_len = param.and_then(tuple_len);

                            Action::Push(
                                meth.clone(),
                                ParseMethod {
                                    parse_fn: opts.parse.clone().unwrap_or_default(),
                                    tuple_len,
                                },
                            )
                        } else {
                            Action::SetField(ParseMethod {
                                parse_fn: opts.parse.clone().unwrap_or_default(),
                                tuple_len: tuple_len(ty),
                            })
                        }
                    }
                }
            }
            _ => {
                let tuple_len = tuple_len(ty);

                Action::SetField(ParseMethod {
                    parse_fn: opts.parse.clone().unwrap_or_default(),
                    tuple_len,
                })
            }
        }
    }

    fn takes_arg(&self) -> bool {
        use self::Action::*;

        match self {
            Push(_, parse) | SetField(parse) | SetOption(parse) => parse.takes_arg(),
            _ => false,
        }
    }

    fn tuple_len(&self) -> Option<usize> {
        use self::Action::*;

        match self {
            Push(_, parse) | SetField(parse) | SetOption(parse) => parse.tuple_len,
            _ => None,
        }
    }
}

impl ParseFn {
    fn make_parse_action(&self, name: Option<&str>) -> TokenStream2 {
        let name = if let Some(name) = name {
            quote! { ::std::string::ToString::to_string(#name) }
        } else {
            quote! { ::gumdrop::Opt::to_string(&_opt) }
        };

        match self {
            ParseFn::Default => quote! {
                ::std::str::FromStr::from_str(::gumdrop::to_str(_arg)?)
                    .map_err(|e| ::gumdrop::Error::failed_parse_with_name(
                        #name, ::std::string::ToString::to_string(&e)))?
            },
            ParseFn::FromStr(None) => quote! {
                ::std::convert::From::from(::gumdrop::to_str(_arg)?)
            },
            ParseFn::FromStr(Some(fun)) => quote! {
                #fun(::gumdrop::to_str(_arg)?)
            },
            ParseFn::TryFromStr(fun) => quote! {
                #fun(::gumdrop::to_str(_arg)?)
                    .map_err(|e| ::gumdrop::Error::failed_parse_with_name(
                        #name, ::std::string::ToString::to_string(&e)))?
            },
            ParseFn::FromOsStr(None) => quote! {
                ::std::convert::From::from(_arg)
            },
            ParseFn::FromOsStr(Some(fun)) => quote! {
                #fun(_arg)
            },
            ParseFn::TryFromOsStr(fun) => quote! {
                #fun(_arg)
                    .map_err(|e| ::gumdrop::Error::failed_parse_with_name(
                        #name, ::std::string::ToString::to_string(&e)))?
            },
        }
    }

    fn make_parse_default_action(&self, ident: &Ident, expr: &str) -> TokenStream2 {
        match self {
            ParseFn::Default => quote! {
                ::std::str::FromStr::from_str(#expr)
                    .map_err(|e| ::gumdrop::Error::failed_parse_default(
                        stringify!(#ident), #expr,
                        ::std::string::ToString::to_string(&e)))?
            },
            ParseFn::FromStr(None) => quote! {
                ::std::convert::From::from(#expr)
            },
            ParseFn::FromStr(Some(fun)) => quote! {
                #fun(#expr)
            },
            ParseFn::TryFromStr(fun) => quote! {
                #fun(#expr)
                    .map_err(|e| ::gumdrop::Error::failed_parse_default(
                        stringify!(#ident), #expr,
                        ::std::string::ToString::to_string(&e)))?
            },
            ParseFn::FromOsStr(None) => quote! {
                ::std::convert::From::from(::std::ffi::OsStr::new(#expr))
            },
            ParseFn::FromOsStr(Some(fun)) => quote! {
                #fun(::std::ffi::OsStr::new(#expr))
            },
            ParseFn::TryFromOsStr(fun) => quote! {
                #fun(::std::ffi::OsStr::new(#expr))
                    .map_err(|e| ::gumdrop::Error::failed_parse_default(
                        stringify!(#ident), #expr,
                        ::std::string::ToString::to_string(&e)))?
            },
        }
    }
}

impl FreeAction {
    fn infer(ty: &Type, opts: &AttrOpts) -> FreeAction {
        match ty {
            Type::Path(path) => {
                let path = path.path.segments.last().unwrap();

                match &path.ident.to_string()[..] {
                    "Option" => FreeAction::SetOption,
                    "Vec" if !opts.no_multi => {
                        FreeAction::Push(Ident::new("push", Span::call_site()))
                    }
                    _ => {
                        if let Some(meth) = &opts.multi {
                            FreeAction::Push(meth.clone())
                        } else {
                            FreeAction::SetField
                        }
                    }
                }
            }
            _ => FreeAction::SetField,
        }
    }

    fn is_push(&self) -> bool {
        matches!(self, FreeAction::Push(_))
    }
}

impl<'a> FreeOpt<'a> {
    fn mark_used(&self) -> TokenStream2 {
        if self.required {
            let field = self.field;
            quote! { _used.#field = true; }
        } else {
            quote! {}
        }
    }
}

impl<'a> Opt<'a> {
    fn display_form(&self) -> String {
        if let Some(long) = &self.long {
            format!("--{}", long)
        } else {
            format!("-{}", self.short.unwrap())
        }
    }

    fn mark_used(&self) -> TokenStream2 {
        if self.required {
            let field = self.field;
            quote! { _used.#field = true; }
        } else {
            quote! {}
        }
    }

    fn make_action(&self) -> TokenStream2 {
        use self::Action::*;

        let field = self.field;
        let mark_used = self.mark_used();

        let action = match &self.action {
            Count => quote! {
                _result.#field += 1;
            },
            Push(meth, parse) => {
                let act = parse.make_action_type();

                quote! {
                    _result.#field.#meth(#act);
                }
            }
            SetField(parse) => {
                let act = parse.make_action_type();

                quote! {
                    _result.#field = #act;
                }
            }
            SetOption(parse) => {
                let act = parse.make_action_type();

                quote! {
                    _result.#field = ::std::option::Option::Some(#act);
                }
            }
            Switch => quote! {
                _result.#field = true;
            },
        };

        quote! {
            #mark_used
            #action
        }
    }

    fn make_action_arg(&self) -> TokenStream2 {
        use self::Action::*;

        let field = self.field;
        let mark_used = self.mark_used();

        let action = match &self.action {
            Push(meth, parse) => {
                let act = parse.make_action_type_arg();

                quote! {
                    _result.#field.#meth(#act);
                }
            }
            SetField(parse) => {
                let act = parse.make_action_type_arg();

                quote! {
                    _result.#field = #act;
                }
            }
            SetOption(parse) => {
                let act = parse.make_action_type_arg();

                quote! {
                    _result.#field = ::std::option::Option::Some(#act);
                }
            }
            _ => unreachable!(),
        };

        quote! {
            #mark_used
            #action
        }
    }
}

impl ParseMethod {
    fn make_action_type(&self) -> TokenStream2 {
        let parse = self.parse_fn.make_parse_action(None);

        match self.tuple_len {
            None => quote! { {
                let _arg = _parser.next_arg()
                    .ok_or_else(|| ::gumdrop::Error::missing_argument(&_opt))?;

                #parse
            } },
            Some(n) => {
                let num = 0..n;
                let n = repeat(n);
                let parse = repeat(parse);

                quote! {
                    ( #( {
                        let _found = #num;
                        let _arg = _parser.next_arg()
                            .ok_or_else(|| ::gumdrop::Error::insufficient_arguments(
                                &_opt, #n, _found))?;

                        #parse
                    } , )* )
                }
            }
        }
    }

    fn make_action_type_arg(&self) -> TokenStream2 {
        match self.tuple_len {
            None => self.parse_fn.make_parse_action(None),
            Some(_) => unreachable!(),
        }
    }
    fn takes_arg(&self) -> bool {
        !matches!(self.tuple_len, Some(0))
    }
}

fn first_ty_param(ty: &Type) -> Option<&Type> {
    match ty {
        Type::Path(path) => {
            let path = path.path.segments.last().unwrap();

            match &path.arguments {
                PathArguments::AngleBracketed(data) => data
                    .args
                    .iter()
                    .filter_map(|arg| match arg {
                        GenericArgument::Type(ty) => Some(ty),
                        _ => None,
                    })
                    .next(),
                _ => None,
            }
        }
        _ => None,
    }
}

fn tuple_len(ty: &Type) -> Option<usize> {
    match ty {
        Type::Tuple(tup) => Some(tup.elems.len()),
        _ => None,
    }
}

fn make_command_name(name: &str) -> String {
    let mut res = String::with_capacity(name.len());

    for ch in name.chars() {
        if ch.is_lowercase() {
            res.push(ch);
        } else {
            if !res.is_empty() {
                res.push('-');
            }

            res.extend(ch.to_lowercase());
        }
    }

    res
}

fn make_long_name(name: &str) -> String {
    name.replace('_', "-")
}

fn make_short_name(name: &str, short: &[char]) -> Option<char> {
    let first = name.chars().next().expect("empty field name");

    if !short.contains(&first) {
        return Some(first);
    }

    let mut to_upper = first.to_uppercase();
    let upper = to_upper.next().expect("empty to_uppercase");

    if to_upper.next().is_some() {
        return None;
    }

    if !short.contains(&upper) {
        Some(upper)
    } else {
        None
    }
}

fn validate_long_name(span: Span, name: &str, names: &[String]) -> Result<(), Error> {
    if name.is_empty() || name.starts_with('-') || name.contains(char::is_whitespace) {
        Err(Error::new(span, "not a valid long option"))
    } else if names.iter().any(|n| n == name) {
        Err(Error::new(span, "duplicate option name"))
    } else {
        Ok(())
    }
}

fn validate_short_name(span: Span, ch: char, names: &[char]) -> Result<(), Error> {
    if ch == '-' || ch.is_whitespace() {
        Err(Error::new(span, "not a valid short option"))
    } else if names.contains(&ch) {
        Err(Error::new(span, "duplicate option name"))
    } else {
        Ok(())
    }
}

fn make_meta(name: &str, action: &Action) -> String {
    use std::fmt::Write;

    let mut name = name.replace('_', "-").to_uppercase();

    match action.tuple_len() {
        Some(0) => unreachable!(),
        Some(1) | None => (),
        Some(2) => {
            name.push_str(" VALUE");
        }
        Some(n) => {
            for i in 1..n {
                let _ = write!(name, " VALUE{}", i - 1);
            }
        }
    }

    name
}
