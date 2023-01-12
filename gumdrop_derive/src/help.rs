use std::iter::repeat;

use crate::{Cmd, FreeOpt, Opt};

pub(crate) fn make_usage(help: &Option<String>, free: &[FreeOpt], opts: &[Opt]) -> String {
    let mut res = String::new();

    if let Some(help) = help {
        res.push_str(help);
        res.push('\n');
    }

    let width = max_width(free, |opt| opt.width()).max(max_width(opts, |opt| opt.width()));

    if !free.is_empty() {
        if !res.is_empty() {
            res.push('\n');
        }

        res.push_str("Positional arguments:\n");

        for opt in free {
            let mut line = String::from("  ");

            line.push_str(&opt.field.to_string());

            if let Some(help) = &opt.help {
                if line.len() < width {
                    let n = width - line.len();
                    line.extend(repeat(' ').take(n));
                } else {
                    line.push('\n');
                    line.extend(repeat(' ').take(width));
                }

                line.push_str(help);
            }

            res.push_str(&line);
            res.push('\n');
        }
    }

    if !opts.is_empty() {
        if !res.is_empty() {
            res.push('\n');
        }

        res.push_str("Optional arguments:\n");

        for opt in opts {
            res.push_str(&opt.usage(width));
            res.push('\n');
        }
    }

    // Pop the last newline so the user may println!() the result.
    res.pop();

    res
}

pub(crate) fn make_cmd_usage(cmds: &[Cmd]) -> String {
    let mut res = String::new();

    let width = max_width(
        cmds,
        // Two spaces each, before and after
        |cmd| cmd.name.len() + 4,
    );

    for cmd in cmds {
        let mut line = String::from("  ");

        line.push_str(&cmd.name);

        if let Some(help) = &cmd.help {
            if line.len() < width {
                let n = width - line.len();
                line.extend(repeat(' ').take(n));
            } else {
                line.push('\n');
                line.extend(repeat(' ').take(width));
            }

            line.push_str(help);
        }

        res.push_str(&line);
        res.push('\n');
    }

    // Pop the last newline
    res.pop();

    res
}

impl<'a> Opt<'a> {
    fn width(&self) -> usize {
        let short = self.short.map_or(0, |_| 1 + 1); // '-' + char
        let long = self.long.as_ref().map_or(0, |s| s.len() + 2); // "--" + str
        let sep = if short == 0 || long == 0 { 0 } else { 2 }; // ", "
        let meta = self.meta.as_ref().map_or(0, |s| s.len() + 1); // ' ' + meta

        2 + short + long + sep + meta + 2 // total + spaces before and after
    }

    fn usage(&self, col_width: usize) -> String {
        let mut res = String::from("  ");

        if let Some(short) = self.short {
            res.push('-');
            res.push(short);
        }

        if self.short.is_some() && self.long.is_some() {
            res.push_str(", ");
        }

        if let Some(long) = &self.long {
            res.push_str("--");
            res.push_str(long);
        }

        if let Some(meta) = &self.meta {
            res.push(' ');
            res.push_str(meta);
        }

        if self.help.is_some() || self.default.is_some() {
            if res.len() < col_width {
                let n = col_width - res.len();
                res.extend(repeat(' ').take(n));
            } else {
                res.push('\n');
                res.extend(repeat(' ').take(col_width));
            }
        }

        if let Some(help) = &self.help {
            res.push_str(help);
        }

        if let Some(default) = &self.default {
            res.push_str(" (default: ");
            res.push_str(default);
            res.push(')');
        }

        res
    }
}

impl<'a> FreeOpt<'a> {
    fn width(&self) -> usize {
        2 + self.field.to_string().len() + 2 // name + spaces before and after
    }
}

fn max_width<T, F>(items: &[T], f: F) -> usize
where
    F: Fn(&T) -> usize,
{
    const MIN_WIDTH: usize = 8;
    const MAX_WIDTH: usize = 30;

    let width = items
        .iter()
        .filter_map(|item| {
            let w = f(item);

            if w > MAX_WIDTH {
                None
            } else {
                Some(w)
            }
        })
        .max()
        .unwrap_or(0);

    width.clamp(MIN_WIDTH, MAX_WIDTH)
}
