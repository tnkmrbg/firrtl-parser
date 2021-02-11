pub type Info = String;
pub type Ident = String;

#[derive(Clone, Debug)]
pub struct FIRRTLFile {
    pub info: Info,
    pub circuit: Circuit,
}

impl std::fmt::Display for FIRRTLFile {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}\n{}", self.info, self.circuit)
    }
}

#[derive(Clone, Debug)]
pub struct Circuit {
    pub info: Info,
    pub modules: Vec<Module>,
    pub id: Ident,
}

impl Circuit {
    pub fn new() -> Circuit {
        Circuit {
            info: "".to_string(),
            modules: vec![],
            id: "".to_string(),
        }
    }
}

impl std::fmt::Display for Circuit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut str = format!("circuit {} : {}\n", self.id, self.info);

        let mstr: Vec<_> = (&self.modules)
            .into_iter()
            .map(|m| format!("{}", m))
            .collect();
        str = format!("{}{}", str, mstr.join("\n"));

        write!(f, "{}", str)
    }
}

#[derive(Clone, Debug)]
pub struct Module {
    pub info: Info,
    pub id: Ident,
    pub ports: Vec<Port>,
    pub body: Vec<Stmt>,
}

impl std::fmt::Display for Module {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let str = format!(
            "{:indent$}module {} : {}",
            "",
            self.id,
            self.info,
            indent = 2
        );
        // for p in &self.ports {
        //     str = format!("{}    {}\n", str, p);
        // }
        let pstr: Vec<_> = (&self.ports)
            .into_iter()
            .map(|p| format!("{:indent$}{}", "", p, indent = 4))
            .collect();

        // for stmt in &self.body {
        //     str = format!("{}    {}\n", str, stmt);
        // }
        let stmtstr: Vec<_> = (&self.body)
            .into_iter()
            .map(|stmt| format!("{:indent$}{}", "", stmt, indent = 4))
            .collect();

        write!(
            f,
            "{}\n{}\n\n{}\n",
            str,
            pstr.join("\n"),
            stmtstr.join("\n")
        )
    }
}

#[derive(Clone, Debug)]
pub struct Port {
    pub info: Info,
    pub name: Ident,
    pub direction: Direction,
    pub tpe: Type,
}

impl std::fmt::Display for Port {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let str = format!(
            "{} {} : {} {}",
            self.direction, self.name, self.tpe, self.info
        );
        write!(f, "{}", str)
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum IntLit {
    UnsignedInt(u64),
    SignedInt(i64),
    HexLit(i64),
    OctalLit(i64),
    BinaryLit(i64),
}

impl std::fmt::Display for IntLit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            IntLit::UnsignedInt(num) => {
                format!("{}", num)
            }
            IntLit::SignedInt(num) => {
                format!("{}", num)
            }
            IntLit::HexLit(num) => {
                if num < &0 {
                    let newnum = -1 * num;
                    format!("\"h-{:X}\"", newnum)
                } else {
                    format!("\"h{:X}\"", num)
                }
            }
            IntLit::OctalLit(num) => {
                if num < &0 {
                    let newnum = -1 * num;
                    format!("\"o-{:o}\"", newnum)
                } else {
                    format!("\"o{:o}\"", num)
                }
            }
            IntLit::BinaryLit(num) => {
                if num < &0 {
                    let newnum = -1 * num;
                    format!("\"b-{:b}\"", newnum)
                } else {
                    format!("\"b{:b}\"", num)
                }
            }
        };
        write!(f, "{}", str)
    }
}

#[derive(Clone, Debug)]
pub struct When {
    cond: Expr,
    info: Info,
    suit: Vec<Stmt>,
    elsecond: Option<Box<When>>,
    elseinfo: Info,
    elsesuit: Option<Vec<Stmt>>,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Direction {
    Input,
    Output,
    Inout,
}

impl std::fmt::Display for Direction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            Direction::Input => "input",
            _ => "output",
        };
        write!(f, "{}", str)
    }
}

#[derive(Clone, Debug)]
pub enum Stmt {
    DefWire {
        info: Info,
        id: Ident,
        tpe: Type,
    },
    DefRegister {
        info: Info,
        id: Ident,
        tpe: Type,
        clock: Expr,
        reset: Option<Expr>,
        init: Option<Expr>,
    },
    DefMemory {
        info: Info,
        id: Ident,
        mem: MemField,
    },
    Instance {
        info: Info,
        module_id: Ident,
        inst_id: Ident,
    },
    Node {
        info: Info,
        id: Ident,
        expr: Expr,
    },
    Connect {
        info: Info,
        lhs: Expr,
        rhs: Expr,
    },
    PartialConnect {
        info: Info,
        lhs: Expr,
        rhs: Expr,
    },
    Invalidate {
        info: Info,
        expr: Expr,
    },
    Attach {
        info: Info,
        vexp: Vec<Expr>,
    },
    When {
        cond: Expr,
        wheninfo: Info,
        whenstmt: Vec<Stmt>,
        elseinfo: Info,
        elsestmt: Vec<Stmt>,
    },
    Stop {
        info: Info,
        expr0: Expr,
        expr1: Expr,
        // intLit: IntLit,
        intlit: i64,
    },
    Printf {
        info: Info,
        expr0: Expr,
        expr1: Expr,
        expr2: Expr,
    },
    Skip {
        info: Info,
    },
}

fn print_when_rec(when: &Stmt, baselevel: usize, dedent: usize) -> String {
    match when {
        Stmt::When {
            cond,
            wheninfo,
            whenstmt,
            elseinfo,
            elsestmt,
        } => {
            let whenstr = format!(
                "{:indent$}when {} : {}",
                "",
                cond,
                wheninfo,
                indent = (baselevel - dedent) * 2
            );

            let mut whenstmtstr = vec![];

            for stmt in whenstmt {
                let tmp = match stmt {
                    Stmt::When {
                        cond: _,
                        wheninfo: _,
                        whenstmt: _,
                        elseinfo: _,
                        elsestmt: _,
                    } => print_when_rec(stmt, baselevel + 1, 0),
                    _ => {
                        format!("{:indent$}{}", "", stmt, indent = (baselevel + 1) * 2)
                    }
                };

                whenstmtstr.push(tmp);
            }

            let elsestr = format!("{:indent$}else : {}", "", elseinfo, indent = baselevel * 2);
            let mut elsestmtstr = vec![];

            for stmt in elsestmt {
                let tmp = match stmt {
                    Stmt::When {
                        cond: _,
                        wheninfo: _,
                        whenstmt: _,
                        elseinfo: _,
                        elsestmt: _,
                    } => print_when_rec(stmt, baselevel + 1, 0),
                    _ => {
                        format!("{:indent$}{}", "", stmt, indent = (baselevel + 1) * 2)
                    }
                };
                elsestmtstr.push(tmp);
            }
            return format!(
                "{}\n{}\n{}\n{}",
                whenstr,
                whenstmtstr.join("\n"),
                elsestr,
                elsestmtstr.join("\n")
            );
        }
        _ => {
            panic!("Unexpected Stmt value {}", when);
        }
    }
}

fn print_rec_when(when: &Stmt, baselevel: usize, indent: usize) -> String {
    match when {
        Stmt::When {
            cond,
            wheninfo,
            whenstmt,
            elseinfo,
            elsestmt,
        } => {
            let whenstr = format!("when {} : {}", cond, wheninfo);

            let whenblock: Vec<_> = whenstmt
                .into_iter()
                .map(|stmt| format!("{}", stmt))
                .collect();
            let delim = format!(
                "\n{sp:>width$}",
                sp = " ",
                width = (baselevel + indent + 1) * 2
            );
            let str1 = format!(
                "\t\t\tstartofwhen\n{}{}{}\n",
                whenstr,
                delim,
                whenblock.join(&delim)
            );

            let str2 = if let Some((elseblock, restblock)) = elsestmt.split_first() {
                match elseblock {
                    Stmt::When {
                        cond: _,
                        wheninfo: _,
                        whenstmt: _,
                        elseinfo: _,
                        elsestmt: _,
                    } => {
                        let str = print_rec_when(elseblock, baselevel + 1, indent + 1);

                        let innerwhenblock: Vec<_> = restblock
                            .into_iter()
                            .map(|stmt| format!("{}", stmt))
                            .collect();

                        format!(
                            "{sp:>width$}else {str}{delim}{block}",
                            sp = " ",
                            width = baselevel * 2,
                            str = str,
                            delim = delim,
                            block = innerwhenblock.join(&delim)
                        )
                    }
                    _ => {
                        let elsestr = format!(
                            "{sp:>width$}else : {elseinfo}",
                            sp = " ",
                            width = baselevel * 2,
                            elseinfo = elseinfo
                        );
                        let elseblock: Vec<_> = elsestmt
                            .into_iter()
                            .map(|stmt| format!("{}", stmt))
                            .collect();

                        format!(
                            "{}{}{}\n\t\tendofwhen",
                            elsestr,
                            delim,
                            elseblock.join(&delim)
                        )
                    }
                }
            } else {
                "".to_string()
            };

            return format!("{}{}", str1, str2);
        }
        _ => {
            panic!("Unexpected Stmt value {}", when);
        }
    }
}

impl std::fmt::Display for Stmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            Stmt::Node { info, id, expr } => {
                format!("node {} = {} {}", id, expr, info)
            }
            Stmt::Connect { info, lhs, rhs } => {
                format!("{} <= {} {}", lhs, rhs, info)
            }
            Stmt::PartialConnect { info, lhs, rhs } => {
                format!("{} <- {} {}", lhs, rhs, info)
            }
            Stmt::DefRegister {
                info,
                id,
                tpe,
                clock,
                reset,
                init,
            } => {
                if let Some(rst) = reset {
                    format!(
                        "reg {} : {}, {} with : (reset => ({}, {})) {}",
                        id,
                        tpe,
                        clock,
                        rst,
                        init.as_ref().unwrap(),
                        info
                    )
                } else {
                    format!("reg {} : {}, {} {}", id, tpe, clock, info)
                }
            }
            Stmt::DefWire { info, id, tpe } => {
                format!("wire {} : {} {}", id, tpe, info)
            }
            Stmt::Skip { info } => {
                format!("skip {}", info)
            }
            Stmt::When {
                cond: _,
                wheninfo: _,
                whenstmt: _,
                elseinfo: _,
                elsestmt: _,
            } => {
                print_when_rec(&self.clone(), 2, 2)
                // println!("{}", print_when_rec(&self.clone(), 1, 1));
                // print_rec_when(self, 2, 0)
            }
            _ => {
                format!("{:?}", self)
            }
        };
        write!(f, "{}", str)
    }
}

#[derive(Clone, Debug)]
pub struct MemField {}

#[derive(Clone, Debug, PartialEq)]
pub enum Width {
    UnknownWidth,
    IntWidth(IntLit),
}

impl std::fmt::Display for Width {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Width::UnknownWidth => {
                write!(f, "")
            }
            Width::IntWidth(w) => {
                write!(f, "{}", w)
            }
        }
    }
}

#[derive(Clone, Debug)]
pub struct Field {
    pub isflip: bool,
    pub id: Ident,
    pub tpe: Type,
}

impl std::fmt::Display for Field {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let str = format!("{} : {}", self.id, self.tpe);
        if self.isflip {
            write!(f, "flip {}", str)
        } else {
            write!(f, "{}", str)
        }
    }
}

#[derive(Clone, Debug)]
pub enum Type {
    Clock,
    Reset,
    AsyncReset,
    UnknownType,
    UInt(Width),
    SInt(Width),
    Fixed(Width, Width),
    Analog(Width),
    Bundle(Vec<Field>),
    Vector(Box<Type>, bool),
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Clock => {
                write!(f, "Clock")
            }
            Type::Reset => {
                write!(f, "Reset")
            }
            Type::UInt(w) => {
                if *w == Width::UnknownWidth {
                    write!(f, "UInt")
                } else {
                    write!(f, "UInt<{}>", w)
                }
            }
            Type::SInt(w) => {
                if *w == Width::UnknownWidth {
                    write!(f, "SInt")
                } else {
                    write!(f, "SInt<{}>", w)
                }
            }
            Type::Analog(w) => {
                if *w == Width::UnknownWidth {
                    write!(f, "Analog")
                } else {
                    write!(f, "Analog<{}>", w)
                }
            }
            Type::Bundle(v) => {
                let str = v
                    .into_iter()
                    .map(|field| format!("{}", field))
                    .collect::<Vec<String>>()
                    .join(", ");
                write!(f, "{{{}}}", str)
            }
            _ => {
                write!(f, "{:?}", self)
            }
        }
    }
}

#[derive(Clone, Debug)]
pub enum Expr {
    Unknown,
    Const(Type, IntLit),
    Reference(Ident, Type),
    SubField(Box<Expr>, String, Type),
    SubIndex(Box<Expr>, IntLit, Type),
    SubAccess(Box<Expr>, Box<Expr>, Type),
    DoPrim(PrimOp, Vec<Expr>, Vec<IntLit>, Type),
    Mux(Box<Expr>, Box<Expr>, Box<Expr>),
    ValidIf(Box<Expr>, Box<Expr>),
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            Expr::DoPrim(op, pv, intlitv, _) => {
                let mut tmp = format!("{}", primop_str(op));

                for p in pv {
                    tmp = format!("{}{}, ", tmp, p);
                }

                for intlit in intlitv {
                    tmp = format!("{}{}, ", tmp, intlit);
                }

                tmp.pop(); //erase " "
                tmp.pop(); //erase ","
                format!("{})", tmp)
            }
            Expr::SubField(expr, a, _) => {
                format!("{}.{}", expr, a)
            }
            Expr::Reference(a, _) => a.to_string(),
            Expr::Const(tpe, num) => {
                format!("{}({})", tpe, num)
            }
            Expr::Mux(s, a, b) => {
                format!("mux({}, {}, {})", s, a, b)
            }
            Expr::SubIndex(expr, index, _) => {
                format!("{}[{}]", expr, index)
            }
            Expr::SubAccess(expr0, expr1, _) => {
                format!("{}[{}]", expr0, expr1)
            }
            Expr::ValidIf(a, b) => {
                format!("validif({}, {})", a, b)
            }
            _ => format!("{:?}", self),
        };
        write!(f, "{}", str)
    }
}

#[derive(Clone, Debug)]
pub enum PrimOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Lt,
    Leq,
    Gt,
    Geq,
    Eq,
    Neq,
    Pad,
    AsUInt,
    AsAsyncReset,
    AsSInt,
    AsClock,
    AsFixedPoint,
    AsInterval,
    Shl,
    Shr,
    Dshl,
    Dshr,
    Cvt,
    Neg,
    Not,
    And,
    Or,
    Xor,
    Andr,
    Orr,
    Xorr,
    Cat,
    Bits,
    Head,
    Tail,
    Incp,
    Decp,
    Setp,
    Wrap,
    Clip,
    Squz,
    Unknown,
}

pub fn primop_str(primop: &PrimOp) -> &'static str {
    match primop {
        PrimOp::Add => "add(",
        PrimOp::Sub => "sub(",
        PrimOp::Mul => "mul(",
        PrimOp::Div => "div(",
        PrimOp::Rem => "rem(",
        PrimOp::Lt => "lt(",
        PrimOp::Leq => "leq(",
        PrimOp::Gt => "gt(",
        PrimOp::Geq => "geq(",
        PrimOp::Eq => "eq(",
        PrimOp::Neq => "neq(",
        PrimOp::Pad => "pad(",
        PrimOp::AsUInt => "asUInt(",
        PrimOp::AsAsyncReset => "asAsyncReset(",
        PrimOp::AsSInt => "asSInt(",
        PrimOp::AsClock => "asClock(",
        PrimOp::AsFixedPoint => "asFixedPoint(",
        PrimOp::AsInterval => "asInterval(",
        PrimOp::Shl => "shl(",
        PrimOp::Shr => "shr(",
        PrimOp::Dshl => "dshl(",
        PrimOp::Dshr => "dshr(",
        PrimOp::Cvt => "cvt(",
        PrimOp::Neg => "neg(",
        PrimOp::Not => "not(",
        PrimOp::And => "and(",
        PrimOp::Or => "or(",
        PrimOp::Xor => "xor(",
        PrimOp::Andr => "andr(",
        PrimOp::Orr => "orr(",
        PrimOp::Xorr => "xorr(",
        PrimOp::Cat => "cat(",
        PrimOp::Bits => "bits(",
        PrimOp::Head => "head(",
        PrimOp::Tail => "tail(",
        PrimOp::Incp => "incp(",
        PrimOp::Decp => "decp(",
        PrimOp::Setp => "setp(",
        PrimOp::Wrap => "wrap(",
        PrimOp::Clip => "clip(",
        PrimOp::Squz => "squz(",
        PrimOp::Unknown => "unknowun(",
    }
}
