// The following describes an API to construct types in Fishy
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FnPtr {
    pub return_type: Box<Type>,
    pub param_types: Vec<Type>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    // u8: bits, bool: signed
    Int(u8, bool),
    // u8: bits
    Float(u8),
    Boolean,
    Void,
    // Box<Type>: inner type, bool: is_mut_ptr
    Pointer(Box<Type>, bool),
    // Box<Type>: inner type, bool: is_mut_ref
    Reference(Box<Type>, bool),
    // Box<Type>: element type, u32: size
    Array(Box<Type>, u32),
    // function pointer
    FunctionPtr(FnPtr),
    // String: name
    UserDefined(String),
    // Vec<(String, Type)>: field name & field type, Vec<(String, Type)>: method name & method type, bool: is_trait
    Struct(String, Vec<(String, Type)>, Vec<(String, Type)>, bool),
}

impl Type {
    pub fn as_fn(&self) -> &FnPtr {
        match self {
            Type::FunctionPtr(fn_ptr) => fn_ptr,
            _ => {
                panic!("YOU SHOULDN'T BE ABLE TO SEE THIS; PLEASE OPEN AN ISSUE IN THE REPOSITORY")
            }
        }
    }

    #[inline(always)]
    pub fn new_i8() -> Type {
        Type::Int(8, true)
    }

    #[inline(always)]
    pub fn new_i16() -> Type {
        Type::Int(16, true)
    }

    #[inline(always)]
    pub fn new_i32() -> Type {
        Type::Int(32, true)
    }

    #[inline(always)]
    pub fn new_i64() -> Type {
        Type::Int(64, true)
    }

    #[inline(always)]
    pub fn new_u8() -> Type {
        Type::Int(8, false)
    }

    #[inline(always)]
    pub fn new_u16() -> Type {
        Type::Int(16, false)
    }

    #[inline(always)]
    pub fn new_u32() -> Type {
        Type::Int(32, false)
    }

    #[inline(always)]
    pub fn new_u64() -> Type {
        Type::Int(64, false)
    }

    #[inline(always)]
    pub fn new_f16() -> Type {
        Type::Float(16)
    }

    #[inline(always)]
    pub fn new_f32() -> Type {
        Type::Float(32)
    }

    #[inline(always)]
    pub fn new_f64() -> Type {
        Type::Float(64)
    }

    #[inline(always)]
    pub fn new_bool() -> Type {
        Type::Boolean
    }

    #[inline(always)]
    pub fn new_void() -> Type {
        Type::Void
    }

    #[inline(always)]
    pub fn new_mut_ptr(inner: Type) -> Type {
        Type::Pointer(Box::new(inner), true)
    }

    #[inline(always)]
    pub fn new_const_ptr(inner: Type) -> Type {
        Type::Pointer(Box::new(inner), false)
    }

    #[inline(always)]
    pub fn new_mut_ref(inner: Type) -> Type {
        Type::Reference(Box::new(inner), true)
    }

    #[inline(always)]
    pub fn new_ref(inner: Type) -> Type {
        Type::Reference(Box::new(inner), false)
    }

    #[inline(always)]
    pub fn new_array(inner: Type, size: u32) -> Type {
        Type::Array(Box::new(inner), size)
    }

    #[inline(always)]
    pub fn new_user_type(name: &str) -> Type {
        Type::UserDefined(name.to_string())
    }

    #[inline(always)]
    pub fn new_str() -> Type {
        Type::new_const_ptr(Type::new_u8())
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Int(bits, signed) => write!(f, "{}{}", if *signed { "i" } else { "u" }, bits),
            Type::Float(bits) => write!(f, "f{}", bits),
            Type::Boolean => write!(f, "bool"),
            Type::Void => write!(f, "void"),
            Type::Pointer(inner, is_mut_ptr) => {
                write!(
                    f,
                    "^{} {}",
                    if *is_mut_ptr { "mut" } else { "const" },
                    inner
                )
            }
            Type::Reference(inner, is_mut_ref) => {
                write!(f, "&{}{}", if *is_mut_ref { "mut " } else { "" }, inner)
            }
            Type::Array(inner, size) => write!(f, "[{}; {}]", inner, size),
            Type::FunctionPtr(fn_ptr) => {
                write!(f, "fn(")?;
                for (i, param) in fn_ptr.param_types.iter().enumerate() {
                    write!(f, "{}", param)?;
                    if i < fn_ptr.param_types.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, ") -> {}", fn_ptr.return_type)
            }
            Type::UserDefined(name) => write!(f, "{}", name),
            Type::Struct(name, fields, methods, is_trait) => {
                let typ = if *is_trait { "trait" } else { "struct" };
                write!(f, "{typ} {name} {{")?;

                for (name, typ) in fields {
                    write!(f, "{name}: {typ}")?;
                }

                for (name, method) in methods {
                    let method = method.as_fn();
                    let param_types = method
                        .param_types
                        .iter()
                        .map(|typ| format!("{typ}"))
                        .collect::<Vec<String>>()
                        .join(", ");
                    write!(f, "fn {name}({param_types}) -> {};", method.return_type)?;
                }

                write!(f, "}}")
            }
        }
    }
}
