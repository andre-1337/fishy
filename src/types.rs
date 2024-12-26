// The following describes an API to construct types in Fishy

#[derive(Debug, Clone)]
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
    // Box<Type>: return type, Vec<Type> parameter types
    FunctionPtr(Box<Type>, Vec<Type>),
    // String: name
    UserDefined(String),
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
            Type::Reference(inner, is_mut_ref) => write!(f, "&{}{}", if *is_mut_ref { "mut " } else { "" }, inner),
            Type::Array(inner, size) => write!(f, "[{}; {}]", inner, size),
            Type::FunctionPtr(return_type, param_types) => {
                write!(f, "fn(")?;
                for (i, param) in param_types.iter().enumerate() {
                    write!(f, "{}", param)?;
                    if i < param_types.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, ") -> {}", return_type)
            },
            Type::UserDefined(name) => write!(f, "{}", name),
        }
    }
}
