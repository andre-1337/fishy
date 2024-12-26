// The following describes an API to construct types in Fishy, as well as convert
// them down to LLVM types.



#[derive(Debug, Clone)]
pub enum Type {
    // u8: bits, bool: signed
    Int(u8, bool),
    // u8: bits
    Float(u8),
    Boolean,
    Void,
    // Box<Type>: inner type
    Pointer(Box<Type>),
    // Box<Type>: element type, u32: size
    Array(Box<Type>, u32),
    // Vec<(String, Type)>: fields
    Struct(Vec<(String, Type)>),
    // Box<Type>: return type, Vec<Type> parameter types
    FunctionPtr(Box<Type>, Vec<Type>),
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Int(bits, signed) => write!(f, "{}{}", if *signed { "i" } else { "u" }, bits),
            Type::Float(bits) => write!(f, "f{}", bits),
            Type::Boolean => write!(f, "bool"),
            Type::Void => write!(f, "void"),
            Type::Pointer(inner) => write!(f, "^{}", inner),
            Type::Array(inner, size) => write!(f, "[{}; {}]", inner, size),
            Type::Struct(fields) => write!(
                f,
                "struct {{ {} }}",
                fields
                    .iter()
                    .map(|(name, ty)| format!("{}: {}", name, ty))
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            Type::FunctionPtr(return_type, param_types) => {
                write!(f, "fn(")?;
                for (i, param) in param_types.iter().enumerate() {
                    write!(f, "{}", param)?;
                    if i < param_types.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, ") -> {}", return_type)
            }
        }
    }
}
