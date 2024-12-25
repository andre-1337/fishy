// The following describes an API to construct types in Fishy, as well as convert
// them down to LLVM types.

use inkwell::{
    context::Context,
    types::{BasicType, BasicTypeEnum, FunctionType},
    AddressSpace,
};

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

impl Type {
    pub fn to_llvm<'ctx>(&self, ctx: &'ctx Context) -> BasicTypeEnum<'ctx> {
        match self {
            Type::Int(bits, signed) => {
                let int_type = match bits {
                    1 => ctx.bool_type(),
                    8 => ctx.i8_type(),
                    16 => ctx.i16_type(),
                    32 => ctx.i32_type(),
                    64 => ctx.i64_type(),

                    _ => panic!("invalid integer width '{bits}'"),
                };

                if *signed {
                    int_type.into()
                } else {
                    int_type.const_zero().get_type().into()
                }
            }
            Type::Float(bits) => {
                let float_type = match bits {
                    16 => ctx.f16_type(),
                    32 => ctx.f32_type(),
                    64 => ctx.f64_type(),

                    _ => panic!("invalid float width '{bits}'"),
                };

                float_type.into()
            }
            Type::Boolean => ctx.bool_type().into(),
            Type::Void => panic!("cannot represent void types with BasicTypeEnum"),
            Type::Pointer(inner) => inner.to_llvm(ctx).ptr_type(AddressSpace::default()).into(),
            Type::Array(inner, size) => inner.to_llvm(ctx).array_type(*size).into(),
            Type::Struct(fields) => {
                let fields: Vec<BasicTypeEnum> = fields
                    .iter()
                    .map(|(_, field_type)| field_type.to_llvm(ctx))
                    .collect();

                ctx.struct_type(&fields, false).into()
            }
            Type::FunctionPtr(_, _) => {
                let fn_type = self.to_llvm_function_type(ctx);
                fn_type.ptr_type(AddressSpace::default()).into()
            }
        }
    }

    pub fn to_llvm_function_type<'ctx>(&self, ctx: &'ctx Context) -> FunctionType<'ctx> {
        if let Type::FunctionPtr(return_type, param_types) = self {
            let param_types: Vec<_> = param_types
                .iter()
                .map(|param| param.to_llvm(ctx).as_basic_type_enum())
                .collect();

            let param_types_meta: Vec<_> = param_types.into_iter().map(|x| x.into()).collect();

            let function = match **return_type {
                Type::Void => ctx.void_type().fn_type(&param_types_meta, false),
                _ => return_type.to_llvm(ctx).fn_type(&param_types_meta, false),
            };

            function
        } else {
            panic!("expected a function pointer, got '{:?}' instead", self)
        }
    }
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
