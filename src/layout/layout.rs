use std::collections::HashMap;

use serde::{Deserialize, Serialize};

use crate::{
    diagnostics::Span,
    lowering::NodeId,
    semantics::{ResolvedTypeKind, TypeId, TypeInfo},
    target::TargetSpec,
};

#[derive(Serialize, Deserialize, Debug, Clone, Hash, Eq, PartialEq)]
pub struct Layout {
    pub size: usize,
    pub alignment: usize,
}

impl Layout {
    pub fn empty() -> Self {
        Layout {
            size: 0,
            alignment: 0,
        }
    }
}

#[derive(Debug, Clone)]
pub enum LayoutError {
    CyclicTypeDependency,
    InvalidLayout { message: String, span: Span },
}

pub type LayoutResult = Result<Layout, LayoutError>;

pub struct LayoutEngine<'a> {
    pub target: &'a TargetSpec,

    // Only successfully computed layouts are stored here.
    pub cache: HashMap<TypeId, Layout>,

    // Types currently being laid out.
    recursion_stack: Vec<TypeId>,
}

impl<'a> LayoutEngine<'a> {
    pub fn new(target: &'a TargetSpec) -> Self {
        Self {
            target,
            cache: HashMap::new(),
            recursion_stack: Vec::new(),
        }
    }

    pub fn layout_of(&mut self, kind: &ResolvedTypeKind, type_id: TypeId) -> LayoutResult {
        if let Some(existing_layout) = self.cache.get(&type_id) {
            return Ok(existing_layout.clone());
        }

        if self.recursion_stack.contains(&type_id) {
            let error = LayoutError::CyclicTypeDependency;
            return Err(error);
        }

        self.recursion_stack.push(type_id.clone());
        let result = self.calculate_layout(kind);
        self.recursion_stack.pop();

        match result {
            Ok(layout) => {
                self.cache.insert(type_id, layout.clone());
                Ok(layout)
            }

            Err(error) => Err(error),
        }
    }

    pub fn calculate_layout(&mut self, kind: &ResolvedTypeKind) -> LayoutResult {
        match kind {
            ResolvedTypeKind::I8
            | ResolvedTypeKind::U8
            | ResolvedTypeKind::Char8
            | ResolvedTypeKind::Bool => Ok(Layout {
                size: 1,
                alignment: 1,
            }),
            ResolvedTypeKind::I16 | ResolvedTypeKind::U16 | ResolvedTypeKind::Char16 => {
                Ok(Layout {
                    size: 2,
                    alignment: 2,
                })
            }
            ResolvedTypeKind::I32
            | ResolvedTypeKind::U32
            | ResolvedTypeKind::Char32
            | ResolvedTypeKind::F32 => Ok(Layout {
                size: 4,
                alignment: 4,
            }),
            ResolvedTypeKind::I64 | ResolvedTypeKind::U64 | ResolvedTypeKind::F64 => Ok(Layout {
                size: 8,
                alignment: 8,
            }),
            ResolvedTypeKind::I128 | ResolvedTypeKind::U128 => Ok(Layout {
                size: 16,
                alignment: 16,
            }),
            ResolvedTypeKind::USize | ResolvedTypeKind::ISize => Ok(Layout {
                size: self.target.int_width,
                alignment: self.target.int_width,
            }),
            ResolvedTypeKind::Str => Ok(Layout {
                size: self.target.pointer_width,
                alignment: self.target.pointer_width,
            }),
            ResolvedTypeKind::Pointer { .. }
            | ResolvedTypeKind::Ref { .. }
            | ResolvedTypeKind::Func { .. } => Ok(Layout {
                size: self.target.pointer_width,
                alignment: self.target.pointer_width,
            }),
            ResolvedTypeKind::Array { inner, size } => {
                let element_layout = self.layout_of(&inner.kind, inner.type_id.clone())?;

                let len = size.unwrap_or(1) as usize;

                Ok(Layout {
                    size: element_layout.size * len,
                    alignment: element_layout.alignment,
                })
            }
            ResolvedTypeKind::Struct { members, .. } => self.struct_layout(members),
            ResolvedTypeKind::Enum { underlying, .. } => self.enum_layout(underlying),
            ResolvedTypeKind::Variant { arms, .. } => self.variant_layout(arms),
            ResolvedTypeKind::Tuple { fields } => self.tuple_layout(fields),
            ResolvedTypeKind::Unit => Ok(Layout {
                size: 0,
                alignment: 1,
            }),

            _ => Ok(Layout::empty()),
        }
    }

    fn struct_layout(&mut self, members: &[(String, TypeInfo, NodeId)]) -> LayoutResult {
        let mut offset = 0;
        let mut max_align = 1;

        for (field_name, field_ty, _node_id) in members {
            // A generic template member is a placeholder: its layout is only
            // known once specialized, so skip it when shaping a template.
            if matches!(field_ty.kind, ResolvedTypeKind::GenericParam(_)) {
                continue;
            }

            let field_layout = self.layout_of(&field_ty.kind, field_ty.type_id.clone())?;

            if field_layout.alignment == 0 {
                let error = LayoutError::InvalidLayout {
                    message: format!("field `{}` has an invalid zero alignment", field_name),
                    span: field_ty.span.clone(),
                };

                return Err(error);
            }

            let padding = (field_layout.alignment - (offset % field_layout.alignment))
                % field_layout.alignment;

            offset += padding;
            offset += field_layout.size;

            max_align = max_align.max(field_layout.alignment);
        }

        let tail_padding = (max_align - (offset % max_align)) % max_align;
        offset += tail_padding;

        Ok(Layout {
            size: offset,
            alignment: max_align,
        })
    }

    fn enum_layout(&mut self, underlying: &TypeInfo) -> LayoutResult {
        self.layout_of(&underlying.kind, underlying.type_id.clone())
    }

    fn variant_layout(
        &mut self,
        arms: &[(String, TypeInfo, NodeId, Vec<TypeInfo>)],
    ) -> LayoutResult {
        let tag_size = 4;
        let tag_alignment = 4;

        let mut max_payload_size = 0;
        let mut max_payload_align = 1;

        for (_arm_name, _arm_info, _node_id, payloads) in arms {
            let mut arm_offset = 0;
            let mut arm_max_align = 1;

            for field_ty in payloads {
                if matches!(field_ty.kind, ResolvedTypeKind::GenericParam(_)) {
                    continue;
                }

                let field_layout = self.layout_of(&field_ty.kind, field_ty.type_id.clone())?;

                if field_layout.alignment == 0 {
                    let error = LayoutError::InvalidLayout {
                        message: "variant field has zero alignment".to_string(),
                        span: field_ty.span.clone(),
                    };

                    return Err(error);
                }

                let padding = (field_layout.alignment - (arm_offset % field_layout.alignment))
                    % field_layout.alignment;

                arm_offset += padding;
                arm_offset += field_layout.size;

                arm_max_align = arm_max_align.max(field_layout.alignment);
            }

            let arm_tail_padding = (arm_max_align - (arm_offset % arm_max_align)) % arm_max_align;

            arm_offset += arm_tail_padding;

            max_payload_size = max_payload_size.max(arm_offset);
            max_payload_align = max_payload_align.max(arm_max_align);
        }

        let final_alignment = tag_alignment.max(max_payload_align);

        let payload_padding =
            (max_payload_align - (tag_size % max_payload_align)) % max_payload_align;

        let mut total_size = tag_size + payload_padding + max_payload_size;

        let final_tail_padding =
            (final_alignment - (total_size % final_alignment)) % final_alignment;

        total_size += final_tail_padding;

        Ok(Layout {
            size: total_size,
            alignment: final_alignment,
        })
    }

    fn tuple_layout(&mut self, fields: &[TypeInfo]) -> LayoutResult {
        let mut offset = 0;
        let mut max_align = 1;

        for field_ty in fields {
            if matches!(field_ty.kind, ResolvedTypeKind::GenericParam(_)) {
                continue;
            }

            let field_layout = self.layout_of(&field_ty.kind, field_ty.type_id.clone())?;

            if field_layout.alignment == 0 {
                let error = LayoutError::InvalidLayout {
                    message: "tuple field has zero alignment".to_string(),
                    span: field_ty.span.clone(),
                };

                return Err(error);
            }

            let padding = (field_layout.alignment - (offset % field_layout.alignment))
                % field_layout.alignment;

            offset += padding;
            offset += field_layout.size;

            max_align = max_align.max(field_layout.alignment);
        }

        let tail_padding = (max_align - (offset % max_align)) % max_align;
        offset += tail_padding;

        Ok(Layout {
            size: offset,
            alignment: max_align,
        })
    }
}
