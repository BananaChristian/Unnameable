use std::collections::HashMap;

use crate::{
    diagnostics::Span,
    semantics::{ResolvedTypeKind, TypeId, TypeInfo},
    target::TargetSpec,
};

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct Layout {
    pub size: usize,      //Total size in bytes
    pub alignment: usize, //Required alignment boundary
}

impl Layout {
    pub fn empty() -> Self {
        Layout {
            size: 0,
            alignment: 0,
        }
    }
}

pub struct LayoutEngine<'a> {
    pub target: &'a TargetSpec,
    pub cache: HashMap<TypeId, Layout>,
    pub errors: Vec<(String, Span)>,
    recursion_stack: Vec<TypeId>,
    pub corrupted: bool,
}

impl<'a> LayoutEngine<'a> {
    pub fn new(target: &'a TargetSpec) -> Self {
        LayoutEngine {
            target,
            cache: HashMap::new(),
            errors: Vec::new(),
            recursion_stack: Vec::new(),
            corrupted: false,
        }
    }

    pub fn layout_of(&mut self, kind: &ResolvedTypeKind, type_id: TypeId, span: Span) -> Layout {
        if let Some(existing_id) = self.cache.get(&type_id) {
            return existing_id.clone();
        }

        if self.recursion_stack.contains(&type_id) {
            self.report("Cyclic type dependency".to_string(), span);
            return Layout::empty();
        }

        self.recursion_stack.push(type_id.clone());
        let layout = self.calculate_layout(kind);
        self.recursion_stack.pop();
        self.cache.insert(type_id, layout.clone());
        layout
    }

    pub fn calculate_layout(&mut self, kind: &ResolvedTypeKind) -> Layout {
        match kind {
            ResolvedTypeKind::I8 | ResolvedTypeKind::U8 => Layout {
                size: 1,
                alignment: 1,
            },
            ResolvedTypeKind::I16 | ResolvedTypeKind::U16 => Layout {
                size: 2,
                alignment: 2,
            },
            ResolvedTypeKind::I32 | ResolvedTypeKind::U32 | ResolvedTypeKind::F32 => Layout {
                size: 4,
                alignment: 4,
            },
            ResolvedTypeKind::I64 | ResolvedTypeKind::U64 | ResolvedTypeKind::F64 => Layout {
                size: 8,
                alignment: 8,
            },
            ResolvedTypeKind::I128 | ResolvedTypeKind::U128 => Layout {
                size: 16,
                alignment: 16,
            },
            ResolvedTypeKind::USize | ResolvedTypeKind::ISize => Layout {
                size: self.target.int_width,
                alignment: self.target.int_width,
            },
            ResolvedTypeKind::Pointer { .. }
            | ResolvedTypeKind::Ref { .. }
            | ResolvedTypeKind::Func { .. } => Layout {
                size: self.target.pointer_width,
                alignment: self.target.pointer_width,
            },
            ResolvedTypeKind::Array { inner, size } => {
                let element_layout =
                    self.layout_of(&inner.kind, inner.type_id.clone(), inner.span.clone());

                let len = match size {
                    Some(extracted) => *extracted,
                    None => 1,
                } as usize;

                Layout {
                    size: element_layout.size * len,
                    alignment: element_layout.alignment,
                }
            }
            ResolvedTypeKind::Struct { members, .. } => self.struct_layout(members),
            ResolvedTypeKind::Enum { underlying, .. } => self.enum_layout(underlying),
            ResolvedTypeKind::Variant { arms, .. } => self.variant_layout(arms),
            _ => Layout::empty(),
        }
    }

    fn struct_layout(&mut self, members: &Vec<(String, TypeInfo)>) -> Layout {
        let mut offset = 0;
        let mut max_align = 1;

        for (_, field_ty) in members {
            let field_layout = self.layout_of(
                &field_ty.kind,
                field_ty.type_id.clone(),
                field_ty.span.clone(),
            );

            let padding = if field_layout.alignment == 0 {
                0
            } else {
                (field_layout.alignment - (offset % field_layout.alignment))
                    % field_layout.alignment
            };

            offset += padding;
            offset += field_layout.size;

            if field_layout.alignment > max_align {
                max_align = field_layout.alignment
            }
        }

        let tail_padding = (max_align - (offset % max_align)) % max_align;
        offset += tail_padding;

        Layout {
            size: offset,
            alignment: max_align,
        }
    }

    fn enum_layout(&mut self, underlying: &TypeInfo) -> Layout {
        let underlying_layout = self.layout_of(
            &underlying.kind,
            underlying.type_id.clone(),
            underlying.span.clone(),
        );
        Layout {
            size: underlying_layout.size,
            alignment: underlying_layout.alignment,
        }
    }

    fn variant_layout(&mut self, arms: &Vec<(String, TypeInfo, Vec<TypeInfo>)>) -> Layout {
        let tag_size: usize = 4;
        let tag_alignment: usize = 4;

        let mut max_payload_size: usize = 0;
        let mut max_payload_align: usize = 1;

        for (_, _, payload_vec) in arms {
            let mut arm_offset: usize = 0;
            let mut arm_max_align: usize = 1;

            for field_ty in payload_vec {
                let field_layout = self.layout_of(
                    &field_ty.kind,
                    field_ty.type_id.clone(),
                    field_ty.span.clone(),
                );
                let padding = (field_ty.layout.alignment - (arm_offset % field_layout.alignment))
                    % field_layout.alignment;
                arm_offset += padding;
                arm_offset += field_layout.size;
                if field_layout.alignment > arm_max_align {
                    arm_max_align = field_layout.alignment;
                }
            }

            let arm_tail_padding = (arm_max_align - (arm_offset % arm_max_align)) % arm_max_align;
            arm_offset += arm_tail_padding;

            if arm_offset > max_payload_size {
                max_payload_size = arm_offset;
            }

            if arm_max_align > max_payload_align {
                max_payload_align = arm_max_align;
            }
        }

        let final_max_align = std::cmp::max(tag_alignment, max_payload_align);

        let payload_padding =
            (max_payload_align - (tag_size % max_payload_align)) % max_payload_align;
        let mut total_size = tag_size + payload_padding + max_payload_size;

        let final_tail_padding =
            (final_max_align - (total_size % final_max_align)) % final_max_align;
        total_size += final_tail_padding;

        Layout {
            size: total_size,
            alignment: final_max_align,
        }
    }

    pub fn report(&mut self, message: String, span: Span) {
        self.corrupted = true;
        self.errors.push((message, span));
    }
}
