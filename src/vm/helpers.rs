use crate::{
    mir::{MIRTy, MIRTykind},
    vm::{AllocId, VM, VMValue},
};

impl<'a> VM<'a> {
    fn read_bytes(&mut self, alloc_id: &AllocId, offset: usize, len: usize) -> Vec<u8> {
        let Some(alloc) = self.memory.allocations.get(alloc_id) else {
            self.report_ice(format!("Invalid or freed allocation ID: {}", alloc_id.0));
            return vec![0u8; len];
        };

        let end = offset + len;
        if end > alloc.data.len() {
            self.report_ice(format!(
            "Out-of-bounds read at AllocId({}) with offset {} and length {} (allocation size: {})",
            alloc_id.0, offset, len, alloc.data.len()
        ));
            return vec![0u8; len];
        }

        alloc.data[offset..end].to_vec()
    }

    fn write_bytes(&mut self, alloc_id: &AllocId, offset: usize, bytes: &[u8]) {
        let len = match self.memory.allocations.get(alloc_id) {
            Some(alloc) => alloc.data.len(),
            None => {
                self.report_ice(format!("Invalid or freed allocation ID: {}", alloc_id.0));
                return;
            }
        };

        let end = offset + bytes.len();
        if end > len {
            self.report_ice(format!(
            "Out-of-bounds write at AllocId({}) with offset {} and length {} (allocation size: {})",
            alloc_id.0, offset, bytes.len(), len
        ));
            return;
        }

        self.memory.allocations.get_mut(alloc_id).unwrap().data[offset..end].copy_from_slice(bytes);
    }

    pub fn read_typed(&mut self, alloc_id: &AllocId, offset: usize, ty: &MIRTy) -> VMValue {
        match &ty.kind {
            MIRTykind::I8 => VMValue::I8(self.read_bytes(alloc_id, offset, 1)[0] as i8),
            MIRTykind::U8 => VMValue::U8(self.read_bytes(alloc_id, offset, 1)[0]),
            MIRTykind::I16 => VMValue::I16(i16::from_le_bytes(
                self.read_bytes(alloc_id, offset, 2).try_into().unwrap(),
            )),
            MIRTykind::U16 => VMValue::U16(u16::from_le_bytes(
                self.read_bytes(alloc_id, offset, 2).try_into().unwrap(),
            )),
            MIRTykind::I32 => VMValue::I32(i32::from_le_bytes(
                self.read_bytes(alloc_id, offset, 4).try_into().unwrap(),
            )),
            MIRTykind::U32 => VMValue::U32(u32::from_le_bytes(
                self.read_bytes(alloc_id, offset, 4).try_into().unwrap(),
            )),
            MIRTykind::I64 => VMValue::I64(i64::from_le_bytes(
                self.read_bytes(alloc_id, offset, 8).try_into().unwrap(),
            )),
            MIRTykind::U64 => VMValue::U64(u64::from_le_bytes(
                self.read_bytes(alloc_id, offset, 8).try_into().unwrap(),
            )),
            MIRTykind::ISIZE => VMValue::Int(isize::from_le_bytes(
                self.read_bytes(alloc_id, offset, 8).try_into().unwrap(),
            )), // width depends on target_spec — adjust
            MIRTykind::USIZE => VMValue::UInt(usize::from_le_bytes(
                self.read_bytes(alloc_id, offset, 8).try_into().unwrap(),
            )),
            MIRTykind::I128 => VMValue::I128(i128::from_le_bytes(
                self.read_bytes(alloc_id, offset, 16).try_into().unwrap(),
            )),
            MIRTykind::U128 => VMValue::U128(u128::from_le_bytes(
                self.read_bytes(alloc_id, offset, 16).try_into().unwrap(),
            )),
            MIRTykind::F32 => VMValue::F32(f32::from_le_bytes(
                self.read_bytes(alloc_id, offset, 4).try_into().unwrap(),
            )),
            MIRTykind::F64 => VMValue::F64(f64::from_le_bytes(
                self.read_bytes(alloc_id, offset, 8).try_into().unwrap(),
            )),
            MIRTykind::Bool => VMValue::Bool(self.read_bytes(alloc_id, offset, 1)[0] != 0),
            MIRTykind::CHAR8 => VMValue::Char8(self.read_bytes(alloc_id, offset, 1)[0]),
            MIRTykind::CHAR16 => VMValue::Char16(u16::from_le_bytes(
                self.read_bytes(alloc_id, offset, 2).try_into().unwrap(),
            )),
            MIRTykind::CHAR32 => VMValue::Char32(u32::from_le_bytes(
                self.read_bytes(alloc_id, offset, 4).try_into().unwrap(),
            )),
            MIRTykind::Ptr => {
                let offset_bytes = self.read_bytes(alloc_id, offset, 8);
                let ptr_offset = usize::from_le_bytes(offset_bytes.try_into().unwrap());

                // Retrieve target AllocId from relocations, defaulting to AllocId(0) if uninitialized/null
                let target_alloc_id = self
                    .memory
                    .allocations
                    .get(alloc_id)
                    .and_then(|alloc| alloc.relocations.get(&offset).copied())
                    .unwrap_or(AllocId(0));

                VMValue::Ptr(target_alloc_id, ptr_offset)
            }
            MIRTykind::Array(elem_ty, count) => {
                let elems = (0..*count)
                    .map(|i| self.read_typed(alloc_id, offset + i * elem_ty.size, elem_ty))
                    .collect();
                VMValue::Array(elems)
            }
            MIRTykind::Struct(struct_id, name, fields) => {
                let mut field_offset = 0usize;
                let mut field_vals = Vec::with_capacity(fields.len());
                for (_, field_ty) in fields {
                    let padding = if field_ty.align == 0 {
                        0
                    } else {
                        (field_ty.align - (field_offset % field_ty.align)) % field_ty.align
                    };
                    field_offset += padding;
                    field_vals.push(self.read_typed(alloc_id, offset + field_offset, field_ty));
                    field_offset += field_ty.size;
                }
                VMValue::Struct {
                    struct_id: *struct_id,
                    name: name.clone(),
                    fields: field_vals,
                }
            }
            MIRTykind::Unit => VMValue::Unit,
        }
    }

    pub fn write_typed(&mut self, alloc_id: &AllocId, offset: usize, val: &VMValue, ty: &MIRTy) {
        if matches!(val, VMValue::Poison) {
            return;
        }

        match &ty.kind {
            MIRTykind::I8 => {
                let byte = self.expect_i8(val) as u8;
                self.write_bytes(alloc_id, offset, &[byte]);
            }
            MIRTykind::U8 => {
                let byte = self.expect_u8(val);
                self.write_bytes(alloc_id, offset, &[byte]);
            }
            MIRTykind::I16 => {
                let bytes = self.expect_i16(val).to_le_bytes();
                self.write_bytes(alloc_id, offset, &bytes);
            }
            MIRTykind::U16 => {
                let bytes = self.expect_u16(val).to_le_bytes();
                self.write_bytes(alloc_id, offset, &bytes);
            }
            MIRTykind::I32 => {
                let bytes = self.expect_i32(val).to_le_bytes();
                self.write_bytes(alloc_id, offset, &bytes);
            }
            MIRTykind::U32 => {
                let bytes = self.expect_u32(val).to_le_bytes();
                self.write_bytes(alloc_id, offset, &bytes);
            }
            MIRTykind::I64 => {
                let bytes = self.expect_i64(val).to_le_bytes();
                self.write_bytes(alloc_id, offset, &bytes);
            }
            MIRTykind::U64 => {
                let bytes = self.expect_u64(val).to_le_bytes();
                self.write_bytes(alloc_id, offset, &bytes);
            }
            MIRTykind::ISIZE => {
                let bytes = self.expect_isize(val).to_le_bytes();
                self.write_bytes(alloc_id, offset, &bytes);
            }
            MIRTykind::USIZE => {
                let bytes = self.expect_usize(val).to_le_bytes();
                self.write_bytes(alloc_id, offset, &bytes);
            }
            MIRTykind::I128 => {
                let bytes = self.expect_i128(val).to_le_bytes();
                self.write_bytes(alloc_id, offset, &bytes);
            }
            MIRTykind::U128 => {
                let bytes = self.expect_u128(val).to_le_bytes();
                self.write_bytes(alloc_id, offset, &bytes);
            }
            MIRTykind::F32 => {
                let bytes = self.expect_f32(val).to_le_bytes();
                self.write_bytes(alloc_id, offset, &bytes);
            }
            MIRTykind::F64 => {
                let bytes = self.expect_f64(val).to_le_bytes();
                self.write_bytes(alloc_id, offset, &bytes);
            }
            MIRTykind::Bool => {
                let byte = self.expect_bool(val) as u8;
                self.write_bytes(alloc_id, offset, &[byte]);
            }
            MIRTykind::CHAR8 => {
                let byte = self.expect_char8(val);
                self.write_bytes(alloc_id, offset, &[byte]);
            }
            MIRTykind::CHAR16 => {
                let bytes = self.expect_char16(val).to_le_bytes();
                self.write_bytes(alloc_id, offset, &bytes);
            }
            MIRTykind::CHAR32 => {
                let bytes = self.expect_char32(val).to_le_bytes();
                self.write_bytes(alloc_id, offset, &bytes);
            }
            MIRTykind::Ptr => {
                let (target_alloc_id, ptr_offset) = match val {
                    VMValue::Ptr(target_id, offset_val) => (Some(*target_id), *offset_val),
                    VMValue::UInt(addr) => (None, *addr),
                    _ => {
                        self.report_ice(format!(
                            "Expected Ptr or UInt VMValue for pointer write, found {:?}",
                            val
                        ));
                        (None, 0)
                    }
                };

                // Write the raw 8-byte offset into memory
                let bytes = ptr_offset.to_le_bytes();
                self.write_bytes(alloc_id, offset, &bytes);

                // Record or clear the relocation at this offset
                if let Some(alloc) = self.memory.allocations.get_mut(alloc_id) {
                    if let Some(target_id) = target_alloc_id {
                        alloc.relocations.insert(offset, target_id);
                    } else {
                        alloc.relocations.remove(&offset);
                    }
                }
            }
            MIRTykind::Array(elem_ty, count) => {
                let VMValue::Array(elems) = val else {
                    self.report_ice(
                        "Expected array VMValue when writing array-typed data".to_string(),
                    );
                    return;
                };
                for i in 0..*count {
                    self.write_typed(alloc_id, offset + i * elem_ty.size, &elems[i], elem_ty);
                }
            }
            MIRTykind::Struct(_, _, fields) => {
                let VMValue::Struct {
                    fields: field_vals, ..
                } = val
                else {
                    self.report_ice(
                        "Expected struct VMValue when writing struct-typed data".to_string(),
                    );
                    return;
                };
                let mut field_offset = 0usize;
                for ((_, field_ty), field_val) in fields.iter().zip(field_vals.iter()) {
                    let padding = if field_ty.align == 0 {
                        0
                    } else {
                        (field_ty.align - (field_offset % field_ty.align)) % field_ty.align
                    };
                    field_offset += padding;
                    self.write_typed(alloc_id, offset + field_offset, field_val, field_ty);
                    field_offset += field_ty.size;
                }
            }
            MIRTykind::Unit => {}
        }
    }

    fn expect_i8(&mut self, val: &VMValue) -> i8 {
        match val {
            VMValue::I8(v) => *v,
            _ => {
                self.report_ice(format!("Expected I8 VMValue, found {:?}", val));
                0
            }
        }
    }

    fn expect_u8(&mut self, val: &VMValue) -> u8 {
        match val {
            VMValue::U8(v) => *v,
            _ => {
                self.report_ice(format!("Expected U8 VMValue, found {:?}", val));
                0
            }
        }
    }

    fn expect_i16(&mut self, val: &VMValue) -> i16 {
        match val {
            VMValue::I16(v) => *v,
            _ => {
                self.report_ice(format!("Expected I16 VMValue, found {:?}", val));
                0
            }
        }
    }

    fn expect_u16(&mut self, val: &VMValue) -> u16 {
        match val {
            VMValue::U16(v) => *v,
            _ => {
                self.report_ice(format!("Expected U16 VMValue, found {:?}", val));
                0
            }
        }
    }

    fn expect_i32(&mut self, val: &VMValue) -> i32 {
        match val {
            VMValue::I32(v) => *v,
            _ => {
                self.report_ice(format!("Expected I32 VMValue, found {:?}", val));
                0
            }
        }
    }

    fn expect_u32(&mut self, val: &VMValue) -> u32 {
        match val {
            VMValue::U32(v) => *v,
            _ => {
                self.report_ice(format!("Expected U32 VMValue, found {:?}", val));
                0
            }
        }
    }

    fn expect_i64(&mut self, val: &VMValue) -> i64 {
        match val {
            VMValue::I64(v) => *v,
            _ => {
                self.report_ice(format!("Expected I64 VMValue, found {:?}", val));
                0
            }
        }
    }

    fn expect_u64(&mut self, val: &VMValue) -> u64 {
        match val {
            VMValue::U64(v) => *v,
            _ => {
                self.report_ice(format!("Expected U64 VMValue, found {:?}", val));
                0
            }
        }
    }

    fn expect_i128(&mut self, val: &VMValue) -> i128 {
        match val {
            VMValue::I128(v) => *v,
            _ => {
                self.report_ice(format!("Expected I128 VMValue, found {:?}", val));
                0
            }
        }
    }

    fn expect_u128(&mut self, val: &VMValue) -> u128 {
        match val {
            VMValue::U128(v) => *v,
            _ => {
                self.report_ice(format!("Expected U128 VMValue, found {:?}", val));
                0
            }
        }
    }

    fn expect_isize(&mut self, val: &VMValue) -> isize {
        match val {
            VMValue::Int(v) => *v,
            _ => {
                self.report_ice(format!("Expected ISIZE VMValue, found {:?}", val));
                0
            }
        }
    }

    fn expect_usize(&mut self, val: &VMValue) -> usize {
        match val {
            VMValue::UInt(v) => *v,
            _ => {
                self.report_ice(format!("Expected USIZE VMValue, found {:?}", val));
                0
            }
        }
    }

    fn expect_f32(&mut self, val: &VMValue) -> f32 {
        match val {
            VMValue::F32(v) => *v,
            _ => {
                self.report_ice(format!("Expected F32 VMValue, found {:?}", val));
                0.0
            }
        }
    }

    fn expect_f64(&mut self, val: &VMValue) -> f64 {
        match val {
            VMValue::F64(v) => *v,
            _ => {
                self.report_ice(format!("Expected F64 VMValue, found {:?}", val));
                0.0
            }
        }
    }

    fn expect_bool(&mut self, val: &VMValue) -> bool {
        match val {
            VMValue::Bool(v) => *v,
            _ => {
                self.report_ice(format!("Expected Bool VMValue, found {:?}", val));
                false
            }
        }
    }

    fn expect_char8(&mut self, val: &VMValue) -> u8 {
        match val {
            VMValue::Char8(v) => *v,
            _ => {
                self.report_ice(format!("Expected Char8 VMValue, found {:?}", val));
                0
            }
        }
    }

    fn expect_char16(&mut self, val: &VMValue) -> u16 {
        match val {
            VMValue::Char16(v) => *v,
            _ => {
                self.report_ice(format!("Expected Char16 VMValue, found {:?}", val));
                0
            }
        }
    }

    fn expect_char32(&mut self, val: &VMValue) -> u32 {
        match val {
            VMValue::Char32(v) => *v,
            _ => {
                self.report_ice(format!("Expected Char32 VMValue, found {:?}", val));
                0
            }
        }
    }

    fn expect_ptr_addr(&mut self, val: &VMValue) -> usize {
        match val {
            VMValue::Ptr(_, offset) => *offset,
            VMValue::UInt(addr) => *addr,
            _ => {
                self.report_ice(format!("Expected Ptr VMValue, found {:?}", val));
                0
            }
        }
    }
}
