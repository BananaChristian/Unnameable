use crate::{
    diagnostics::{CompilerError, Phase, SharedDiagnostics, Span},
    mir::{MIRDollarMode, MIRFn, MIRInstruction, MIRModule, MIRValue},
};

pub struct DollarVerifier<'a> {
    mir_module: &'a MIRModule,
    pub diagnostics: SharedDiagnostics,
    pub corrupted: bool,
}

impl<'a> DollarVerifier<'a> {
    pub fn new(mir_module: &'a MIRModule, diagnostics: SharedDiagnostics) -> Self {
        DollarVerifier {
            mir_module,
            diagnostics,
            corrupted: false,
        }
    }

    ///Main dollar verifier driver
    pub fn verify(&mut self) {
        let mut sorted_fns: Vec<_> = self.mir_module.functions.values().collect();
        sorted_fns.sort_by_key(|f| f.fn_id);
        for func in sorted_fns {
            if func.dollar_mode == MIRDollarMode::None {
                continue;
            }
            self.verify_func(func);
        }
    }

    fn verify_func(&mut self, mir_fn: &MIRFn) {
        if let Some(body) = &mir_fn.body {
            let mut sorted_bbs: Vec<_> = body.blocks.values().collect();
            sorted_bbs.sort_by_key(|b| b.id);
            for block in sorted_bbs {
                for inst in &block.instructions {
                    self.verify_instruction(inst, mir_fn);
                }
            }
        }
    }

    ///This scrutinizes that every instruction it handles follows dollar rules(The assumption being
    ///that every instruction here is under full dollar jurisdication)
    fn verify_instruction(&mut self, instruction: &MIRInstruction, calling_func: &MIRFn) {
        match instruction {
            MIRInstruction::Call { callee, .. } => self.check_call(callee, calling_func),
            _ => (),
        }
    }

    fn check_call(&mut self, callee: &MIRValue, calling_func: &MIRFn) {
        match callee {
            MIRValue::FunctionRef(fn_id) => {
                let Some(target_func) = self.mir_module.functions.get(fn_id) else {
                    return; //This is an error but the MIR builder must have already resolved such
                    //crap
                };
                if target_func.dollar_mode == MIRDollarMode::None {
                    self.report(
                        format!(
                            "Cannot call a non dollar function '{}' from a dollar scope ",
                            target_func.name
                        ),
                        None,
                    );
                }
            }
            MIRValue::Register { .. } => {
                self.report(format!(
                    "Indirect (function-pointer) calls inside dollar scope  are not yet verifiable and not currently allowed"
                ),None);
            }
            _ => {
                self.report_ice(
                    format!(
                        "Unexpected callee value shape in dollar scope '{}': {:?}",
                        calling_func.name, callee
                    ),
                    None,
                );
            }
        }
    }

    fn report(&mut self, message: String, span: Option<Span>) {
        self.corrupted = true;
        self.diagnostics.borrow_mut().report(CompilerError::error(
            message,
            Phase::MIRBuilder,
            span,
        ));
    }

    fn report_ice(&mut self, message: String, span: Option<Span>) {
        self.corrupted = true;
        self.diagnostics
            .borrow_mut()
            .report_ice_and_panic(CompilerError::ice(message, Phase::MIRBuilder, span));
    }
}
