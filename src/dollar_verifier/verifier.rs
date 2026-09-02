use crate::{
    diagnostics::{CompilerError, Phase, SharedDiagnostics},
    mir::{MIRDollarMode, MIRFn, MIRInstruction, MIRModule},
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

    pub fn verify(&self) {
        let mut sorted_fns: Vec<_> = self.mir_module.functions.values().collect();
        sorted_fns.sort_by_key(|f| f.fn_id);
        for func in sorted_fns {
            if func.dollar_mode == MIRDollarMode::None {
                continue;
            }
            self.verify_func(func);
        }
    }

    fn verify_func(&self, mir_fn: &MIRFn) {
        if let Some(body) = &mir_fn.body {
            let mut sorted_bbs: Vec<_> = body.blocks.values().collect();
            sorted_bbs.sort_by_key(|b| b.id);
            for block in sorted_bbs {
                for inst in &block.instructions {
                    self.verify_instruction(inst);
                }
            }
        }
    }

    ///This scrutinizes that every instruction it handles follows dollar rules(The assumption being
    ///that every instruction here is under full dollar jurisdication)
    fn verify_instruction(&self, instruction: &MIRInstruction) {
        match instruction {
            MIRInstruction::Call {
                dest,
                callee,
                args,
                sig,
            } => (),
            _ => (),
        }
    }

    fn report(&mut self, message: String) {
        self.corrupted = true;
        self.diagnostics.borrow_mut().report(CompilerError::error(
            message,
            Phase::MIRBuilder,
            None,
        ));
    }
}
