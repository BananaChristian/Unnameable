#include "ast.hpp"
#include "audit.hpp"

void Auditor::auditExpressionStatement(Node* node) {
    auto exprStmt = dynamic_cast<ExpressionStatement*>(node);
    audit(exprStmt->expression.get());
}

void Auditor::auditVariableDeclaration(Node* node) {
    auto declaration = dynamic_cast<VariableDeclaration*>(node);
    if (!declaration) return;

    auto sym = semantics.getSymbolFromMeta(declaration);

    if (!sym) return;

    if (!sym->storage().isHeap) return;

    simulateDeclFree(declaration, sym->codegen().ID);
}

void Auditor::auditFieldAssignmentStatement(Node* node) {
    auto fieldAssign = dynamic_cast<FieldAssignment*>(node);
    if (!fieldAssign) return;

    auto sym = semantics.getSymbolFromMeta(fieldAssign);
    if (!sym) reportDevBug("Failed to get field assignment symbol info", fieldAssign);

    auto valSym = semantics.getSymbolFromMeta(fieldAssign->value.get());
    if (!valSym) reportDevBug("Failed to get field assignment value symbol info", fieldAssign);

    inhibit = true;
    audit(fieldAssign->lhs_chain.get());
    audit(fieldAssign->value.get());

    assignmentRob(fieldAssign, sym);
    inhibit = false;
    simulateFree(fieldAssign, sym->codegen().ID);
}

void Auditor::auditAssignmentStatement(Node* node) {
    auto assignStmt = dynamic_cast<AssignmentStatement*>(node);
    if (!assignStmt) return;

    auto assignSym = semantics.getSymbolFromMeta(assignStmt);
    if (!assignSym) reportDevBug("Failed to get assignment symbol info", assignStmt);
    if (!assignSym->storage().isHeap) return;

    auto valSym = semantics.getSymbolFromMeta(assignStmt->value.get());
    if (!valSym) reportDevBug("Failed to get assignment value symbol info", assignStmt);

    inhibit = true;
    audit(assignStmt->identifier.get());
    audit(assignStmt->value.get());
    assignmentRob(assignStmt, assignSym);
    inhibit = false;
    simulateFree(assignStmt, assignSym->codegen().ID);
}

void Auditor::auditFunctionStatement(Node* node) {
    auto funcStmt = dynamic_cast<FunctionStatement*>(node);
    if (!funcStmt) return;

    audit(funcStmt->funcExpr.get());
}

void Auditor::auditFunctionExpression(Node* node) {
    auto funcExpr = dynamic_cast<FunctionExpression*>(node);
    if (!funcExpr) return;

    audit(funcExpr->block.get());
}

void Auditor::auditForStatement(Node* node) {
    auto forStmt = dynamic_cast<ForStatement*>(node);
    if (!forStmt) return;

    if (shouldForeignBunkerBlock(forStmt->body.get())) {
        logInternal("[TRIGGER] For Loop with foreigners - bunkering");
        bunkerForeigners(forStmt->body.get());
    }

    audit(forStmt->body.get());
}

void Auditor::auditWhileStatement(Node* node) {
    auto whileStmt = dynamic_cast<WhileStatement*>(node);
    if (!whileStmt) return;

    if (shouldForeignBunkerBlock(whileStmt->loop.get())) {
        logInternal("[TRIGGER] Loop with foreigners - bunkering");
        bunkerForeigners(whileStmt->loop.get());
    }

    audit(whileStmt->loop.get());
}

void Auditor::auditCaseStatement(Node* node) {
    auto caseClause = dynamic_cast<CaseClause*>(node);
    if (!caseClause) return;

    for (auto& stmt : caseClause->body) {
        audit(stmt.get());
    }
}

void Auditor::auditSwitchStatement(Node* node) {
    auto switchStmt = dynamic_cast<SwitchStatement*>(node);
    if (!switchStmt) return;

    if (deferedFrees.find(switchStmt) == deferedFrees.end()) {
        deferedFrees[switchStmt] = std::make_unique<BlockInfo>();
    }

    if (shouldForeignBunkerBlock(switchStmt)) {
        logInternal("[TRIGGER] Switch with foreigners - bunkering");
        bunkerForeigners(switchStmt);
    }

    // Now audit everything normally
    for (auto& caseClause : switchStmt->case_clauses) {
        audit(caseClause.get());
    }
    for (auto& stmt : switchStmt->default_statements) {
        audit(stmt.get());
    }
}

void Auditor::auditElifStatement(Node* node) {
    auto elifStmt = dynamic_cast<elifStatement*>(node);
    if (!elifStmt) return;

    audit(elifStmt->elif_result.get());
}

void Auditor::auditIfStatement(Node* node) {
    auto ifStmt = dynamic_cast<ifStatement*>(node);
    if (!ifStmt) return;

    // Check then block
    if (shouldForeignBunkerBlock(ifStmt->if_result.get())) {
        logInternal("[TRIGGER] Then block has foreigners - bunkering");
        bunkerForeigners(ifStmt->if_result.get());
    }

    // Check elif blocks
    for (auto& elif : ifStmt->elifClauses) {
        auto elifStmt = dynamic_cast<elifStatement*>(elif.get());
        if (shouldForeignBunkerBlock(elifStmt->elif_result.get())) {
            logInternal("[TRIGGER] Elif block has foreigners - bunkering");
            bunkerForeigners(elifStmt->elif_result.get());
        }
    }

    // Check else block (if it exists)
    if (ifStmt->else_result.has_value()) {
        if (shouldForeignBunkerBlock(ifStmt->else_result.value().get())) {
            logInternal("[TRIGGER] Else block has foreigners - bunkering");
            bunkerForeigners(ifStmt->else_result.value().get());
        }
    }

    // Audit all blocks normally
    audit(ifStmt->if_result.get());
    for (auto& elif : ifStmt->elifClauses) {
        audit(elif.get());
    }
    if (ifStmt->else_result.has_value()) {
        audit(ifStmt->else_result.value().get());
    }
}

void Auditor::auditReturnStatement(Node* node) {
    auto retStmt = dynamic_cast<ReturnStatement*>(node);
    if (!retStmt) return;

    auto retSym = semantics.getSymbolFromMeta(retStmt);
    if (!retSym->storage().isHeap) return;

    logInternal("Return statement ID: " + retSym->codegen().ID);

    const auto& valName = semantics.extractIdentifierName(retStmt->return_value.get());

    Node* batonHolder = semantics.queryForLifeTimeBaton(retSym->codegen().ID);
    if (!batonHolder) {
        logInternal("Failed to get baton holder");
        return;
    }
    auto identifiers = semantics.digIdentifiers(retStmt);
    for (const auto& identifier : identifiers) {
        // If the return statement isnt the baton holder this is an escape
        if (batonHolder != identifier) {
            logInternal("The baton escaped");
            auto& baton = semantics.responsibilityTable[batonHolder];
            if (!baton) {
                logInternal("Failed to get baton yet holder exists");
                return;
            }

            logInternal("The baton has '" + std::to_string(baton->dependents.size()) +
                        "' dependents");
            if (baton && !baton->dependents.empty()) {
                logInternal("The dependents exist attempting to block this return");
                errorHandler.addHint(
                    "The compiler cannot allow the escape of a variable that is "
                    "extending the lifetime of other variables as if it frees them and "
                    "allows your pointer to escape then you will have a dangling "
                    "pointer");
                logAuditError(
                    "The return value '" + valName + "' cannot escape with hidden dependecies",
                    retStmt->return_value.get());
            }
        }
    }

    simulateFree(retStmt, retSym->codegen().ID);
}

void Auditor::auditInfix(Node* node) {
    auto infix = dynamic_cast<InfixExpression*>(node);
    if (!infix) return;

    auto infixSym = semantics.getSymbolFromMeta(infix);
    if (!infixSym) 
        reportDevBug("Failed to get symbol info for infix", infix);
    
    inhibit = true;
    audit(infix->left_operand.get());
    audit(infix->right_operand.get());
    inhibit = false;
    simulateFree(infix, infixSym->codegen().ID);
}

void Auditor::auditIdentifier(Node* node) {
    auto ident = dynamic_cast<Identifier*>(node);
    if (!ident) return;

    auto identSym = semantics.getSymbolFromMeta(ident);
    if(!identSym)
        reportDevBug("Failed to get identifier symbol info",ident);
    
    simulateFree(ident, identSym->codegen().ID);
}

void Auditor::auditTraceStatement(Node* node) {
    auto traceStmt = dynamic_cast<TraceStatement*>(node);
    if (!traceStmt) return;

    inhibit = true; 

    for(const auto &expr : traceStmt->arguments) {
        audit(expr.get()); 
    }

    inhibit = false;

    for(const auto &expr : traceStmt->arguments) {
        auto argSym = semantics.getSymbolFromMeta(expr.get());
        if(argSym) {
            simulateFree(expr.get(), argSym->codegen().ID);
        }
    }
}

void Auditor::auditFStringLiteral(Node* node) {
    auto fstr = dynamic_cast<FStringLiteral*>(node);
    
    for (auto &seg : fstr->segments) {
        for (auto &valExpr : seg.values) {
            audit(valExpr.get()); 
        }
    }
}

void Auditor::auditAddressExpression(Node* node) {
    auto addrExpr = dynamic_cast<AddressExpression*>(node);
    if (!addrExpr) return;

    auto addrSym = semantics.getSymbolFromMeta(addrExpr);
    if(!addrSym)
        reportDevBug("Failed to get address expression symbol info",addrExpr);
    
    inhibit = true;
    audit(addrExpr->identifier.get());
    inhibit = false;
    simulateFree(addrExpr, addrSym->codegen().ID);
}

void Auditor::auditArraySubscriptExpression(Node *node){
    auto arrSub=dynamic_cast<ArraySubscript*>(node);
    if(!arrSub) return;
    
    auto arrSubSym=semantics.getSymbolFromMeta(arrSub);
    if(!arrSubSym)
        reportDevBug("Failed to get array subscript symbol info",arrSub);
    
    inhibit=true;
    audit(arrSub->identifier.get());
    for(const auto &len:arrSub->index_exprs)
        audit(len.get());
        
    inhibit=false;
    simulateFree(arrSub,arrSubSym->codegen().ID);
}

void Auditor::auditDereferenceExpression(Node* node) {
    auto derefExpr = dynamic_cast<DereferenceExpression*>(node);
    if (!derefExpr) return;

    auto derefSym = semantics.getSymbolFromMeta(derefExpr);
    if(!derefSym)
        reportDevBug("Failed to get dereference expression symbol info",derefExpr);
    
    inhibit = true;
    audit(derefExpr->identifier.get());
    inhibit = false;
    
    simulateFree(derefExpr, derefSym->codegen().ID);
}

void Auditor::auditCallExpression(Node *node){
    auto callExpr=dynamic_cast<CallExpression*>(node);
    if(!callExpr)
        return;

    auto callSym=semantics.getSymbolFromMeta(callExpr);
    if(!callSym)
        reportDevBug("Failed to get call expression symbol info",callExpr);

    inhibit=true;
    std::vector<Node *> tofree;
    for(const auto &arg:callExpr->parameters){
        audit(arg.get());
        tofree.push_back(arg.get());
    }
    inhibit=false;
    
    if(callSym->type().isPointer){
        if(callSym->storage().isHeap)
            simulateFree(callExpr,callSym->codegen().ID);
    }else{
        for(const auto &arg:tofree){
            auto argSym=semantics.getSymbolFromMeta(arg);
            if(!argSym)
                reportDevBug("Failed to get argument symbol info ",arg);
            simulateFree(arg,argSym->codegen().ID);
        }
    }
}

void Auditor::auditBlockStatement(Node* node) {
    auto blockStmt = dynamic_cast<BlockStatement*>(node);
    if (!blockStmt) return;

    activeBlocks.push_back(blockStmt);
    if (shouldNativeBunkerBlock(blockStmt)) {
        bunkerPersists(blockStmt);
        if (hasDisruptors(blockStmt)) {
            logInternal("[TRIGGER] Block disruptor detected bunkering natives for block: " +
                        blockStmt->toString());
            bunkerNatives(blockStmt);
        }
    }

    for (const auto& stmt : blockStmt->statements) {
        audit(stmt.get());
    }
    activeBlocks.pop_back();
}

void Auditor::auditBlockExpression(Node* node) {
    auto blockExpr = dynamic_cast<BlockExpression*>(node);
    if (!blockExpr) return;

    activeBlocks.push_back(blockExpr);
    buildUsageMap(blockExpr);
    int n=blockExpr->statements.size();

    for (int i =0 ;i<n;++i) {
        currentStmtIdx=i;
        auto &stmt=blockExpr->statements[i];
        audit(stmt.get());
    }

    if (blockExpr->finalexpr.has_value()) {
        currentStmtIdx=n;
        audit(blockExpr->finalexpr.value().get());
    }

    if (shouldNativeBunkerBlock(blockExpr)) {
        bunkerPersists(blockExpr);
        bunkerNativeHeists(blockExpr);
    }
    if(shouldForeignBunkerBlock(blockExpr))
        bunkerForeignHeists(blockExpr);

    activeBlocks.pop_back();
}

void Auditor::auditASMStatement(Node *node){
    auto asmStmt=dynamic_cast<ASMStatement*>(node);
    if(!asmStmt)
        return;

    std::vector<Expression*> tofree;
    inhibit=true;
    for(const auto &instruction:asmStmt->instructions){
        auto asmInst=dynamic_cast<ASMInstruction*>(instruction.get());
        if(!asmInst)
            reportDevBug("Invalid assembly instruction",instruction.get());
        
        for(const auto &constraint:asmInst->constraints){
            audit(constraint->variable.get());
            tofree.push_back(constraint->variable.get());
        }
            
    }
    inhibit=false;
    for(const auto &node:tofree){
        auto sym=semantics.getSymbolFromMeta(node);
        if(!sym)
            reportDevBug("Failed to get constraint symbol info",node);

        simulateFree(node,sym->codegen().ID);
    }
}
